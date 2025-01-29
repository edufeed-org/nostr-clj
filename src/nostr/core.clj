(ns nostr.core
  (:require [hato.websocket :as ws]
            [cheshire.core :as json]
            [clojure.core.async :refer [chan put! go-loop <! close!]])
  (:import [java.nio CharBuffer]))

(defn generate-sub-id []
  (str "sub-" (subs (str (java.util.UUID/randomUUID)) 0 6)))

(defn connect
  "Establishes connections to multiple Nostr relays and provides an API for managing relays, 
  subscriptions, and event handlers. Messages from relays are forwarded to a shared `core.async` 
  channel, and event handlers can be registered for specific event types.

  Args:
  - `relay-uris`: A list of WebSocket URIs to establish connections with.

  Returns:
  A map containing functions for interacting with the connected relays:
  
  - `:channel`          → A shared `core.async` channel where all incoming events are published.
                          Each message is a map with `:relay` (relay URI) and `:event` (Nostr event).

  - `:add-relay`        → `(add-relay relay-uri)`  
                          Connects to a new relay dynamically and starts listening for events.

  - `:remove-relay`     → `(remove-relay relay-uri)`  
                          Disconnects from a relay and cancels all active subscriptions.

  - `:subscribe`        → `(subscribe relay-uri filters)`  
                          Subscribes to events using Nostr filters. If `relay-uri` is `nil`, 
                          subscribes to all connected relays. The library manages subscription 
                          IDs internally.

  - `:unsubscribe`      → `(unsubscribe relay-uri filters)`  
                          Unsubscribes from events using the same filters used during subscription. 
                          If `relay-uri` is `nil`, removes the subscription from all relays.

  - `:register-handler` → `(register-handler event-type handler-fn)`  
                          Registers a handler function for a specific event type (`kind`). Multiple 
                          handlers can be registered for the same type.

  - `:remove-handler`   → `(remove-handler event-type)`  
                          Removes all handlers for a specific event type.
                          `(remove-handler event-type handler-fn)` removes a specific handler.

  - `:list-handlers`    → `(list-handlers)`  
                          Returns a list of registered event types that have handlers.

  - `:close-all`        → `(close-all)`  
                          Closes all relay connections, cancels all subscriptions, 
                          and cleans up internal resources.

  Example usage:
  
  ```
  (let [{:keys [channel 
                add-relay 
                remove-relay 
                subscribe 
                unsubscribe 
                register-handler 
                remove-handler 
                list-handlers 
                close-all]} (connect-relays [\"wss://relay1.example.com\" \"wss://relay2.example.com\"])]

    ;; Register a handler for text events (kind 1)
    (register-handler 1 (fn [event] (println \"Text event received:\" event)))

    ;; Subscribe to all relays with event filter
    (subscribe nil {:kinds [1]})

    ;; Consume messages from the shared channel
    (go-loop []
      (when-let [msg (<! channel)]
        (println \"Received event:\" msg)
        (recur)))

    ;; Unsubscribe from all relays
    (unsubscribe nil {:kinds [1]})

    ;; Add and remove relays dynamically
    (add-relay \"wss://relay3.example.com\")
    (remove-relay \"wss://relay1.example.com\")

    ;; Close all connections and clean up
    (close-all))
  ```"
  [relay-uris]
  (let [shared-channel (chan)
        connections (atom {})
        handlers (atom {})]

    (defn handle-message
      "Processes an incoming message, routes it to all handlers for the event type,
     and forwards the event to the shared channel."
      [uri msg]
      (let [event (nth msg 2)
            event-type (get event :kind) 
            event-handlers (get @handlers event-type [])]
        (put! shared-channel {:relay uri :event event})

        (if (seq event-handlers)
          (doseq [handler event-handlers]
            (try
              (handler {:relay uri :event event})
              (catch Exception e
                (println "Error in handler for event type" event-type ":" (.getMessage e)))))
          (println "No handlers for event type:" event-type))))

    (defn subscribe
      "Subscribes to one or all relays with the given filters. Users no longer provide a subscription ID."
      [uri filters]
      (if uri
        (if-let [{:keys [ws subscriptions]} (get @connections uri nil)]
          (when ws
            (let [sub-id (generate-sub-id)
                  req-msg (json/generate-string ["REQ" sub-id filters])]
              (println "Sending subscription to" uri ":" req-msg)
              (ws/send! ws req-msg)
              (swap! connections update-in [uri :subscriptions] conj sub-id)
              (swap! connections update-in [uri :subscription-filters] assoc sub-id filters)
              sub-id)) ;; Return generated sub-id in case it's needed internally
          (println "Relay not found:" uri))

        ;; Subscribe to all relays
        (doseq [[relay-uri {:keys [ws]}] @connections]
          (subscribe relay-uri filters))))

    (defn unsubscribe
      "Unsubscribes from one or all relays based on the filters used in the subscription.
      If `uri` is nil, unsubscribes from all relays."
      [uri filters]
      (if uri
        (if-let [{:keys [ws subscriptions subscription-filters]} (get @connections uri)]
          (let [matching-subs (filter #(= (subscription-filters %) filters) subscriptions)]
            (doseq [sub-id matching-subs]
              (let [close-msg (json/generate-string ["CLOSE" sub-id])]
                (println "Cancelling subscription on" uri ":" close-msg)
                (ws/send! ws close-msg)
                (swap! connections update-in [uri :subscriptions] disj sub-id)
                (swap! connections update-in [uri :subscription-filters] dissoc sub-id))))
          (println "Relay not found:" uri))

        ;; Unsubscribe from all relays
        (doseq [[relay-uri {:keys [ws]}] @connections]
          (unsubscribe relay-uri filters))))

    (defn resubscribe [uri]
      (let [existing-subs (get-in @connections [uri :subscription-filters] {})]
        (if (seq existing-subs)
          (do
            (println "Re-subscribing to existing filters on" uri)
            (doseq [[sub-id filters] existing-subs]
              (subscribe uri filters)))
          (println "No existing subscriptions to re-subscribe on" uri))))

    (defn connect-relay [uri]
      (let [reconnect-attempts (atom 0)
            message-buffer (atom "")
            reconnect (fn reconnect []
                        (try
                          (when (< @reconnect-attempts 5)
                            (Thread/sleep 5000)
                            (swap! reconnect-attempts inc)
                            (println "Reconnecting to " uri " (attempt " @reconnect-attempts ")")
                            (connect-relay uri))
                          (catch Exception e
                            (println "Reconnect failed for " uri ":" (.getMessage e))
                            (reconnect))))
            ws (try @(ws/websocket uri
                                   ;; we need to make sure msg is complete
                                   {:on-message (fn [_ msg last?]
                                                  (let [current-msg (if (instance? CharBuffer msg)
                                                                      (str msg)
                                                                      msg)]
                                                    (swap! message-buffer str current-msg)
                                                    (when last?
                                                      (try
                                                        (let [complete-msg @message-buffer
                                                              parsed (json/parse-string complete-msg true)]
                                                          (when (contains? #{"EOSE" "EVENT"} (first parsed))
                                                            (handle-message uri parsed)))
                                                        (catch Exception e
                                                          (println "Error processing message from" uri ":"
                                                                   (.getMessage e)
                                                                   "\nMessage:" @message-buffer))
                                                        (finally
                                                          (reset! message-buffer ""))))))

                                    :on-open  (fn [ws]
                                                (println "Connection open:" uri)
                                                (swap! connections update uri assoc :ws ws)
                                                (resubscribe uri))
                                    :on-close (fn [_ status reason]
                                                (println "Connection closed:" uri status reason)
                                                (when (= 1006 status)
                                                  (reconnect)))})
                    (catch Exception  e
                      (println "Error creating websocket" e)))]
        (swap! connections assoc uri {:ws ws :subscriptions #{}})
        {:ws ws}))

    ;; Connect to all initial relays
    (doseq [uri relay-uris]
      (connect-relay uri))

    {:channel shared-channel
     :add-relay (fn [uri]
                  (if (contains? @connections uri)
                    (println "Relay already connected:" uri)
                    (connect-relay uri)))
     :remove-relay (fn [uri]
                     (if-let [{:keys [ws subscriptions]} (get @connections uri)]
                       (do
                         ;; Cancel all subscriptions before closing
                         (doseq [sub-id subscriptions]
                           (unsubscribe uri sub-id))
                         (ws/close! ws)
                         (swap! connections dissoc uri)
                         (println "Relay removed:" uri))
                       (println "Relay not found:" uri)))
     :close-all (fn []
                  (doseq [{:keys [ws subscriptions]} (vals @connections)]
                ;; Cancel all subscriptions for all relays
                    (doseq [sub-id subscriptions]
                      (unsubscribe nil sub-id))
                    (ws/close! ws))
                  (reset! connections {})
                  (close! shared-channel)
                  (println "All relays disconnected and resources cleaned up"))
     :subscribe (fn [uri filters]
                  (subscribe uri filters))
     :unsubscribe (fn [uri filters]
                    (unsubscribe uri filters))
     :register-handler (fn [event-type handler-fn]
                         (swap! handlers update event-type (fnil conj []) handler-fn))
     :remove-handler (fn
                       ([event-type] (swap! handlers dissoc event-type))
                       ([event-type handler-fn]
                        (swap! handlers update event-type
                               (fn [handlers]
                                 (vec (remove #(= % handler-fn) handlers))))))
     :list-handlers (fn [] (keys @handlers))}))

(comment
  (let [relays ["wss://relay-hed.edufeed.org" "wss://relay-k12.edufeed.org"  "wss://relay-edu.edufeed.org"]
        {:keys [channel
                add-relay
                close-all
                subscribe
                register-handler]} (connect relays)]
    (go-loop []
      (when-let [msg (<! channel)]
        (println "Received message from channel:" msg)
        (recur)))

    (add-relay "ws://localhost:10547")
    (subscribe "ws://localhost:10547" {:kinds [1]})
    (subscribe "wss://relay-edu.edufeed.org" {:kinds [1]})
    (subscribe "wss://relay-hed.edufeed.org" {:kinds [30142]})
    (subscribe "wss://relay-k12.edufeed.org" {:kinds [1]})

    (register-handler 30142 (fn [event]
                              (println "MD event:" event)))

    @(promise)
    (println "done")
    (close-all)))

(defn fetch-events
  "Args: 
 - `uri`: URI of relay to connect to
 - `filter`: Filter for query.

 Returns:
 A vector of found events.

  Example: 
  `(fetch-events 'wss://my-relay.org' {:kinds [30142]})`
 "
  [uri filter]
  (let [resources (atom [])
        result-promise (promise)
        {:keys [ws channel]} (connect-relay uri)]
    (subscribe ws filter)
    (go-loop []
      (when-some [message (<! channel)]
        (case (first message)
          "EVENT" (let [event (nth message 2 nil)]
                    (swap! resources conj event))
          "EOSE"  (do
                    (ws/close! ws)
                    (deliver result-promise @resources)))
        (recur)))
    @result-promise))

