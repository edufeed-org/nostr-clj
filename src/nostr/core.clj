(ns nostr.core
  (:require [hato.websocket :as ws]
            [cheshire.core :as json]
            [clojure.core.async :refer [chan put! go-loop <!]])
  (:import [java.nio CharBuffer]))

(def subscriptions (atom {}))
(def ws-uri-map (atom {}))

(defn add-to-subs [uri subscription]
  (swap! subscriptions
         (fn [current-map]
           (update current-map uri (fn [existing-set]
                                     (conj (or existing-set #{}) subscription))))))

(defn remove-sub [uri subscription]
  (swap! subscriptions
         (fn [current-map]
           (update current-map uri (fn [existing-set]
                                     (filter #(not= subscription %) existing-set))))))

(defn send! [ws msg]
  (ws/send! ws (json/generate-string msg)))

(defn subscribe [ws event-filter]
  (let [sub-id (str (random-uuid))
        msg ["REQ" sub-id event-filter]]
    (add-to-subs (get @ws-uri-map ws) msg)
    (send! ws msg)))

(defn resubscribe [ws subs]
  (for [sub subs]
    (subscribe ws sub)))

(defn unsubscribe [ws event-filter]
  (let [ws-uri (get @ws-uri-map ws)
        sub (first (filter #(= event-filter (nth % 2)) (get @subscriptions ws-uri)))]
    (send! ws ["CLOSE" (second sub)])
    (remove-sub ws-uri sub)))

(defn close! [ws]
  (ws/close! ws))

(defn connect
  "Establishes a websocket connection and handles events using user-defined handlers.
  
  Args:
  - `uri`: The websocket URI to connect to.

  Returns:
  - The websocket instance and a channel"
  [uri]
  (let [c (chan)
        reconnect (fn reconnect [status]
                    (try
                      (Thread/sleep 3000)
                      (connect uri)
                      (resubscribe uri (get @subscriptions uri))
                      (catch Exception e
                        (println "Reconnect failed for " uri ":" (.getMessage e))
                        (reconnect status))))
        ws @(ws/websocket uri
                          {:on-message (fn [ws msg last?]
                                         (when last?
                                           (let [msg-str
                                                 (if (instance? CharBuffer msg)
                                                   (str msg)
                                                   msg)
                                                 parsed (json/parse-string msg-str true)]
                                             (when (contains? #{"EOSE" "EVENT"} (first parsed))
                                               (put! c parsed)))))
                           :on-open (fn [ws]
                                      (println "connection open: "  ws))
                           :on-close (fn [ws status reason]
                                       (println "connection closed: " ws status reason)
                                       (when (= 1006 status)
                                         (reconnect status)))})]
    (swap! ws-uri-map assoc ws uri)
    {:ws ws
     :channel c}))

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
        {:keys [ws channel]} (connect uri)]
    (subscribe ws filter)
    (go-loop []
      (when-some [message (<! channel)]
        (case (first message)
          "EVENT" (let [event (nth message 2 nil)]
                    (swap! resources conj event))
          "EOSE"  (do
                    (close! ws)
                    (deliver result-promise @resources)))
        (recur)))
    @result-promise))

