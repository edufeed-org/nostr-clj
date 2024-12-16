(ns nostr.core
  (:require [hato.websocket :as ws]
            [cheshire.core :as json])
  (:import [java.nio CharBuffer]))

(defn connect
  "Establishes a websocket connection and handles events using user-defined handlers.
  
  Args:
  - `uri`: The websocket URI to connect to.
  - `handlers`: A map containing the following keys:
      - `:on-open-handler`: (fn [ws] ...) called when the connection opens.
      - `:on-message-handler`: (fn [ws parsed-msg] ...) called for each message.
      - `:on-close-handler`: (fn [ws status reason] ...) called when the connection closes.

  Returns:
  - The websocket instance with your handlers."
  [uri {:keys [on-message-handler on-open-handler on-close-handler]}]
  @(ws/websocket uri
                 {:on-message (fn [ws msg last?]
                                (let [msg-str (if (instance? CharBuffer msg)
                                                (str msg)
                                                msg)
                                      parsed (json/parse-string msg-str true)]
                                  (on-message-handler  ws parsed)))
                  :on-open (fn [ws]
                             (on-open-handler  ws))
                  :on-close (fn [ws status reason]
                              (on-close-handler  ws status reason))}))

(defn subscribe [ws msg]
  (ws/send! ws (json/generate-string msg)))

(defn send! [ws msg]
  (ws/send! ws (json/generate-string msg)))

(defn close! [ws]
  (ws/close! ws))

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
        result-promise (promise)]
    (connect uri
             {:on-message-handler (fn [ws msg]
                                    (case (first msg)
                                      "EVENT" (let [event (nth msg 2 nil)]
                                                (swap! resources conj event)) ;; Add to resources
                                      "EOSE"  (do
                                                (deliver result-promise @resources) ;; Deliver resources to the promise
                                                (close! ws)))) ;; TODO should I really close here?
              :on-open-handler (fn [ws]
                                 (send! ws ["REQ" (random-uuid) filter]))})
    @result-promise))

(comment
  (let [ws (connect "ws://localhost:7778"
                    {:on-message-handler (fn [ws msg]
                                           (println "Received:" msg)
                                           msg)
                     :on-open-handler (fn [ws]
                                        (println "WS:" ws))})]
    (subscribe ws {:kinds [30142]
                   :limit 2})
    (subscribe ws {:kinds [1]
                   :limit 2})))

