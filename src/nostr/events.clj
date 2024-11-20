(ns nostr.events
  (:require [nostr.util :as util]
            [cheshire.core :as json]
            
            )
  (:import (java.nio.charset StandardCharsets)))

(defn make-id
  "copied from uncle bob's more speech.
  returns byte array of id given the clojure form of the body"
  [{:keys [pubkey created_at kind tags content]}]
  (let [id-event (json/generate-string  [0 pubkey created_at kind tags content])
        id (util/sha-256 (.getBytes id-event StandardCharsets/UTF_8))]
    id)
  )

(defn- pow2 [n] (reduce * (repeat n 2N)))

(defn make-id-with-pow
  "returns byte array and updated body of id given the clojure form of the body, and the
  POW constraint given in the number of preceding binary zeroes."
  [pow body]
  (let [limit (pow2 (- 256 pow))]
    (loop [nonce 0]
      (let [body (update-in body [:tags] concat [[:nonce (str nonce) (str pow)]])
            id (make-id body)
            id-num (util/bytes->num id)]
        (if (< id-num limit)
          [id body]
          (recur (inc nonce)))))))
