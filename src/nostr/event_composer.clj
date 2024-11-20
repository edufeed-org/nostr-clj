(ns nostr.event-composer
  (:require [nostr.events :as events]
            [nostr.elliptic-signature :as ecc]
            [nostr.util :as util]))

(defn body->event
  "Copied from uncle bobs more speech client.
  (https://github.com/unclebob/more-speech/)
  Adds pubkey, created-at, id, and sig to the partially composed body,
  which must include kind, tags, and content.  The body is put into an
  EVENT wrapper that is ready to send.
  Private Key should be a hex-string"
  ([body private-key]
   (let [private-key (util/hex-string->bytes  private-key )
         public-key (util/bytes->hex-string (ecc/get-pub-key private-key))
         now (util/get-now)
         body (assoc body :pubkey public-key
                     :created_at now)
         [id body] (events/make-id-with-pow 12 body)
         aux-rand (util/num->bytes 32 (biginteger (System/currentTimeMillis)))
         signature (ecc/do-sign id private-key aux-rand)
         event (assoc body :id (util/bytes->hex-string id)
                      :sig (util/bytes->hex-string signature))]
     ["EVENT" event])))

(comment
  (let [private-key "9109ffc9fcddb1086e153ed23ff91c4f69f726178bf959f09b1f29c7569e24a1"
        body {:content "hello"
              :kind 30142
              :tags [["r" "hello"]]}]
    (body->event body private-key )
    )
  )
