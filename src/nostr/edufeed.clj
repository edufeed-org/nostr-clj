(ns nostr.edufeed
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [nostr.core :as nostr]))

(defn get-key [coll key]
  (or (get coll key) ""))

(defn transform-name [e]
  ["name" e])

(defn transform-description [e]
  ["description" e])

(defn transform-image [e]
  ["image" e])

(defn transform-creator [creator]
  (map (fn  [e]
         ["creator" (get-key e :id) (get-key e :name) (get-key e :type)])
       creator))

(defn transform-contributor [contributor]
  (map (fn  [e]
         ["contributor" (get-key e :id) (get-key e :name) (get-key e :type)])
       contributor))

(defn transform-publisher [publisher]
  (map (fn  [e]
         ["publisher" (get-key e :id)  (get-key e :name) (get-key e :type)])
       publisher))

(defn transform-keywords [keywords]
  (into ["keywords"] keywords))

(defn transform-about [about]
  (let [langs (keys (get about :prefLabel))]
    (map (fn [k]
           ["about" (get about :id "") (get-in about [:prefLabel k]) (name k)])
         langs)))

(defn transform-learningResourceType [learningResourceType]
  (let [langs (keys (get learningResourceType :prefLabel))]
    (map (fn [k]
           ["learningResourceType" (get learningResourceType :id "") (get-in learningResourceType [:prefLabel k]) (name k)])
         langs)))

(defn transform-inLanguage [l]
  (into ["inLanguage"] l))

(defn transform-isAccessibleForFree [a]
  ["isAccessibleForFree" (str a)])

(defn transform-type [t]
  (into ["type"] t))

(defn transform-datePublished [d]
  ["datePublished" d])

(defn transform-dateCreated [d]
  ["dateCreated" d])

(defn transform-conditionsOfAccess [c]
  (let [langs (keys (get c :prefLabel))]
    (map (fn [k]
           ["conditionsOfAccess" (get-key c :id) (get-in c [:prefLabel k]) (name k)])
         langs)))

(defn transform-license [l]
  ["license" (:id l)])

(defn transform-value [k v]
  (case k
    :name [(transform-name v)]
    :description [(transform-description v)]
    :image [(transform-image v)]
    :creator (transform-creator v)
    :keywords [(transform-keywords v)]
    :about (mapcat transform-about v)
    :inLanguage [(transform-inLanguage v)]
    :isAccessibleForFree [(transform-isAccessibleForFree v)]
    :type [(transform-type v)]
    :datePublished [(transform-datePublished v)]
    :dateCreated [(transform-dateCreated v)]
    :conditionsOfAccess (transform-conditionsOfAccess v)
    :license [(transform-license v)]
    :contributor (transform-contributor v)
    :publisher (transform-publisher v)
    :learningResourceType (mapcat transform-learningResourceType v)
    :id [["r" v] ["d" v] ["id" v]]
    []))

(defn transform-amb-to-30142-event [amb]
  (let [tags (mapcat (fn [[k v]]
                       (transform-value k v))
                     amb)
        raw-event {:kind 30142
                   :content ""
                   :tags tags}]
    raw-event))

(comment
  (let [jfile (slurp "resources/test-data.json")
        decoded (json/parse-string jfile true)]
    ; (transform-amb-to-30142-event (select-keys decoded [:learningResourceType])))
    (transform-amb-to-30142-event decoded)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now the other way around ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transform-skos-fields [data]
  (->> data
       (reduce (fn [acc [_, id label lang]]
                 (update acc id
                         (fn [m] (update m :prefLabel assoc (keyword lang) label))))
               {})
       (map (fn [[id {:keys [prefLabel]}]] {:id id, :prefLabel prefLabel}))
       (into [])))

(defmulti extract-tag (fn [tags key] key))

(defmethod extract-tag :default [tags key]
  (second (first (filter #(= key (first %)) tags))))

(defmethod extract-tag "creator" [tags _]
  (map (fn [e]
         (let [id (second e)
               name (nth e 2)
               type (get e 3 "Person")]
           (cond-> {:name name}
             (not (str/blank? id)) (assoc :id id)
             (not (nil? type)) (assoc :type type))))

       (filter #(= "creator" (first %)) tags)))

(defmethod extract-tag "type" [tags _]
  (rest (first (filter #(= key (first %)) tags))))

(defn extract-tag-array [tags key]
  (nthrest (first (filter #(= key (first %)) tags)) 1))

(defn convert-30142-to-nostr-amb
  "Args:
  - `event`
  - `skip-raw-event`: boolean don't include the raw event info in result
  - `amb-id`: boolean return the resource id instead of event id"
  ([event] (convert-30142-to-nostr-amb event true))
  ([event skip-raw-event] (convert-30142-to-nostr-amb event skip-raw-event false))
  ([event skip-raw-event amb-id]
   (let [tags (:tags event)
         amb-result {;; TODO context sollte auch bei der Konvertierung gespeichert
                     ;; und hier entsprechend ausgegeben werden
                     (keyword "@context") ["https://w3id.org/kim/amb/context.jsonld",
                                           "https://schema.org",
                                           {(keyword "@language") "de"}]

                     :id (if amb-id (extract-tag tags "r") (:id event)) ;; TODO this needs to be amb id for "real amb"
                     :type (let [type (extract-tag-array tags "type") ]
                            (if (nil? (seq type)) 
                              ["LearningResource"]
                              type))
                     :name (extract-tag tags "name")
                     :creator (extract-tag tags "creator")
                     :description (extract-tag tags "description")
                     :keywords (extract-tag-array tags "keywords")
                     :about (transform-skos-fields (filter #(= "about" (first %)) tags))}]
     (if skip-raw-event
       amb-result
       (assoc amb-result
              :event_raw event
              :event_created_at (:created_at event)
              :event_kind (:kind event)
              :event_pubkey (:pubkey event)
              :resource_id (extract-tag tags "r"))))))

(comment
  (def test-event {:id "1234"
                   :kind 30142
                   :pubkey "12345"
                   :created_at 123456
                   :content ""
                   :tags [["r" "https://oersi.org/resources/aHR0cHM6Ly9hdi50aWIuZXUvbWVkaWEvNjY5ODM="]
                          ["id" "https://oersi.org/resources/aHR0cHM6Ly9hdi50aWIuZXUvbWVkaWEvNjY5ODM="]
                          ; ["type" "CreativeWork" "LearningResource"]
                          ["name", "Wurzeln konstruieren: Die Schnecke des Pythagoras"],
                          ["creator", "test-uri", "Christian Spannagel"],
                          ["creator", "", "Maxi Muster"],
                          ["image", "https://av.tib.eu/thumbnail/66983"]
                          ["about", "https://w3id.org/kim/hochschulfaechersystematik/n37", "Mathe", "de"],
                          ["about", "https://w3id.org/kim/hochschulfaechersystematik/n37", "Mathematics", "en"],
                          ["about", "https://...", "Geometrie", "de"],
                          ["resourceType", "https://...", "Video", "de"],
                          ["inLanguage", "de"],
                          ["keywords", "Pythagoras", "Geometrie"],
                          ["license", "https://creativecommons.org/licenses/by/3.0", "cc-by"],
                          ["source", "https://av.tib.eu/", "TIB AV-Portal"]]})

  (convert-30142-to-nostr-amb test-event true true))

