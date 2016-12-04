(ns datomic-transactions.coerce
  "Code to check the consistency of transaxion data output.

  It's difficult to test code that prouduces transaction data, as temporary ids are generated randomly. The code in
  this gist provides a way to test the equivalence of two transactions."
  (:require [clojure-utils.bimap :as bimap]
            [clojure-utils.core :as core]))


(defn is-transacted-db-id? [id]
  "Is this a transacted db id"
  (or (integer? id) (keyword? id)))

(defn is-temp-db-id? [id]
  "Is this a temp db id?"
  (and (map? id) (contains? id :part) (contains? id :idx)))

(defn error
  "return an error entity"
  ([type]
   {:error/type type})
  ([type data]
   (assoc (error type) :error/data data)))

(defn ids-valid? [expected-id expected-ident actual-id actual-ident]

  "Checks that ids are valid"
  (or

    ;; if the ids are already transacted then we just need to check they are equal
    (and (is-transacted-db-id? actual-id) (= actual-id expected-id))

    ;; It's valid when the actual has a transacted numerical id and an ident, and the expected id is mapped to the ident
    (and (is-transacted-db-id? actual-id) (= expected-id actual-ident))

    ;; if they are temp ids we need to check that the partitions are the same
    (and (is-temp-db-id? actual-id) (is-temp-db-id? expected-id) (= (:part actual-id) (:part expected-id)))

    ))

(defn update-output [output id-path id]
  (assoc-in output id-path id))


(defn coerce-entity-ids
  "Make actual entity ids consistent with expected entity ids"
  ([actual-entity expected-entity]
   (coerce-entity-ids actual-entity expected-entity (bimap/create-empty)))
  ([actual-entity expected-entity input-exp-act-bimap]
   (->>  (core/flatten-upto-keys expected-entity :db/id :db/ident)
        (reduce
          (fn [[exp-act-bimap output] [expected-id-map-path {expected-id :db/id expected-ident :db/ident}]]
            (let [{actual-id :db/id
                   actual-ident :db/ident} (get-in actual-entity expected-id-map-path)
                  expected-id-path (conj expected-id-map-path :db/id)]
              (cond
                ;; id attribute from expected entity does not exist in actual entity
                (nil? actual-id)
                [exp-act-bimap (update-output output expected-id-path
                                         (error :error.type/expected-id {:expected-id expected-id}))]

                ;; actual-id and expected-id are consitently mapped and valid
                (and (bimap/consistently-unmapped? exp-act-bimap expected-id actual-id)
                     (ids-valid? expected-id expected-ident actual-id actual-ident))
                [(bimap/assoc exp-act-bimap expected-id actual-id) (update-output output expected-id-path expected-id)]

                ;; actual id and expected id are consistently mapped
                (bimap/consistently-mapped? exp-act-bimap expected-id actual-id)
                [exp-act-bimap (update-output output expected-id-path expected-id)]

                ;; there's an inconsistency between the actual id and expected id.
                :else (let [error (->> (error :error.type/inconsistent-ids
                                              (->> {:expected-id   expected-id
                                                    :actual-id     actual-id
                                                    :mapped-act-id (bimap/get-value exp-act-bimap expected-id)
                                                    :mapped-exp-id (bimap/get-key exp-act-bimap actual-id)}
                                                   (filter (comp not nil? second))
                                                   (into {})))
                                       (into {}))]
                        [exp-act-bimap (update-output output expected-id-path error)]))))

          [input-exp-act-bimap actual-entity]))))




(defn adjust-actual
  "For example ({:db/id :bob} {:db/id 1234 :db/ident :bob}) -> {:db/id :bob}"
  [expected actual]
  (if-let [ident (:db/ident actual)]
    (-> actual
        (dissoc :db/ident)
        (assoc :db/id ident))
    actual))


(defn coerce-entities-ids [actual-entities expected-entities]
  "Make all actual entity ids consistent with the expected entity ids."
  (->> (map vector actual-entities expected-entities)
       (reduce (fn [[exp-act-id-bi-map output-coll] [actual expected]]
                 (let [[new-exp-act-id-bi-map output] (coerce-entity-ids actual expected exp-act-id-bi-map)]
                   [new-exp-act-id-bi-map (conj output-coll output)])) [(bimap/create-empty) []])
       (second)))


