(ns datomic-transactions.coerce-test
  "Tests to show how the make-ids-consistent function works"
  (:require [datomic-transactions.coerce :as coerce]
            [clojure.test :refer :all]
            [datomic.api :as d]))


(deftest test-adjust-actual

  (is (= (coerce/adjust-actual {:db/id :bob} {:db/id 1234 :db/ident :bob})
         {:db/id :bob}))

  (is (= (coerce/adjust-actual {:db/id 1234 :db/ident :bob} {:db/id 1234 :db/ident :bob})
         {:db/id 1234
          :db/ident :bob}))

  (is (= (coerce/adjust-actual {:db/id 1234} {:db/id 1234 :db/ident :bob})
         {:db/id 1234}))

  )




(defn create-comparable-output [desc actual expected expected-output]
  (let [actual-made-consistent (coerce/coerce-entities-ids actual expected)]
    [expected-output actual-made-consistent desc]))

(deftest make-ids-consistent-tests

  (->> (create-comparable-output "Ids are equivalent"

                                 [{:db/id (d/tempid :db.part/user -2)}
                                  {:db/id (d/tempid :db.part/user -2)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -1)}]


                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -1)}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Entity with stuff"
                                 [{:test-entity/string-field "Bob12",
                                   :db/id                    {:part :db.part/entity-schema, :idx -1000104}}
                                  {:test-entity/string-field "Bob12",
                                   :db/id                    {:part :db.part/entity-schema, :idx -1000104}}]
                                 [{:test-entity/string-field "Bob12",
                                   :db/id                    {:part :db.part/entity-schema, :idx -1}}
                                  {:test-entity/string-field "Bob12",
                                   :db/id                    {:part :db.part/entity-schema, :idx -1}}]

                                 [{:db/id                    {:part :db.part/entity-schema, :idx -1}
                                   :test-entity/string-field "Bob12"}
                                  {:db/id                    {:part :db.part/entity-schema, :idx -1}
                                   :test-entity/string-field "Bob12"}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Nested Ids are equivalent"

                                 [{:db/id      (d/tempid :db.part/user -3)
                                   :entity/ref {:db/id (d/tempid :db.part/user -4)}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}]



                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Nested equivalent ids with same ref"
                                 [{:db/id      (d/tempid :db.part/user -3)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Nested equivalent transactions with transacted id"
                                 [{:db/id      (d/tempid :db.part/user -3)
                                   :entity/ref {:db/id :some-ident}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id :some-ident}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id :some-ident}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Missing Id"
                                 [{}]

                                 [{:db/id (d/tempid :db.part/user -1)}]

                                 [{:db/id {:error/type :error.type/expected-id,
                                           :error/data {:expected-id (d/tempid :db.part/user -1)}}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Expected and actual have inconsistent partitions"

                                 [{:db/id (d/tempid :db.part/user -2)}
                                  {:db/id (d/tempid :db.part/user -2)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/custom -1)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                        :expected-id   (d/tempid :db.part/custom -1)
                                                        :mapped-exp-id (d/tempid :db.part/user -1)}
                                           :error/type :error.type/inconsistent-ids}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))


  (->> (create-comparable-output "Expected ids different, actual the same"
                                 [{:db/id (d/tempid :db.part/user -2)}
                                  {:db/id (d/tempid :db.part/user -2)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -3)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                        :expected-id   (d/tempid :db.part/user -3)
                                                        :mapped-exp-id (d/tempid :db.part/user -1)}
                                           :error/type :error.type/inconsistent-ids}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))


  (->> (create-comparable-output "Expected ids the same, actual different"
                                 [{:db/id (d/tempid :db.part/user -4)}
                                  {:db/id (d/tempid :db.part/user -2)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -1)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                        :expected-id   (d/tempid :db.part/user -1)
                                                        :mapped-act-id (d/tempid :db.part/user -4)}
                                           :error/type :error.type/inconsistent-ids}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))


  (->> (create-comparable-output "Different transacted ids"

                                 [{:db/id :something2}]

                                 [{:db/id :something}]

                                 [{:db/id {:error/type :error.type/inconsistent-ids
                                           :error/data {:actual-id   :something2
                                                        :expected-id :something}}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Long form of transacted ids"

                                 [{:db/id 1234567}]

                                 [{:db/id 1234567}]

                                 [{:db/id 1234567}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "When the expected db/id has an equivalent db/ident"

                                 [{:db/id 1234567
                                   :db/ident :bob}]

                                 [{:db/id :bob}]

                                 [{:db/id :bob}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Same db/ident diff temp db/id"

                                 [{:db/id (d/tempid :part -1)
                                   :db/ident :bob}]

                                 [{:db/id (d/tempid :part -2)
                                   :db/ident :bob}]

                                 [{:db/id {:error/type :error.type/inconsistent-ids
                                           :error/data {:actual-id   (d/tempid :part -2)
                                                        :expected-id (d/tempid :part -1)}}
                                   :db/ident :bob}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "When the expected id has an ident and that is used to refer"

                                 [{:db/id 123456}
                                  {:db/id 123456}]

                                 [{:db/id :something}
                                  {:db/id :something-else}]

                                 [{:db/id :something}
                                  {:db/id {:error/data {:actual-id     123456
                                                        :expected-id   :something-else
                                                        :mapped-exp-id :something}
                                           :error/type :error.type/inconsistent-ids}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  )







