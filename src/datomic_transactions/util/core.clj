(ns datomic-transactions.util.core)


(defn flatten-keys [m]
  "Flatten all paths in the map"
  (letfn [(flatten-keys* [a ks m]
            (if (map? m)
              (if (empty? m)
                a
                (reduce into (map (fn [[k v]] (flatten-keys* a (conj ks k) v)) (seq m))))
              (assoc a ks m)))]
    (flatten-keys* {} [] m)))



(defn flatten-for-keys [m k & ks]
  "Flatten all paths ending in the specified key.

  E.g.

  (flatten-for-keys {:a 1
                    :b {:a 2
                        :c 3}
                    :d 4} :a) => {[:a] 1 [:b :a] 2}"
  (let [keys (into #{k} ks)]
    (letfn [(flatten-upto-key* [a ks m]
            (let [last-k (last ks)]
              (if (contains? keys last-k)
                (update a (pop ks) assoc last-k m)
                (if (and (map? m) (not (empty? m)))
                  (reduce (partial merge-with into) (map (fn [[k v]] (flatten-upto-key* a (conj ks k) v)) (seq m)))
                  a))))]
    (if (empty? m) m (flatten-upto-key* {} [] m)))))



(defn dissoc-in
  "Dissociates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  [m [k & ks]]
  (if ks
    (assoc m k (dissoc-in (get m k) ks))
    (dissoc m k)))


