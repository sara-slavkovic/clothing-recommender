(ns clothing-recommender.core)

(defn simple-recommendation
      [temperature]
      (if (< temperature 15)
        "Wear a jacket."
        "A t-shirt is fine."))

(defn filter-warm-clothes
      [items]
      (filter #(= (:type %) :warm) items))

(defn -main
      [& _]
      (println "Example 1:" (simple-recommendation 10))
      (println "Example 2:" (filter-warm-clothes
                              [{:name "Jacket" :type :warm}
                               {:name "T-shirt" :type :light}])))