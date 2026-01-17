(ns clothing-recommender.preprocessing)

(defn attributes
  "Returns all attribute keys except label."
  [dataset label-key]
  (-> dataset first keys set (disj label-key)))

(defn numeric-attribute?
  [dataset attr]
  (every? number? (map attr dataset)))

(defn quantile-cuts
  "Computes cut points for k bins."
  [values k]
  (let [sorted (sort values)
        n      (count sorted)]
    (map #(nth sorted (int (* % n)))
         (map #(/ % k) (range 1 k)))))

(defn make-binner
  "Returns a function that maps a numeric value to :low/:medium/:high based on cuts."
  [cuts]
  (fn [v]
    (cond
      (< v (first cuts)) :low
      (< v (second cuts)) :medium
      :else :high)))

(defn build-discretizers
  "Builds discretizers for all numeric attributes in dataset using quantiles."
  [dataset label-key]
  (reduce
    (fn [acc attr]
      (if (numeric-attribute? dataset attr)
        (let [values (vec (map attr dataset))]  ;; materijalizuj sekvencu
          (assoc acc attr
                     (-> values
                         (quantile-cuts 3)
                         make-binner)))
        acc))
    {}
    (attributes dataset label-key)))

(defn discretize-instance
  [instance discretizers]
  (reduce
    (fn [inst [attr f]]
      (update inst attr f))
    instance
    discretizers))

(defn discretize-dataset
  "Returns {:data discretized-data :discretizers discretizers}"
  [dataset label-key]
  (let [discretizers (build-discretizers dataset label-key)
        data (map #(discretize-instance % discretizers) dataset)]
    {:data data
     :discretizers discretizers}))
