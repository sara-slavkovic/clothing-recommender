(ns clothing-recommender.discretization)

;; -----------------------------
;; continuous -> discrete
;; -----------------------------

(defn quantiles
  "Returns cut points for k bins."
  [values k]
  (let [sorted (sort values)
        n      (count sorted)]
    (map #(nth sorted (int (* % n)))
         (map #(/ % k) (range 1 k)))))

(defn make-binner
  [cuts]
  (fn [x]
    (cond
      (< x (first cuts)) :low
      (< x (second cuts)) :medium
      :else :high)))

(defn build-discretizers
  [dataset]
  (let [prices  (map :price dataset)
        ratings (map :rating dataset)]
    {:price-bin  (make-binner (quantiles prices 3))
     :rating-bin (make-binner (quantiles ratings 3))}))

(defn discretize-instance
  [instance {:keys [price-bin rating-bin]}]
  (-> instance
      (update :price price-bin)
      (update :rating rating-bin)))
