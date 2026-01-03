(ns clothing-recommender.discretization)

;; -----------------------------
;; continuous -> discrete
;; -----------------------------

(defn price-bin
  "Discretize normalized price."
  [x]
  (cond
    (< x 0.33) :low
    (< x 0.66) :medium
    :else      :high))

(defn rating-bin
  "Discretize normalized rating."
  [x]
  (cond
    (< x 0.4)  :bad
    (< x 0.7)  :ok
    :else      :good))

(defn discretize-instance
  "Transforms numeric features into discrete attributes."
  [instance]
  (-> instance
      (update :price price-bin)
      (update :rating rating-bin)))