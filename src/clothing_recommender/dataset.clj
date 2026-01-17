(ns clothing-recommender.dataset
  (:require
    [clothing-recommender.training-data :as td]
    [clothing-recommender.decision-tree :as tree]
    [clojure.edn :as edn]))

;; label generator
(defn simulate-label
  [user product]
  (let [price-ok   (<= (:price product) (:budget user))
        rating-ok  (>= (:rating product)
                       (get-in user [:preferences :min-rating]))
        brand-ok   (some #{(:brand product)}
                         (get-in user [:preferences :brands]))
        category-ok (some #{(:category product)}
                          (get-in user [:preferences :categories]))
        color-ok (some #{(:color product)}
                       (get-in user [:preferences :color]))

        ptype (tree/product-size-type product)
        size-ok (and ptype
                     (= (:size product)
                        (get-in user [:sizes ptype])))]

    (if (and price-ok category-ok size-ok (or brand-ok rating-ok color-ok))
      :recommend
      :not-recommend)))

;; 1 row in dataset
(defn user-product-instance
  [user product]
  (-> (td/feature-vector user product)
      (assoc :label (simulate-label user product))))

(defn build-dataset
  [users products]
  (vec
    (for [u users
          p products]
      (user-product-instance u p))))

;;persistence
(defn save-dataset
  "Saves dataset to disk as EDN."
  [dataset path]
  (spit path (pr-str dataset)))

(defn load-dataset
  "Loads dataset from EDN file."
  [path]
  (-> path
      slurp
      edn/read-string))