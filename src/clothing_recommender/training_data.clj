(ns clothing-recommender.training-data
  (:require [clothing-recommender.decision-tree :as tree]))

(defn feature-vector
  "ML features – no rule-based scoring."
  [user product]
  (let [ptype (tree/product-size-type product)]
    {:price        (:price product)
     :rating       (:rating product)

     ;; size depends on product type {:tops "M" :pants "S"}
     :size-match   (and ptype
                        (= (:size product)
                           (get-in user [:sizes ptype])))

     ;; preference matches
     :category     (if (some #{(:category product)}
                         (get-in user [:preferences :categories]))
                     :match
                     :no-match)

     :brand        (if (some #{(:brand product)}
                         (get-in user [:preferences :brands]))
                     :match
                     :no-match)

     :color        (if (some #{(:color product)}
                         (get-in user [:preferences :colors]))
                     :match
                     :no-match)}))
