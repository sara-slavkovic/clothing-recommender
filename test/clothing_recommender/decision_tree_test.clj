(ns clothing-recommender.decision-tree-test
  (:require [clojure.test :refer :all]
            [clothing-recommender.decision-tree :as dt]))

(deftest decision-tree-basic-test
  (let [user {:budget-norm 0.5
              :min-rating-norm 0.6
              :sizes {:tops "M" :pants "S" :shoes "M"}
              :preferences {:categories ["Women's Fashion"]
                            :brands ["Adidas"]
                            :colors ["Black"]}}

        product {:price-norm 0.4
                 :rating-norm 0.9
                 :category "Women's Fashion"
                 :brand "Adidas"
                 :product-name "T-shirt"
                 :color "Black"
                 :size "M"}]

    (let [score (dt/decision-tree-score user product)]
      (is (number? score))
      (is (<= 0 score 100)))))