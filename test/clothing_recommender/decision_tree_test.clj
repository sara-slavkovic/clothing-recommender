(ns clothing-recommender.decision-tree-test
  (:require [clojure.test :refer :all]
            [clothing-recommender.users :as users]
            [clothing-recommender.decision-tree :as dt]))

(deftest decision-tree-basic-test
  (let [user users/sara
        product {:price 80
                 :rating 4.5
                 :category "Women's Fashion"
                 :brand "Adidas"
                 :product-name "T-shirt"
                 :color "Black"
                 :size "M"}]
    (is (= (dt/decision-tree-score user product) 100))))

