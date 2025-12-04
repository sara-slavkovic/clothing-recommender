(ns clothing-recommender.core-test
    (:require [clothing-recommender.core :refer :all]
      [clojure.test :refer :all]))

(deftest simple-recommendation-test
         (testing "simple clothing recommendation based on temperature"
                  (is (= "Wear a jacket." (simple-recommendation 10)))
                  (is (= "A t-shirt is fine." (simple-recommendation 20)))))

(deftest filter-warm-clothes-test
         (testing "filter warm clothes from items list"
                  (let [items [{:name "Jacket" :type :warm}
                               {:name "T-shirt" :type :light}]]
                       (is (= [{:name "Jacket" :type :warm}]
                              (filter-warm-clothes items))))))

