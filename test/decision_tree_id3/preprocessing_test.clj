(ns decision-tree-id3.preprocessing-test
  (:require [clojure.test :refer :all]
            [decision-tree-id3.preprocessing :as pre]))

(def sample-dataset
  [{:outlook "Sunny" :temperature 30 :humidity 85 :wind "Strong" :play "No"}
   {:outlook "Overcast" :temperature 25 :humidity 70 :wind "Weak" :play "Yes"}
   {:outlook "Rainy" :temperature 20 :humidity 90 :wind "Weak" :play "Yes"}])

(def label-key :play)

(deftest test-attributes
  (let [attrs (pre/attributes sample-dataset label-key)]
    (is (= attrs #{:outlook :temperature :humidity :wind}))))

(deftest test-numeric-attribute?
  (is (pre/numeric-attribute? sample-dataset :temperature))
  (is (pre/numeric-attribute? sample-dataset :humidity))
  (is (not (pre/numeric-attribute? sample-dataset :outlook)))
  (is (not (pre/numeric-attribute? sample-dataset :wind))))

(deftest test-quantile-cuts
  (let [values [10 20 30 40 50 60]
        cuts (vec (pre/quantile-cuts values 3))]
    ;; should return 2 cut points for 3 bins
    (is (= (count cuts) 2))
    ;; first cut < second cut
    (is (< (first cuts) (second cuts)))
    ;; all cuts are in original values
    (is (every? #(some #{%} values) cuts))))

(deftest test-make-binner
  (let [b (pre/make-binner [25 50])]
    (is (= (b 10) :low))
    (is (= (b 30) :medium))
    (is (= (b 60) :high))))

(deftest test-build-discretizers
  (let [discretizers (pre/build-discretizers sample-dataset label-key)]
    ;; only numeric attrs should be discretized
    (is (= (set (keys discretizers)) #{:temperature :humidity}))
    ;; each discretizer is a function
    (is (every? fn? (vals discretizers)))))

(deftest test-discretize-instance
  (let [discretizers (pre/build-discretizers sample-dataset label-key)
        instance {:temperature 28 :humidity 80 :outlook "Sunny" :wind "Weak" :play "No"}
        discretized (pre/discretize-instance instance discretizers)]
    ;; numeric values should be :low/:medium/:high
    (is (#{:low :medium :high} (:temperature discretized)))
    (is (#{:low :medium :high} (:humidity discretized)))
    ;; non-numeric values unchanged
    (is (= (:outlook discretized) "Sunny"))
    (is (= (:wind discretized) "Weak"))
    (is (= (:play discretized) "No"))))

(deftest test-discretize-dataset
  (let [result (pre/discretize-dataset sample-dataset label-key)
        data (:data result)
        discretizers (:discretizers result)]
    ;; discretizers exist for numeric attributes
    (is (= (set (keys discretizers)) #{:temperature :humidity}))
    ;; all instances discretized
    (is (= (count data) (count sample-dataset)))
    ;; check first instance numeric discretized
    (let [first-instance (first data)]
      (is (#{:low :medium :high} (:temperature first-instance)))
      (is (#{:low :medium :high} (:humidity first-instance)))
      ;; non-numeric unchanged
      (is (= (:outlook first-instance) "Sunny"))
      (is (= (:wind first-instance) "Strong"))
      (is (= (:play first-instance) "No")))))