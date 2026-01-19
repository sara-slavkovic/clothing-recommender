(ns decision-tree-id3.id3-test
  (:require [clojure.test :refer :all]
            [decision-tree-id3.id3 :as id3]))

(def sample-dataset
  [{:outlook "Sunny"    :temperature 30 :humidity 85 :wind "Strong" :play "No"}
   {:outlook "Overcast" :temperature 25 :humidity 70 :wind "Weak"   :play "Yes"}
   {:outlook "Rainy"    :temperature 20 :humidity 90 :wind "Weak"   :play "Yes"}])

(def attrs [:outlook :temperature :humidity :wind])
(def label-key :play)

(deftest split-by-attribute-test
  (let [splits (id3/split-by-attribute sample-dataset :outlook)]
    (is (= 3 (count splits)))
    (is (contains? splits "Sunny"))
    (is (contains? splits "Overcast"))
    (is (contains? splits "Rainy"))))

(deftest majority-label-test
  (is (= "Yes" (id3/majority-label sample-dataset label-key))))

(deftest same-label?-test
  (is (false? (id3/same-label? sample-dataset label-key)))
  (is (true? (id3/same-label? [{:play "Yes"} {:play "Yes"}] label-key))))

(deftest information-gain-test
  (doseq [attr attrs]
    (is (>= (id3/information-gain sample-dataset attr label-key) 0))))

(deftest best-attribute-test
  (let [best (id3/best-attribute sample-dataset attrs label-key)]
    (is (some #{best} attrs))))

(deftest build-tree-test
  (let [tree (id3/build-tree sample-dataset attrs label-key)]
    (is (map? tree))
    (is (some #(contains? tree %) attrs))))

(deftest predict-test
  (let [tree (id3/build-tree sample-dataset attrs label-key)]
    ;; normal prediction
    (is (some #{"Yes" "No"} [(id3/predict tree {:outlook "Sunny" :temperature 30} sample-dataset label-key)]))
    ;; missing attribute value, should fall back to majority-label
    (is (= "Yes" (id3/predict tree {:outlook "Foggy" :temperature 99} sample-dataset label-key)))
    (is (some #{"Yes" "No"} [(id3/predict tree {:outlook "Rainy" :temperature 20} sample-dataset label-key)]))))

(deftest print-tree-test
  ;; call it to ensure no exception
  (let [tree (id3/build-tree sample-dataset attrs label-key)]
    (id3/print-tree tree 0)))