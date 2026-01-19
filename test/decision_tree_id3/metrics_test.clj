(ns decision-tree-id3.metrics-test
  (:require [clojure.test :refer :all]
            [decision-tree-id3.metrics :as m]))

(def true-labels ["Yes" "No" "Yes" "No" "Yes"])
(def predicted ["Yes" "No" "No"  "No" "Yes"])
(def positive "Yes")
(def cm (m/confusion-matrix true-labels predicted positive))

(deftest confusion-matrix-test
  (is (= cm {:tp 2 :fp 0 :fn 1 :tn 2})))

(deftest accuracy-test
  (is (= (m/accuracy cm) (/ 4 5))))

(deftest precision-test
  (is (= (m/precision cm) (/ 2 2))))

(deftest recall-test
  (is (= (m/recall cm) (/ 2 3))))

(deftest f1-score-test
  (is (= (m/f1-score (m/precision cm) (m/recall cm))
         (* 2 (/ (* (m/precision cm) (m/recall cm))
                 (+ (m/precision cm) (m/recall cm)))))))

(deftest to-percent-test
  (is (= (m/to-percent 0.123456) 12.35))
  (is (= (m/to-percent 0.875) 87.5)))

(deftest metrics-percent-test
  (is (= (m/accuracy% cm) (m/to-percent (m/accuracy cm))))
  (is (= (m/precision% cm) (m/to-percent (m/precision cm))))
  (is (= (m/recall% cm) (m/to-percent (m/recall cm))))
  (is (= (m/f1-score% cm) (m/to-percent (m/f1-score (m/precision cm) (m/recall cm))))))