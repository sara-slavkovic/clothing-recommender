(ns clothing-recommender.csv-loader-test
  (:require [clojure.test :refer :all]
            [clothing-recommender.csv-loader :as loader]))

(def car-eval-path "csv/car_evaluation.csv")
(def loan-path     "csv/loan_approval_dataset.csv")
(def tennis-path   "csv/play_tennis_dataset.csv")

(deftest test-lower-key
  (is (= (loader/lower-key "Buying_Price") :buying_price))
  (is (= (loader/lower-key " Play ") :play)))

(deftest test-id-column
  (is (loader/id-column? :loan_id))
  (is (loader/id-column? :day_id))
  (is (not (loader/id-column? :safety)))
  (is (not (loader/id-column? :decision))))

(deftest test-sanitize-headers
  (let [headers ["Day_ID" "Outlook" "Temperature" "Humidity" "Wind" "Play"]
        sanitized (loader/sanitize-headers headers)]
    ;; ID removed, order preserved
    (is (= (map second sanitized)
           [:outlook :temperature :humidity :wind :play]))))

(deftest test-parse-value
  (is (= (loader/parse-value "42") 42))
  (is (= (loader/parse-value "3.5") 3.5))
  (is (= (loader/parse-value " Graduate ") "Graduate"))
  (is (= (loader/parse-value "Yes") "Yes")))

(deftest test-row->map
  (let [indexed-headers [[0 :outlook] [1 :temperature] [2 :humidity] [3 :wind] [4 :play]]
        row ["Sunny" "Mild" "High" "Strong" "No"]]
    (is (= (loader/row->map indexed-headers row)
           (array-map
             :outlook "Sunny"
             :temperature "Mild"
             :humidity "High"
             :wind "Strong"
             :play "No")))))

(deftest test-load-car-evaluation
  (let [data (loader/load-csv->maps car-eval-path)
        first-row (first data)]
    ;; attributes present
    (is (every? #(contains? first-row %)
                [:buying_price :maintenance_cost :doors :persons
                 :lug_boot :safety :decision]))
    ;; numeric parsing
    (is (number? (:doors first-row)))
    (is (number? (:persons first-row)))
    ;; label
    (is (= (:decision first-row) "unacc"))))

(deftest test-load-loan-approval
  (let [data (loader/load-csv->maps loan-path)
        first-row (first data)]
    ;; ID removed
    (is (not (contains? first-row :loan_id)))
    ;; attributes present
    (is (every? #(contains? first-row %)
                [:no_of_dependents :education :self_employed :income_annum
                 :loan_amount :loan_term :cibil_score :residential_assets_value
                 :commercial_assets_value :luxury_assets_value
                 :bank_asset_value :loan_status]))
    ;; numeric parsing
    (is (number? (:income_annum first-row)))
    (is (number? (:loan_term first-row)))
    ;; label
    (is (= (:loan_status first-row) "Approved"))))

(deftest test-load-play-tennis
  (let [data (loader/load-csv->maps tennis-path)
        first-row (first data)]
    ;; Day_ID removed
    (is (not (contains? first-row :day_id)))
    ;; attributes present
    (is (= (set (keys first-row))
           #{:outlook :temperature :humidity :wind :play}))
    ;; label
    (is (= (:play first-row) "Yes"))))