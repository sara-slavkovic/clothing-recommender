(ns clothing-recommender.attribute-analysis)

(def attributes
  [:price :rating :size-match :category :brand :color])

(defn attribute-impact
  "Measures how often attribute is present in recommended samples.
  Works with discrete (keyword or 0/1) attributes."
  [data attribute]
  (let [recommended (filter #(= :recommend (:label %)) data)
        present? (fn [x]
                   (let [v (get x attribute)]
                     (or (= v 1)
                         (keyword? v))))]
    (/ (count (filter present? recommended))
       (max 1 (count recommended)))))

(defn analyze-attributes
  "Quinlan-style attribute usefulness analysis."
  [training-data]
  (into {}
        (map (fn [attr]
               [attr (attribute-impact training-data attr)])
             attributes)))