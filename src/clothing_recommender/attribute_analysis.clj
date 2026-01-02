(ns clothing-recommender.attribute-analysis)

(def attributes
  [:price :rating :size-match :category :brand :color])

(defn attribute-impact
  "Measures how often attribute is present in recommended samples."
  [data attribute]
  (let [recommended (filter #(= :recommend (:label %)) data)
        avg (double
              (/ (count (filter #(pos? (get % attribute)) recommended))
                 (max 1 (count recommended))))]
    avg))

(defn analyze-attributes
  "Quinlan-style attribute usefulness analysis."
  [training-data]
  (into {}
        (map (fn [attr]
               [attr (attribute-impact training-data attr)])
             attributes)))