(ns clothing-recommender.metrics)

(defn confusion-matrix
  "Returns TP FP FN TN."
  [true-labels predicted-labels positive-label]
  (reduce
    (fn [acc [t p]]
      (cond
        (and (= t positive-label) (= p positive-label))
        (update acc :tp inc)

        (and (not= t positive-label) (= p positive-label))
        (update acc :fp inc)

        (and (= t positive-label) (not= p positive-label))
        (update acc :fn inc)

        :else
        (update acc :tn inc)))
    {:tp 0 :fp 0 :fn 0 :tn 0}
    (map vector true-labels predicted-labels)))

(defn accuracy
  "Ratio of correctly predicted observations to the total observations."
  [{:keys [tp fp fn tn]}]
  (/ (+ tp tn)
     (+ tp fp fn tn)))

(defn precision
  "Ratio of correctly predicted positive observations to the total predicted positives."
  [{:keys [tp fp]}]
  (if (zero? (+ tp fp))
    0
    (/ tp (+ tp fp))))

(defn recall
  "Ratio of correctly predicted positive observations to all actual positives."
  [{:keys [tp fn]}]
  (if (zero? (+ tp fn))
    0
    (/ tp (+ tp fn))))

(defn f1-score
  "The harmonic mean of precision and recall, balancing both metrics."
  [precision recall]
  (if (zero? (+ precision recall))
    0
    (* 2 (/ (* precision recall)
            (+ precision recall)))))

(defn to-percent
  "Converts ratio (0–1) to percentage with 2 decimals."
  [x]
  (let [v (* 10000.0 (double x))]
    (/ (Math/round v) 100.0)))

(defn accuracy%
  [cm]
  (to-percent (accuracy cm)))

(defn precision%
  [cm]
  (to-percent (precision cm)))

(defn recall%
  [cm]
  (to-percent (recall cm)))

(defn f1-score%
  [cm]
  (to-percent (f1-score (precision cm) (recall cm))))