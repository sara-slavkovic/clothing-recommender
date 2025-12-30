(ns clothing-recommender.decision-tree)

;; -------------------------
;; Helper functions
;; -------------------------

(defn in?
  [coll x]
  (some #(= % x) coll))


;; -------------------------
;; Decision nodes (scoring)
;; -------------------------

(defn price-score
  [user product]
  (let [budget (:budget user)
        price  (:price product)]
    (cond
      (<= price budget)           30
      (<= price (* 1.2 budget))   15
      :else                        0)))


(defn size-score
  [user product]
  (let [product-size (:size product)
        user-sizes   (:sizes user)]
    (if (in? (vals user-sizes) product-size)
      25
      0)))


(defn rating-score
  [user product]
  (let [min-rating (get-in user [:preferences :min-rating] 0)
        rating     (:rating product)]
    (cond
      (>= rating min-rating)         20
      (>= rating (dec min-rating))   10
      :else                           0)))


(defn category-score
  [user product]
  (let [preferred (get-in user [:preferences :categories])
        category  (:category product)]
    (if (and preferred (in? preferred category))
      10
      0)))


(defn brand-score
  [user product]
  (let [preferred (get-in user [:preferences :brands])
        brand     (:brand product)]
    (if (and preferred (in? preferred brand))
      10
      0)))


(defn color-score
  [user product]
  (let [preferred (get-in user [:preferences :colors])
        color     (:color product)]
    (if (and preferred (in? preferred color))
      5
      0)))


;; -------------------------
;; Final decision tree
;; -------------------------

(defn decision-tree-score
  "Evaluates a single product for a given user and returns a score (0–100)."
  [user product]
  (+ (price-score user product)
     (size-score user product)
     (rating-score user product)
     (category-score user product)
     (brand-score user product)
     (color-score user product)))


(defn recommend
  "Scores all products for a user and returns top N recommendations."
  [user products n]
  (->> products
       (map (fn [p]
              {:product p
               :score   (decision-tree-score user p)}))
       (sort-by :score >)
       (take n)))
