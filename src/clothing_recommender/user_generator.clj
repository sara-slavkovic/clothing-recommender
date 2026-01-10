(ns clothing-recommender.user-generator
  (:require [clothing-recommender.user :as u]))

;------------------------------------
; allowed values
; -----------------------------------
(def categories ["Women's Fashion" "Men's Fashion" "Kids' Fashion"])

(def brands ["Nike" "Adidas" "H&M" "Zara" "Gucci"])

(def colors ["Yellow" "Black" "White" "Blue" "Green" "Red"])

(def sizes ["S" "M" "L" "XL"])

;------------------------------------
; helper functions
; -----------------------------------
 (defn rand-from [coll]
  (rand-nth coll))

(defn rand-subset
  "Random non-empty subset"
  [coll]
  (->> coll
       shuffle
       (take (+ 1 (rand-int (count coll))))
       vec))

(defn min-price [products]
  (apply min (map :price products)))

(defn max-price [products]
  (apply max (map :price products)))

;------------------------------------
; user generation
; -----------------------------------
(defn generate-user
  [id products]
  (let [min-p (min-price products)
        max-p (max-price products)]
    (u/make-user
      id
      (str "User-" id)

      ;; preferences
      {:categories (rand-subset categories)
       :brands     (rand-subset brands)
       :colors     (rand-subset colors)
       :min-rating (+ 1.0 (* 4.0 (rand)))}

      ;; sizes
      {:tops   (rand-from sizes)
       :pants  (rand-from sizes)
       :shoes  (rand-from sizes)}

      ;; budget
      (+ min-p
         (rand-int (int (- max-p min-p)))))))

(defn generate-users
  [n products]
  (mapv (fn [id]
          (generate-user id products))
        (range 1 (inc n))))
