(ns aoc.smoke-basin
  (:require [clojure.string :as str]))

(def input
  (->>
   (str/split (slurp "src/aoc/smoke_basin_input.txt") #"\n")
   (map #(->>
          (str/split % #"")
          (map (fn [d] (Integer/parseInt d)))))))

(def padded_input
  (->>
   input
   (map (fn [row] (conj (vec (conj row Integer/MAX_VALUE)) Integer/MAX_VALUE)))
  ;; (cons (repeat (count (first input)) Integer/MAX_VALUE))
   (#(conj % (repeat (inc (inc (count  (first input)))) Integer/MAX_VALUE)))
   (vec)
   (#(conj % (repeat (inc (inc (count  (first input)))) Integer/MAX_VALUE)))))

(def visited
  (vec (for [x (range (count padded_input))]
         (->>
          (vec (for [y (range (count (first padded_input)))]
      ;; (get_point x y)))
                 false))))))

(defn get_point [x y]
  (->>
   padded_input
   (#(nth % x))
   (#(nth % y))))

(defn is_low_point? [x y]
  (let [point (get_point x y)]
    (if (and
         (< point (get_point (dec x) y))
         (< point (get_point (inc x) y))
         (< point (get_point x (dec y)))
         (< point (get_point x (inc y))))
      true false)))

(->>
 (for [x (range (count input))]
   (->>
    (for [y (range (count (first input)))]
      ;; (get_point x y)))
      (->>
       (if (is_low_point? (inc x) (inc y)) (inc (get_point (inc x) (inc y))) 0)))))
 (flatten)
 (reduce +))

(defn get_basin_size [curr visited q]
  (if (empty? q)
    curr
    (let [x (first (first q)) y (second (first q)) q (rest q) curr_visited (get-in visited [x y]) point (get_point x y)]
      (let [_q
            (->>
             q
             (#(if (and (< point 9) (not (get-in visited [(dec x) y]))) (conj % [(dec x) y]) %))
             (#(if (and (< point 9) (not (get-in visited [(inc x) y]))) (conj % [(inc x) y]) %))
             (#(if (and (< point 9) (not (get-in visited [x (inc y)]))) (conj % [x (inc y)]) %))
             (#(if (and (< point 9) (not (get-in visited [x (dec y)]))) (conj % [x (dec y)]) %)))
            _visited (update-in visited [x y] (constantly true))]
        (let [_plusone (if (or curr_visited (>= point 9)) 0 1)]
          (recur (+ curr _plusone) _visited _q))))))

(get_point 1 10)
(get-in visited [1 2])
visited
(update-in visited [0 0] (constantly true))

(get_basin_size 0 visited [[1 1]])

(->>
 (for [x (range (count input))]
   (->>
    (for [y (range (count (first input)))]
      ;; (get_point x y)))
      (->>
       (if (is_low_point? (inc x) (inc y)) (get_basin_size 0 visited [[(inc x) (inc y)]]) 0)))))
 (flatten)
 (sort >)
 (take 3)
 (reduce *))
