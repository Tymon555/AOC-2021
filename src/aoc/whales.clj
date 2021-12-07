(ns aoc.whales
  (:require [clojure.string :as str]))

(def input (->>
            (slurp "src/aoc/whales_input.txt")
            (str/trim)
            (#(str/split % #","))
            (map #(Integer/parseInt %))))

(def median
  (->> input
       (sort)
       (#(nth % (quot (count input) 2)))))

(time
 (->> input
      (map (fn [x] (Math/abs (- x median))))
      (reduce +)
      )

 )

(def max_element (apply max input))

(def cached_dist
  (memoize
   (fn[x]
     (/ (* x (inc x)) 2))))


(defn total_cost_for_position
  [pos]
    (->> input
         (map (fn [x] (cached_dist(Math/abs (- x pos)))))
         (reduce +)))

(def full_table
 (->> max_element
      range
      (map #(total_cost_for_position %)))
 )
;; (apply min)
     ;; (println)
     ;; (time))
