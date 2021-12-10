(ns aoc.syntax-scoring
  (:require [clojure.string :as str]))

(def matching_braces
  {\[ \] \{ \} \( \) \< \>})

(def opening_braces (set (keys matching_braces))
  )
(print opening_braces)
(print matching_braces)
(contains? opening_braces \[)

(defn check_char [[stack mismatch] char]
  (do
    (if mismatch [stack mismatch]
    (if (contains? opening_braces char)
      [(conj stack char) nil]
      (if (not= (get matching_braces (peek stack)) char)
        [stack char]
        [(pop stack) nil])))))

(defn check_line [line]
  (let [stack '()]
    (->>
     line
     seq
     (reduce (fn [acc curr] (check_char acc curr)) [stack nil]))))

(def points_map
  {\) 3 \] 57 \} 1197 \> 25137})

(->> "src/aoc/syntax_scoring_input.txt"
     slurp
     str/split-lines
     (map #(check_line %))
     (filter #(some? (second %)))
     (map (partial second))
     (map #(get points_map %))
     (reduce +))

(defn complete_braces [to_complete]
  (map (fn[x] (get matching_braces x)) (seq to_complete)))

(def brace_score
  { \) 1 \] 2 \} 3  \> 4})

(defn compute_score [completion]
  (reduce (fn[acc curr] (+ (get brace_score curr) (* 5 acc))) 0 completion))

(->> "src/aoc/syntax_scoring_input.txt"
     slurp
     str/split-lines
     (map #(check_line %))
     (filter #(not (some? (second %))))
     (map (partial first))
     (map (partial complete_braces))
     (map (partial compute_score))
     (sort)
     (#(nth % (quot (count %) 2)))
     (time))
