(ns aoc.lanternfish)

(defn step [current]
  (let [_next_old (map (fn [x] (if (> x 0) (dec x) 6)) current)
        _new (->> current (filter (fn [x] (= x 0))) (map (fn [x] 8)))]

    (concat _next_old _new)))

(defn generate_history
  "assuming a internal timer 0 lanternfish, see the state of it's descendants on day X in history[X]
  takes count of days do generate"
  [count]
  (loop [i 0 history [] current [0]]
    (if (>= i count)
      (conj history current)
      (recur
       (inc i) (conj history current) (step current)))))

(def history80 (generate_history 80))

(defn count_lanternfish_descendants [fish]
  ;align to 0
  (let [_aligned_fish (- 80 fish)]
    (count (nth history80 _aligned_fish))))

(def precomputed_descendants (reduce (fn [acc curr] (assoc acc curr (count_lanternfish_descendants curr))) {} (range 9)))

(def input_ints (->>
                 (clojure.string/split (slurp "src/aoc/lanternfish_input.txt") #",")
                 (map (fn [x] (bigint (Long/parseLong (clojure.string/trim x)))))))

(reduce + (map (fn [x] (get precomputed_descendants x)) input_ints))

;; faster solution for part 2
;;
(def initial_counts (for [i (range 10)]
                      (count (filter (partial = i) input_ints))))

(defn simulate_day [counts]
  (let [born (first counts)]
    (->> (conj (into [] (rest counts)) (bigint 0))
         (map-indexed (fn [idx x] (if (contains? #{6 8} idx) (+ x born) x))))))

(->> initial_counts (simulate_day))
(->> initial_counts (simulate_day) (simulate_day))

(defn simulate [_counts _days]
  (loop [counts _counts days _days]
    (if (= days 0)
      counts
      (recur
       (simulate_day counts) (dec days)))))

(->> (simulate initial_counts 256)
     (reduce +) (time))

(->> (iterate (partial concat [2]) [1]) (#(nth % 4)))
