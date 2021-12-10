(ns aoc.segment-search
  (:require [clojure.string :as str]))
;; (:require [clojure.string :as str])
(defn trim_split_whitespace [x]
  (->> x
       (str/trim)
       (#(str/split % #" "))))

(defn split_line [line]
  [(trim_split_whitespace (first line)) (trim_split_whitespace (second line))])

(def input_lines
  (->>
   (slurp "src/aoc/segment_search_input.txt")
   (#(str/split % #"\n"))
   (map #(str/split % #"\|"))
   (map #(split_line %))))

(defn map_count_easy [line]
  (->>
   (map (fn [x]
          (->>
           x
           (count))) line)
   (filter (fn [letter_count] (contains? #{2 3 4 7} letter_count)))))

(->> input_lines
     (map #(second %))
     (map #(map_count_easy %))
     (flatten)
     (count)
     (println))

(def segment_counts {:top 8 :left-top 6 :right-top 8 :mid 7 :left-bot 4 :right-bot 9 :bot 7})
(def segments_by_count (clojure.set/map-invert segment_counts))

(defn remove-simple [digit_strs]
  (->>
   digit_strs
   (filter #(not (contains? #{2 3 4 7} (count %))))))
(remove-simple (first (first input_lines)))

(defn top_segment [line]
  (->>
   line
   (first)
   (filter #(contains? #{2 3} (count %)))
   (sort #(> (count %1) (count %2)))
   (map set)
   (apply clojure.set/difference)
   (first)
   (str)))

(defn counts_wo_4 [line]
  (->>
   line
   (first)
   (filter #(not (contains? #{4} (count %))))
   (reduce concat)
   (reduce (fn [acc curr] (update acc (str curr) inc)) {"a" 0 "b" 0 "c" 0 "d" 0 "e" 0 "f" 0 "g" 0})
 ;; (apply clojure.set/intersection)
   ))

(defn letter_counts [line]
  (->>
   line
   (first)
   (str/join "")
   (reduce (fn [acc curr] (update acc (str curr) inc)) {"a" 0 "b" 0 "c" 0 "d" 0 "e" 0 "f" 0 "g" 0})))

(defn _partial_char_segment_map [line]
  (->>
   line
   (letter_counts)
   (#(dissoc % top_segment))
   (reduce (fn [acc [key value]] (assoc acc key (get segments_by_count value))) {})))

(defn middle_bot_candidate_chars [line]
  (->>
   line
   (_partial_char_segment_map)
   (filter (fn [[key value]] (= value :bot)))
   (mapv #(first %))))

(defn middle_segment [line]
  (if (<
       (get (counts_wo_4 line) (first (middle_bot_candidate_chars line)))
       (get (counts_wo_4 line) (second (middle_bot_candidate_chars line))))
   ;; (first is bot)
    (first (middle_bot_candidate_chars line))
    (second (middle_bot_candidate_chars line))))

(defn full_char_segment_map [line]
  (->>
   line
   (_partial_char_segment_map)
   (#(update % (middle_segment line)  (constantly :mid)))
   (#(update % (top_segment line) (constantly :top)))))

(def all_segments #{:top :right-top :right-bot :bot :left-bot :left-top :mid})

(def segmentset_to_letter
  {(clojure.set/difference all_segments #{:mid}) 0
   #{:right-bot :right-top} 1
   (clojure.set/difference all_segments #{:right-bot :left-top}) 2
   (clojure.set/difference all_segments #{:left-bot :left-top}) 3
   #{:right-bot :right-top :mid :left-top} 4
   (clojure.set/difference all_segments #{:left-bot :right-top}) 5
   (clojure.set/difference all_segments #{:right-top}) 6
   #{:top :right-top :right-bot} 7
   all_segments 8
   (clojure.set/difference all_segments #{:left-bot}) 9})

(full_char_segment_map (first input_lines))

(first input_lines)
(->> input_lines
     (map #(let [char_segment_map_for_line (full_char_segment_map %)]
             (->>
              (second %)
              (map (fn [x] (->>
                            x
                            (set)
                            (map (fn [char] (get char_segment_map_for_line (str char))))
                            (set)
                            (get segmentset_to_letter))))
              (str/join "")
              (Integer/parseInt))))
     (reduce +))
