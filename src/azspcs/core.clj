(ns azspcs.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn format-line [grid y x1 x2]
  (as->
    (for [x (range x1 x2)] (get grid [x y] 0)) line
    (partition-by #(= % 0) line)
    (if (= (first (first line)) 0) line (conj line '()))
    (partition 2 line)
    (map #(str (count (first %)) ": " (str/join ", " (second %))) line)
    (str/join " / " line)
    (str "(" line ")")))

(defn submit [grid]
  (let [coords (keys grid)
        y1 (apply min (map second coords))
        y2 (inc (apply max (map second coords)))
        x1 (apply min (map first coords))
        x2 (inc (apply max (map first coords)))]
    (str/join ", "
              (for [y (range y1 y2)]
                (format-line grid y x1 x2)))))

(defn neighbors [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))

(defn openings [grid]
  (let [coords (set (keys grid))]
    (set/difference
      (reduce set/union #{} (map (comp set neighbors) coords))
      coords)))

(defn neighbor-sum [grid p]
  (reduce + (map #(get grid % 0) (neighbors p))))

(defn choices [grid n]
  (filter
    #(= n (neighbor-sum grid %))
    (openings grid)))

(defn maximize [grid n]
  (let [best (atom 0)
        maximize' (fn maximize' [grid n]
                    (let [choices (choices grid n)
                          new-grids (map #(maximize' (conj grid [% n]) (inc n)) choices)]
                      (if (empty? choices)
                        (if (> n @best)
                          (do
                            (println grid)
                            (println (submit grid))
                            (println n)
                            (swap! best (constantly (count grid)))
                            grid)
                          grid)
                        (apply max-key count new-grids))))]
    (maximize' grid n)))

(defn randstart [n]
  (let [size 6
        points (for [x (range (- size) size)
                     y (range (- size) size)]
                 [x y])]
    (into {} (map #(vector % 1) (take n (shuffle points))))))

(defn search [n t]
  (let [solution
        (time
          (apply max-key count
                 (take t (repeatedly #(maximize (randstart n) 2)))))]
    (println solution)
    (println (submit solution))
    (print (- (inc (count solution)) n))
    nil))
