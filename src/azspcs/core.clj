(ns azspcs.core
  (:require [clojure.set :as set]))

(defn neighbors [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))

(def g1 {[0 0] 1 [2 -2] 1})

(defn openings [grid]
  (set/difference
    (reduce set/union #{} (map (comp set neighbors) (set (keys grid))))
    (set (keys grid))))

(defn neighbor-sum [grid p]
  (reduce + (map #(get grid % 0) (neighbors p))))

(defn choices [grid n]
  (filter
    #(= n (neighbor-sum grid %))
    (openings grid)))

(defn maximize [grid n]
  (let [choices (choices grid n)
        new-grids (map #(maximize (conj grid [% n]) (inc n)) choices)]
    (if (empty? choices)
      grid
      (apply max-key count new-grids))))

(defn randstart [n]
  (let [size (inc n)
        points (for [x (range (- size) size)
                     y (range (- size) size)]
                 [x y])]
    (into {} (map #(vector % 1) (take n (shuffle points))))))

(defn search [n t]
  (apply max-key count (take t (repeatedly #(maximize (randstart n) 2)))))

(defn format-line [grid y x1 x2]
  (as->
    (for [x (range x1 x2)] (get grid [x y] 0)) line
    (partition-by #(= % 0) line)
    (if (= (first (first line)) 0) line (conj line '()))
    (partition 1000 2 line)))

(defn submit [grid]
  (let [y1 (min-key (comp second key) grid)
        y2 (max-key (comp inc second key) grid)
        x1 (min-key (comp first key) grid)
        x2 (max-key (comp inc first key) grid)]
    (for [y (range y1 y2)]
      (format-line grid y x1 x2))))