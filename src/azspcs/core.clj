(ns azspcs.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def best (atom 0))

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

(defn print-grid [grid]
  (println grid)
  (println (submit grid))
  (println (count grid)))

(defn maximize [grid halo p n]
  (let [grid (assoc grid p n)
        halo (into (dissoc halo p)
                   (for [p' (neighbors p)
                         :let [pval (neighbor-sum grid p')]
                         :when (not (contains? (set (keys grid)) p'))]
                     [p' pval]))
        choices (map first (filter (fn [[k v]] (= v (inc n))) halo))
        new-grids (map #(maximize grid halo % (inc n)) choices)]
    (if (empty? choices)
      (do
        (if (> (count grid) @best)
          (do
            (swap! best (constantly (count grid)))
            (print-grid grid)))
        grid)
      (apply max-key count new-grids))))

(def corners {[-1 -1] 1 [1 1] 1})

(defn randstart [n]
  (let [xsize 10
        ysize 6
        points (for [x (range (- xsize) xsize) :when (not (contains? #{-1 0 1} x))
                     y (range (- ysize) ysize) :when (not (contains? #{-1 0 1} y))]
                 [x y])]
    (into corners (map #(vector % 1) (take (- n 2) (shuffle points))))))

(defn search [n]
  (swap! best (constantly 0))
  (let [grid (randstart n)
        halo (into {} (map #(vector % (neighbor-sum grid %)) (openings grid)))
        answer (maximize grid halo [0 0] 2)]
    (print-grid answer)))
