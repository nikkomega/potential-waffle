(ns azspcs.core
  (:require [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [quil.core :as q]))

(def best-file "best.edn")

(def best (atom (edn/read-string (slurp best-file))))

(def current-grid (atom {}))

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

(defn print-grid [grid]
  (println grid)
  (println (submit grid)))

(defn update-best [n candidate]
  (let [best-n (@best n)]
    (if (or (nil? best-n)
            (< (count best-n) (count candidate)))
      (do
        (swap! best assoc n candidate)
        (spit best-file (pr-str @best))
        (println (str "Improved n=" n " from " (count best-n) " to " (count candidate)))
        (print-grid candidate)))))

(defn neighbors [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))

(defn openings [grid]
  (let [coords (set (keys grid))]
    (set/difference
      (reduce set/union #{} (map (comp set neighbors) coords))
      coords)))

(defn neighbor-sum [grid p]
  (reduce + (map #(get grid % 0) (neighbors p))))

(defn maximize [n grid halo p m]
  (let [grid (assoc grid p m)
        halo (into (dissoc halo p)
                   (for [p' (neighbors p)
                         :let [pval (neighbor-sum grid p')]
                         :when (not (contains? (set (keys grid)) p'))]
                     [p' pval]))
        choices (map first (filter (fn [[_ v]] (= v (inc m))) halo))
        new-grids (map #(maximize n grid halo % (inc m)) choices)]
    (if (empty? choices)
      (do
        (update-best n grid)
        (swap! current-grid (constantly grid))
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
  (let [grid (randstart n)
        halo (into {} (map #(vector % (neighbor-sum grid %)) (openings grid)))
        answer (maximize n grid halo [0 0] 2)]))

(def gridsize 40)

(defn show [n]
  (do (swap! current-grid (constantly (@best n)))
      nil))

(defn setup []
  (q/frame-rate 10)
  (q/background 255)
  (let [font (q/create-font "Arial" 12)]
    (q/text-font font)))

(defn draw []
  (q/fill 255)
  (q/rect 0 0 (q/width) (q/height))
  (let [size (quot (q/width) gridsize)
        grid @current-grid]
    (doseq [[[x y] n] grid]
      (let [x (* size (+ x (/ gridsize 2)))
            y (* size (+ y (/ gridsize 2)))]
        (q/fill 220)
        (q/stroke 0)
        (q/rect x y size size)
        (if (= n 1)
          (q/fill 255 64 64)
          (q/fill 0))
        (q/text (str n) x (+ y size)))))
 )

(q/defsketch stonegui
             :title "Stepping Stones"
             :settings #(q/smooth 2)
             :setup setup
             :draw draw
             :size [600 600])
