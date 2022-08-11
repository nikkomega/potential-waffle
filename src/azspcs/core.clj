(ns azspcs.core
  (:require [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [quil.core :as q]))

(def best-file "best.edn")
(def submitted-file "submitted.edn")

(def best (atom (edn/read-string (slurp best-file))))
(def submitted (atom (edn/read-string (slurp submitted-file))))

(def current-grid (atom {}))
(def current-halo (atom {}))

(defn score [n]
  (apply max (vals (@best n))))

(defn format-line [grid y x1 x2]
  (as->
    (for [x (range x1 x2)] (get grid [x y] 0)) line
    (partition-by #(= % 0) line)
    (if (= (first (first line)) 0) line (conj line '()))
    (partition 2 line)
    (map #(str (count (first %)) ": " (str/join ", " (second %))) line)
    (str/join " / " line)
    (str "(" line ")")))

(defn format-submission [grid]
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
  (println (format-submission grid)))

(defn submit []
  (let [newscores (into {} (for [n (range 7 32)]
                             [n (apply max (vals (@best n)))]))
        oldscores @submitted]
    (println
      (str/join ";"
                (for [[n grid] @best :when (> (newscores n) (oldscores n))]
                  (format-submission grid))))
    (swap! submitted (constantly newscores))
    (spit submitted-file (pr-str newscores))))

(defn update-best [n candidate]
  (if (and (>= n 7) (<= n 31))
    (let [best-n (@best n)]
      (if (or (nil? best-n)
              (< (count best-n) (count candidate)))
        (do
          (swap! best assoc n candidate)
          (spit best-file (pr-str @best))
          (println (str "Improved n=" n " from "
                        (apply max (vals best-n)) " to " (apply max (vals candidate))))
          (print-grid candidate))))))

(defn neighbors [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))

(defn openings [grid]
  (let [coords (set (keys grid))]
    (set/difference
      (reduce set/union #{} (map (comp set neighbors) coords))
      coords)))

(defn neighbor-sum [grid p]
  (reduce + (map #(get grid % 0) (neighbors p))))

(defn no-neighbors [pointset p]
  (not (some #(contains? pointset %) (neighbors p))))

(defn genhalo [grid]
  (into {} (map #(vector % (neighbor-sum grid %)) (openings grid))))

(defn unwind [grid k]
  (let [cull (apply max-key #(grid %) (keys grid))
        grid (dissoc grid cull)
        orphaned (filter (partial no-neighbors (set (keys grid))) (keys grid))
        grid (apply dissoc grid orphaned)]
    (if (> k 1)
      (unwind grid (dec k))
      grid)))

;; STRATEGIES
(def search-breadth 1)

(defn maximize [n grid halo p m timeout]
  (let [grid (assoc grid p m)
        halo (into (dissoc halo p)
                   (for [p' (neighbors p)
                         :let [pval (neighbor-sum grid p')]
                         :when (not (contains? (set (keys grid)) p'))]
                     [p' pval]))
        choices (take search-breadth (map first (filter (fn [[_ v]] (= v (inc m))) halo)))
        new-grids (map #(maximize n grid halo % (inc m) timeout) choices)]
    (swap! current-grid (constantly grid))
    (swap! current-halo (constantly halo))
    (if (or (empty? choices) (> (System/currentTimeMillis) timeout))
      (do
        (update-best n grid)
        grid)
      (apply max-key count new-grids))))

(defn build [n grid halo p pval m]
  (let [grid (assoc grid p pval)
        halo (into (dissoc halo p)
                   (for [p' (neighbors p)
                         :let [pval (neighbor-sum grid p')]
                         :when (not (contains? (set (keys grid)) p'))]
                     [p' pval]))
        search-breadth (if (> m (- (score n) 20)) 10 1)
        choices (->> halo
                     (filter (fn [[_ v]] (= v (inc m))))
                     (map first)
                     shuffle
                     (take search-breadth))
        new-grids (map #(build n grid halo % (inc m) (inc m)) choices)
        huts (count (filter (fn [[_ v]] (= v 1)) grid))
        hutchoices (->>
                     halo
                     (filter (fn [[_ v]] (= v m)))
                     (map first)
                     (mapcat neighbors)
                     (filter (partial no-neighbors (set (keys grid))))
                     shuffle
                     (take search-breadth))
        hutgrids (map #(build n grid halo % 1 m) hutchoices)]
    (swap! current-grid (constantly grid))
    (swap! current-halo (constantly halo))
    (cond
      (and (> pval 1) (> 0.2 (rand)) (seq hutchoices) (< huts n)) (apply max-key count hutgrids)
      (seq choices) (apply max-key count new-grids)
      (and (> pval 1) (seq hutchoices) (< huts n)) (apply max-key count hutgrids)
      :else (do
              (update-best (count (filter (fn [[_ v]] (= v 1)) grid)) grid)
              grid))))

;; STARTS
(def cornerstart {[-1 -1] 1 [1 1] 1})

(defn twostart []
  (into {} (map #(vector % 1) (take 2 (shuffle (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [dx dy]))))))

(defn randstart [n]
  (let [xsize 10
        ysize 6
        points (for [x (range (- xsize) xsize) :when (not (contains? #{-1 0 1} x))
                     y (range (- ysize) ysize) :when (not (contains? #{-1 0 1} y))]
                 [x y])]
    (into cornerstart (map #(vector % 1) (take (- n 2) (shuffle points))))))

(def two-offsets
  [[-2 -2] [-2 -1] [-2 0] [-2 1] [-2 2]
   [-1 -2] [-1 2]
   [0 -2] [0 2]
   [1 -2] [1 2]
   [2 -2] [2 -1] [2 0] [2 1] [2 2]])

(def two-corners
  [[-2 -2] [-2 2] [2 -2] [2 2]])

(defn spreadstart [n grid]
  (if (> n 1)
    (let [parents (shuffle (keys grid))
          offsets (shuffle two-corners)
          children (for [parent parents
                         offset offsets
                         :let [child (map + parent offset)]
                         :when (not (contains? (set (keys grid)) child))
                         :when (not (contains? (set (mapcat neighbors (keys grid))) child))]
                     child)]
      (spreadstart (dec n) (assoc grid (first children) 1)))
    grid))

;; SEARCHES
(defn buildsearch [n]
  (let [grid cornerstart
        halo (into {} (map #(vector % (neighbor-sum grid %)) (openings grid)))
        answer (build n grid halo [0 0] 2 2)]))

(defn buildloop [n]
  (doseq [_ (range)]
    (buildsearch n)))

(defn maxsearch [n]
  (let [grid (spreadstart n {[0 0] 1})
        halo (into {} (map #(vector % (neighbor-sum grid %)) (openings grid)))
        answer (maximize n grid halo [0 0] 2 (+ (System/currentTimeMillis) 1000))]))

(defn maxloop [n]
  (doseq [_ (range)]
    (maxsearch n)))

(defn unwindsearch [n k]
  (let [m (- (score n) k)
        grid (unwind (@best n) k)
        halo (genhalo grid)]
    (doseq [_ (range)]
      (build n grid halo [0 0] 2 m))))


;; VISUALIZATION
(def gridsize 60)

(defn show [grid]
  (do (swap! current-grid (constantly grid))
      (swap! current-halo (constantly {}))
      nil))

(defn showbest [n]
  (show (@best n)))

(defn setup []
  (q/frame-rate 10)
  (q/background 255)
  (let [font (q/create-font "Arial" 12)]
    (q/text-font font)))

(defn draw []
  (q/fill 255)
  (q/rect 0 0 (q/width) (q/height))
  (let [size (quot (q/width) gridsize)
        grid @current-grid
        halo @current-halo]
    (doseq [[[x y] n] grid]
      (let [x (* size (+ x (/ gridsize 2)))
            y (* size (+ y (/ gridsize 2)))]
        (q/fill 220)
        (q/stroke 0)
        (q/rect x y size size)
        (if (= n 1)
          (q/fill 255 64 64)
          (q/fill 0))
        (q/text (str n) x (+ y size))))
    (doseq [[[x y] n] halo]
      (let [x (* size (+ x (/ gridsize 2)))
            y (* size (+ y (/ gridsize 2)))]
        (q/fill 0 0 128)
        (q/stroke 255)
        (q/rect x y size size)
        (q/fill 255)
        (q/text (str n) x (+ y size))))))

(q/defsketch stonegui
             :title "Stepping Stones"
             :settings #(q/smooth 2)
             :setup setup
             :draw draw
             :size [1200 1200])
