(ns mazes.core
  (:require [clojure.string :as str]))

(def *directions* [:north :west :south :east])

(def *coords*
  (zipmap *directions* [[0 1]
                       [-1 0]
                       [0 -1]
                       [1 0]]))

(defn all-coords-for [rows cols]
  "Generate every coordinate for grid of size rows by cols"
  (for [x (range cols) y (range rows)] [x y]))

(defn grid-key
  "Create the key to look up a cell in grid"
  ([x y] (str/join "," [x y]))
  ([cell] (grid-key (:column cell) (:row cell))))

(defn make-cell [row column]
  "Creates cell using row and column positions"
  {:row row :column column :links #{}})

(defn init-grid [rows cols]
  "initialise a vector of length cols * rows, calling (make-cell x y) for each element"
  {:rows rows
   :cols cols
   :cells (reduce (fn [grid [x y]] (assoc grid (grid-key x y) (make-cell y x)))
          {}
          (all-coords-for rows cols))})

(defn get-cell [grid x y]
  "Return the cell at x,y in grid"
  (get-in grid [:cells (grid-key x y)]))

(defn iter-grid [grid]
  "ITerate through grid by row then column, calling f at each cell"
  (vec (for [x (range (:cols grid)) y (range (:rows grid))] (get-cell grid x y))))

(defn iter-rows [grid]
  "Create a vector of grid rows"
  (vec (for [y (range (:rows grid))] (get-row grid y))))

(defn get-row [grid y]
  (vec (for [x (range (:cols grid))] (get-cell grid x y))))

(defn get-col [grid x]
  (vec (for [y (range (:rows grid))] (get-cell grid x y))))

(defn direction-from-cell [cell direction]
  "get coordinate of direction from a given cell"
  (let [[dx dy] (get *coords* direction)]
    [(+ dx (:column cell)) (+ dy (:row cell))]))

(defn cell-has-neighbour [grid cell direction]
  (let [[x y] (direction-from-cell cell direction)]
    (not (nil? (get-cell grid x y)))))

(defn cell-at-dir [grid cell direction]
  "Get the cell at direction"
  (let [[x y] (direction-from-cell cell direction)]
    (get-cell grid x y)))

(defn get-direction [from to]
  "find the direction between two cells"
  (let [dy (- (:row to) (:row from))
        dx (- (:column to) (:column from))]
    (first (map first
      (filter (fn [[k, [x, y]]] (and (= x dx) (= y dy))) *coords*)))))

(defn update-link [grid cell dir]
  "Add dir to cell links"
  (update-in grid [:cells (grid-key cell) :links] #(conj % dir)))

(defn link-cells
  "Record a link between two cells"
  ([grid, src, dest] (link-cells grid src dest true))
  ([grid, src, dest, bidirectional]
   (let [direction (get-direction src dest)
         reverse (get-direction dest src)]
     (cond-> grid
       (not (nil? direction)) (update-link src direction)
       (and bidirectional (not (nil? reverse))) (update-link dest reverse)))))

(defn get-neighbours [grid cell directions]
  "Get all cells neighbouring cell at specified directions"
  (reduce (fn [neighbours dir] (if (cell-has-neighbour grid cell dir)
                                 (conj neighbours (cell-at-dir grid cell dir))
                                 neighbours))
          []
          directions))

(defn str-row-upper [row]
  (str "|"
       (str (str/join
             ""
             (map (fn [cell] (str "   " (if (contains? (:links cell) :east) " " "|")))
                  row)))
       "\n"))

(defn str-row-lower [row]
  (str "+"
       (str (str/join
             ""
             (map (fn [cell] (str (if (contains? (:links cell) :south) "   " "---") "+"))
                  row)))
       "\n"))

(defn str-row [row]
  (str (str-row-upper row) (str-row-lower row)))

(defn str-grid [grid]
  (str "+" (apply str (repeat (:cols grid) "---+")) "\n"
       (str (str/join "" (map (fn [x] (str-row (get-row grid x))) (reverse (range (:rows grid))))))))

(defn binary-tree [grid]
  (reduce (fn [grid cell]
            (let [neighbours (get-neighbours grid cell '(:north :east))]
              (if (not (empty? neighbours))
                (link-cells grid cell (rand-nth neighbours))
                grid)))
          grid
          (iter-grid grid)))

(defn should-close-out [grid cell]
  (or (not (cell-has-neighbour grid cell :east))
      (and (cell-has-neighbour grid cell :north)
           (even? (rand-nth '(0 1))))))

(defn generate-runs [grid row]
  (partition-by #(should-close-out grid %) row))

(defn link-random-north [grid run]
  (let [rnd (rand-nth run)]
    (if (cell-has-neighbour grid rnd :north)
      (link-cells grid rnd (cell-at-dir grid rnd :north))
      grid)))

(defn link-east [grid run]
  (reduce (fn [grid [fst snd]] (link-cells grid fst snd))
          grid
          (partition 2 1 run)))

(defn connect-run [grid run]
  (-> grid
      (link-random-north run)
      (link-east run)))

(defn sidewinder [grid]
  (let [runs (mapcat #(generate-runs grid %) (iter-rows grid))]
    (reduce connect-run grid runs)))
