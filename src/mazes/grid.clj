(ns mazes.grid
  (:require
   [clojure.string :as str]))

(def directions [:north :west :south :east])

(def coords
  (zipmap directions [[0 1]
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

(defn make-cell [column row]
  "Creates cell using row and column positions"
  {:row row :column column :links #{}})

(defn init [rows cols]
  "initialise a vector of length cols * rows, calling (make-cell x y) for each element"
  {:rows rows
   :cols cols
   :cells (reduce (fn [grid [x y]] (assoc grid (grid-key x y) (make-cell x y)))
          {}
          (all-coords-for rows cols))})

(defn get-cell [grid x y]
  "Return the cell at x,y in grid"
  (get-in grid [:cells (grid-key x y)]))

(defn iter-grid [grid]
  "ITerate through grid by row then column, returning each cell"
  (vec (for [x (range (:cols grid)) y (range (:rows grid))] (get-cell grid x y))))

(defn iter-row [grid y]
  (vec (for [x (range (:cols grid))] (get-cell grid x y))))

(defn iter-col [grid x]
  (vec (for [y (range (:rows grid))] (get-cell grid x y))))

(defn iter-rows [grid]
  "Create a vector of grid rows"
  (vec (for [y (range (:rows grid))] (iter-row grid y))))

(defn direction-from-cell [cell direction]
  "get coordinate of direction from a given cell"
  (let [[dx dy] (get coords direction)]
    [(+ dx (:column cell)) (+ dy (:row cell))]))

(defn cell-has-neighbour? [grid cell direction]
  (let [[x y] (direction-from-cell cell direction)]
    (not (nil? (get-cell grid x y)))))

(defn cell-has-link? [cell direction]
  (contains? (:links cell) direction))

(defn cell-at-dir [grid cell direction]
  "Get the cell at direction"
  (let [[x y] (direction-from-cell cell direction)]
    (get-cell grid x y)))

(defn direction-between [from to]
  "find the direction between two cells"
  (let [dy (- (:row to) (:row from))
        dx (- (:column to) (:column from))]
    (-> coords
        (select-keys (for [[k [x y]] coords :when (and (= x dx) (= y dy))] k))
        keys
        first)))

(defn update-link [grid cell dir]
  "Add dir to cell links"
  (update-in grid [:cells (grid-key cell) :links] #(conj % dir)))

(defn link-cells
  "Record a link between two cells"
  ([grid, src, dest] (link-cells grid src dest true))
  ([grid, src, dest, bidirectional]
   (let [direction (direction-between src dest)
         reverse (direction-between dest src)]
     (cond-> grid
       (some? direction) (update-link src direction)
       (and bidirectional (some? reverse)) (update-link dest reverse)))))

(defn get-neighbour [grid cell directions]
  "Get all cells neighbouring cell at specified directions"
  (reduce (fn [neighbours dir] (if (cell-has-neighbour? grid cell dir)
                                 (conj neighbours (cell-at-dir grid cell dir))
                                 neighbours))
          []
          directions))

(defn get-link-cell [grid cell directions]
  "Get all linked cell at specified directions"
  (reduce (fn [links dir] (if (cell-has-link? cell dir)
                                 (conj links (cell-at-dir grid cell dir))
                                 links))
          []
          directions))

(defn get-linked-cells [grid cell]
  (get-link-cell grid cell [:north :east :south :west]))

