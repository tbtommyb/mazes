(ns mazes.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def *directions* [:north :west :south :east])

(def *coords*
  (zipmap *directions* [[0 1]
                       [-1 0]
                       [0 -1]
                       [1 0]]))

(defn all-coords-for [rows cols]
  "Generate every coordinate for grid of size rows by cols"
  (for [x (range rows) y (range cols)] [x y]))

(defn init-grid [rows cols f]
  "initialise a vector of length cols * rows, calling (f x y) for each element"
  {:rows rows
   :cols cols
   :cells (reduce (fn [grid [x y]] (assoc grid (grid-key x y) (f x y)))
          {}
          (all-coords-for rows cols))})

(defn grid-key
  "Create the key to look up a cell in grid"
  ([x y] (clojure.string/join "," [x y]))
  ([cell] (grid-key (:row cell) (:column cell))))

(defn iter-grid [grid f]
  "ITerate through grid by row then column, calling f at each cell"
  (for [x (range (:rows grid)) y (range (:cols grid))] (f (get-cell grid x y))))

(defn make-cell [row column]
  "Creates cell using row and column positions"
  {:row row :column column :links #{}})

(defn get-cell [grid x y]
  "Return the cell at x,y in grid"
  (get-in grid [:cells (grid-key x y)]))

(defn direction-from-cell [cell direction]
  "get coordinate of direction from a given cell"
  (let [[dx dy] (get *coords* direction)]
    [(+ dx (:row cell)) (+ dy (:column cell))]))

(defn get-direction [from to]
  "find the direction between two cells"
  (let [dx (- (:row to) (:row from))
        dy (- (:column to) (:column from))]
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
       (and bidirectional (not (nil? direction))) (update-link dest reverse)))))
