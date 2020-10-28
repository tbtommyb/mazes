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

(defn iter-grid [grid]
  "ITerate through grid by row then column, calling f at each cell"
  (vec (for [x (range (:rows grid)) y (range (:cols grid))] (get-cell grid x y))))

(defn make-cell [row column]
  "Creates cell using row and column positions"
  {:row row :column column :links #{}})

(defn get-cell [grid x y]
  "Return the cell at x,y in grid"
  (get-in grid [:cells (grid-key x y)]))

(defn get-row [grid x]
  (vec (for [y (range (:cols grid))] (get-cell grid x y))))

(defn get-col [grid y]
  (vec (for [x (range (:rows grid))] (get-cell grid x y))))

(defn direction-from-cell [cell direction]
  "get coordinate of direction from a given cell"
  (let [[dx dy] (get *coords* direction)]
    [(+ dx (:row cell)) (+ dy (:column cell))]))

(defn cell-has-neighbour [grid cell direction]
  (let [[x y] (direction-from-cell cell direction)]
    (not (nil? (get-cell grid x y)))))

(defn cell-at-dir [grid cell direction]
  "Get the cell at direction"
  (let [[x y] (direction-from-cell cell direction)]
    (get-cell grid x y)))

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

(defn get-neighbours [grid cell directions]
  "Get all cells neighbouring cell at specified directions"
  (reduce (fn [neighbours dir] (if (cell-has-neighbour grid cell dir)
                                 (conj neighbours (cell-at-dir grid cell dir))
                                 neighbours))
          []
          directions))

(defn binary-tree [grid]
  (reduce (fn [grid cell]
            (let [neighbours (get-neighbours grid cell '(:north :east))]
              (if (not (empty? neighbours))
                (link-cells grid cell (rand-nth neighbours))
                grid)))
          grid
          (iter-grid grid)))

(defn str-row [row]
  (str (str-row-upper row) (str-row-lower row)))

(defn str-grid [grid]
  (str "+" (apply str (repeat (:cols grid) "---+")) "\n"
       (str (clojure.string/join "" (map (fn [x] (str-row (get-row grid x))) (range (:rows grid)))))))
       ;; (map (fn [r]
       ;;        (clojure.string/join "" '((str-row-upper (get-row grid r)) (str-row-lower (get-row grid r)))))
       ;;      (range (:rows grid)))))

(defn str-row-upper [row]
  (str "|"
       (str (clojure.string/join "" (map (fn [cell] (str "   " (if (contains? (:links cell) :east) " " "|")))
                                         row)))
       "\n"))

(defn str-row-lower [row]
  (str "+"
       (str (clojure.string/join "" (map (fn [cell]
                                           (str (if (contains? (:links cell) :south) "   " "---") "+"))
                                         row)))
       "\n"))
