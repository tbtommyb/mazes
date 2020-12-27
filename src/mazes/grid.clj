(ns mazes.grid
  (:require
   [mazes.cell :as cell]
   [mazes.specs :as spec]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

;; PROTOCOL STUFF
(defprotocol Grid
  "Grid represents the idea of a grid with accessible cells"
  (count-visible-cells [this]
    "Return the number of visible cells")
  (get-visible-cell [this coord]
    "Gets the cell located at `coord` if visible")
  (iter-visible-coords [this]
    "Iterate through every coordinate")
  (iter-visible-rows-coords [this]
    "Iterate through every row and return a vector of coords")
  (iter-visible-row-coords [this y]
    "Iterate through row `y` in `this`"))

;; Base Grid utilities
(defn all-coords-for
  "Generate every coordinate for grid of size `rows` by `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/coord-list %)]}
  (for [x (range cols) y (range rows)] [x y]))

(defn init-cells
  "Create a grid of size `rows` * `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/cells %)]}
   (reduce #(assoc %1 %2 (set '())) {} (all-coords-for rows cols)))

(defn get-cell
  "Return the cell located in `grid` at `coords`, ignoring visibility"
  [grid coords]
  {:pre [(s/valid? ::spec/coords coords)
         (s/valid? ::spec/grid? grid)]
   :post [(s/valid? (s/nilable ::spec/cell?) %)]}
  (when-let [links (get-in grid [:cells coords])]
    (cell/make-cell coords links)))

(defn iter-row-coords
  "Create a vector of every coord in row `y` of `grid`, ignoring visibility"
  [grid y]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? int? y)]
   :post [(s/valid? ::spec/coord-list %)]}
  (vec (for [x (range (:cols grid))] [x y])))

(defn iter-rows-coords
  "Create a vector of every coord by row in `grid`, ignoring visibility"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? (s/coll-of ::spec/coord-list) %)]}
  (seq (for [y (range (:rows grid))] (iter-row-coords grid y))))

(defn iter-rows-cells
  "Create a vector of every cell by row in `grid`, ignoring visibility"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? (s/coll-of ::spec/cell-list?) %)]}
  (seq (for [y (range (:rows grid))]
         (map (partial get-cell grid) (iter-row-coords grid y)))))

(defn iter-cells
  "Iterate through `grid` by column, returning each cell, ignoring visibility"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (map (partial get-cell grid) (all-coords-for (:rows grid) (:cols grid))))

(defn iter-coords
  "Iterate through `grid` by column, returning each coord, ignoring visibility"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/coord-list %)]}
  (all-coords-for (:rows grid) (:cols grid)))


;; Protocol-dependent functions
(defn iter-visible-cells
  "Iterate through `grid` by column, returning each visible cell"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (map (partial get-cell grid) (iter-visible-coords grid)))

(defn iter-visible-rows-cells
  "Create a seq of every visible cell by row in `grid`"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? (s/coll-of ::spec/cell-list?) %)]}
  (seq (for [y (range (:rows grid))]
         (map (partial get-cell grid) (iter-visible-row-coords grid y)))))

;; (defn iter-visible-rows-coords
;;   "Create a coord vector of every row in `grid`"
;;   [grid]
;;   {:pre [(s/valid? ::grid? grid)]
;;    :post [(s/valid? (s/coll-of ::coord-list) %)]}
;;   (vec (for [y (range (:rows grid))] (iter-visible-row-coords grid y))))

(declare add-direction-to-coords)
(defn visible-neighbour-coords
  "Find visible neighbour from `src` in `direction` in `grid`"
  [grid src direction]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords src)
         (s/valid? ::spec/direction? direction)]
   :post [(s/valid? (s/nilable ::spec/coords) %)]}
  (let [neighbour-coords (add-direction-to-coords src direction)]
    (when (get-visible-cell grid neighbour-coords)
      neighbour-coords)))

;; TODO: use a seq?
(defn get-cell-links
  "In `grid` get all cells linked to cell at `coords` in `directions`. Default all"
  ([grid coords] (get-cell-links grid coords spec/directions))
  ([grid coords dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/coords coords)
          (s/valid? (s/coll-of ::spec/direction?) dirs)]
    :post [(s/valid? ::spec/coord-list %)]}
   (reduce #(if (cell/cell-has-link? (get-visible-cell grid coords) %2)
              (conj %1 (visible-neighbour-coords grid coords %2))
              %1)
           []
           spec/directions)))

;; Direction stuff
(defn add-direction-to-coords
  "Add `direction` to `coords`"
  [coords direction]
  {:pre [(s/valid? ::spec/coords coords)
         (s/valid? ::spec/direction? direction)]}
  (let [[dx dy] (get cell/dir-step direction)]
    (vector (+ dx (first coords)) (+ dy (second coords)))))

(defn direction-between
  "Returns the direction as nilable symbol between `from` and `to`"
  [from to]
  {:pre [(s/valid? ::spec/coords from)
         (s/valid? ::spec/coords to)]
   :post [(s/valid? (s/nilable ::spec/direction?) %)]}
  (let [dx (- (first to) (first from))
        dy (- (second to) (second from))]
    (get cell/step-dir [dx dy])))

(defn link-cell-towards
  "Add a link from `coords` to `direction`"
  [grid coords direction]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords coords)
         (s/valid? ::spec/direction? direction)]
   :post [(s/valid? ::spec/grid? %)]}
  (update-in grid [:cells coords] #(conj % direction)))

(defn link-cells
  "Record a link in `grid` from `src` to `dest` if they are neighbours. Default bidirectional"
  ([grid, src, dest] (link-cells grid src dest true))
  ([grid, src, dest, bidirectional]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/coords src)
          (s/valid? ::spec/coords dest)
          (s/valid? boolean? bidirectional)]
    :post [(s/valid? ::spec/grid? grid)]}
   (let [direction (direction-between src dest)
         reverse (direction-between dest src)]
     (cond-> grid
       (some? direction) (link-cell-towards src direction)
       (and bidirectional (some? reverse)) (link-cell-towards dest reverse)))))

;; TODO: use a seq?
(defn get-neighbouring-coords
  "In `grid` get neighbours of cell at `coords` in `directions`"
  [grid coords directions]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords coords)
         (s/valid? (s/coll-of ::spec/direction?) directions)]
   :post [(s/valid? ::spec/coord-list %)]}
  (reduce #(if-let [neighbour (visible-neighbour-coords grid coords %2)]
             (conj %1 neighbour)
             %1)
          []
          directions))

(defn get-all-neighbouring-coords
  "In `grid` get all coords of cells neighbouring `coords`"
  [grid coords]
  (get-neighbouring-coords grid coords '(:north :east :south :west)))

(defn get-all-neighbouring-cells
  "In `grid` get all cells neighbouring `coords`"
  [grid coords]
  (map (partial get-cell grid)
       (get-neighbouring-coords grid coords '(:north :east :south :west))))


(defrecord SimpleGrid [rows cols cells]
  Grid
  (count-visible-cells [this] (* rows cols))
  (get-visible-cell [this coord] (get-cell this coord))
  (iter-visible-coords [this] (all-coords-for rows cols))
  (iter-visible-rows-coords [this] (iter-rows-coords this))
  (iter-visible-row-coords [this y] (iter-row-coords this y)))

(defn new-simple-grid
  "Create a SimpleGrid of `rows` and `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/grid? %)]}
  (SimpleGrid. rows cols (init-cells rows cols)))
