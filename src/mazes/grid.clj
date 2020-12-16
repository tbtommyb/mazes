(ns mazes.grid
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

(def directions #{:north :south :east :west})
(def step-dir {[0 1] :north
               [-1 0] :west
               [0 -1] :south
               [1 0] :east})
(def dir-step {:north [0 1]
               :west [-1 0]
               :south [0 -1]
               :east [1 0]})


(s/def ::coords (s/and #(= 2 (count %))
                       #(every? int? %)))
(s/def ::coord-list (s/coll-of ::coords))
(s/def ::direction? #(contains? directions %))
(s/def ::rows pos-int?)
(s/def ::cols pos-int?)
(s/def ::cells map?)
(s/def ::links set?)
(s/def ::grid? (s/keys :req-un [::rows ::cols ::cells]))
(s/def ::cell? (s/keys :req-un [::coords ::links]))
(s/def ::cell-list? (s/coll-of ::cell?))

(defn all-coords-for
  "Generate every coordinate for grid of size `rows` by `cols`"
  [rows cols]
  {:pre [(s/valid? ::rows rows)
         (s/valid? ::cols cols)]
   :post [(s/valid? ::coord-list %)]}
  (for [x (range cols) y (range rows)] [x y]))

(defn grid-key
  "Create the key to look up `cell`"
  [cell]
  {:pre [(s/valid? ::cell? cell)]
   :post [(s/valid? ::coords %)]}
  (:coords cell))

(defn init
  "Create a grid of size `rows` * `cols`"
  [rows cols]
  {:pre [(s/valid? ::rows rows)
         (s/valid? ::cols cols)]
   :post [(s/valid? ::grid? %)]}
  {:rows rows
   :cols cols
   :cells (reduce #(assoc %1 %2 (set '()))
                  {}
                  (all-coords-for rows cols))})

(defn make-cell
  "Make a cell from `coords` and optional `links`"
  ([coords]
   {:pre [(s/valid? ::coords coords)]}
   (make-cell coords #{}))
  ([coords links]
  {:pre [(s/valid? ::coords coords)
         (s/valid? ::links links)]}
   (hash-map :coords coords :links links)))

(defn get-cell
  "Return the cell located in `grid` at `coords`"
  [grid coords]
  {:pre [(s/valid? ::coords coords)
         (s/valid? ::grid? grid)]
   :post [(s/valid? (s/nilable ::cell?) %)]}
  (when-let [links (get-in grid [:cells coords])]
    (make-cell coords links)))

(defn get-cell-x
  "Return x coordinate of `cell`"
  [cell]
  {:pre [(s/valid? ::cell? cell)]
   :post [(s/valid? int? %)]}
  (get-in cell [:coords 0]))

(defn get-cell-y
  "Return y coordinate of `cell`"
  [cell]
  {:pre [(s/valid? ::cell? cell)]
   :post [(s/valid? int? %)]}
  (get-in cell [:coords 1]))

(defn iter-cells
  "Iterate through `grid` by column, returning each cell"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? ::cell-list? %)]}
  (map (partial get-cell grid)
       (all-coords-for (:rows grid) (:cols grid))))

(defn iter-rows-cells
  "Iterate through `grid` by row, returning each cell"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? (s/coll-of ::cell-list?) %)]}
  (map (partial get-cell grid)
       (all-coords-for (:rows grid) (:cols grid))))

;; TODO: cells or coordinates?
(defn iter-single-row
  "Create a vector of every coordinate in row `y` of `grid`"
  [grid y]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? int? y)]
   :post [(s/valid? ::coord-list %)]}
  (vec (for [x (range (:cols grid))] [x y])))

(defn iter-rows
  "Create a vector of every row in `grid`"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? (s/coll-of ::coord-list) %)]}
  (vec (for [y (range (:rows grid))] (iter-single-row grid y))))

(defn coords-from-cell
  "Get coordinate of `direction` from `cell`"
  [cell direction]
  {:pre [(s/valid? ::cell? cell)
         (s/valid? ::direction? direction)]}
  (let [[dx dy] (get dir-step direction)]
    (vector (+ dx (get-cell-x cell)) (+ dy (get-cell-y cell)))))

(defn cell-neighbour-at
  "Get the (possibly nil) cell neighbouring `cell` at `direction`"
  [grid cell direction]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? ::cell? cell)
         (s/valid? ::direction? direction)]
   :post [(s/valid? (s/nilable ::cell?) %)]}
  (get-cell grid (coords-from-cell cell direction)))

(defn cell-has-link?
  "Boolean whether the `cell` has a link to `direction`"
  [cell direction]
  {:pre [(s/valid? ::cell? cell)
         (s/valid? ::direction? direction)]
   :post [(s/valid? boolean? %)]}
  (contains? (:links cell) direction))

(defn direction-between
  "Returns the direction as nilable symbol between `from` and `to`"
  [from to]
  {:pre [(s/valid? ::cell? from)
         (s/valid? ::cell? to)]
   :post [(s/valid? (s/nilable ::direction?) %)]}
  (let [dx (- (get-cell-x to) (get-cell-x from))
        dy (- (get-cell-y to) (get-cell-y from))]
    (get step-dir [dx dy])))

(defn link-cell-towards
  "Add a link from `cell` to `direction`"
  [grid cell direction]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? ::cell? cell)
         (s/valid? ::direction? direction)]
   :post [(s/valid? ::grid? %)]}
  (update-in grid [:cells (grid-key cell)] #(conj % direction)))

(defn link-cells
  "Record a link in `grid` from `src` to `dest` if they are neighbours. Default bidirectional"
  ([grid, src, dest] (link-cells grid src dest true))
  ([grid, src, dest, bidirectional]
   {:pre [(s/valid? ::grid? grid)
          (s/valid? ::cell? src)
          (s/valid? ::cell? dest)
          (s/valid? boolean? bidirectional)]
    :post [(s/valid? ::grid? grid)]}
   (let [direction (direction-between src dest)
         reverse (direction-between dest src)]
     (cond-> grid
       (some? direction) (link-cell-towards src direction)
       (and bidirectional (some? reverse)) (link-cell-towards dest reverse)))))

;; TODO: use a seq?
(defn get-cell-neighbours
  "In `grid` get neighbours of `cell` at `directions`"
  [grid cell directions]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? ::cell? cell)
         (s/valid? (s/coll-of ::direction?) directions)]
   :post [(s/valid? ::cell-list? %)]}
  (reduce #(if-let [neighbour (cell-neighbour-at grid cell %2)]
             (conj %1 neighbour)
             %1)
          []
          directions))

;; TODO: use a seq?
(defn get-cell-links
  "In `grid` get all cells linked to `cell` in `directions`. Default all"
  ([grid cell] (get-cell-links grid cell [:north :east :west :south]))
  ([grid cell directions]
   {:pre [(s/valid? ::grid? grid)
          (s/valid? ::cell? cell)
          (s/valid? (s/coll-of ::direction?) directions)]
    :post [(s/valid? ::cell-list? %)]}
   (reduce #(if (cell-has-link? (:links cell) %2)
              (conj %1 (cell-neighbour-at grid cell %2))
              %1)
           []
           directions)))

