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

(defn iter-coords
  "Iterate through `grid` by column, returning each coord"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? ::coord-list %)]}
  (all-coords-for (:rows grid) (:cols grid)))

;; TODO: cells or coordinates?
(defn iter-single-row
  "Create a vector of every coord in row `y` of `grid`"
  [grid y]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? int? y)]
   :post [(s/valid? ::coord-list %)]}
  (vec (for [x (range (:cols grid))] [x y])))

(defn iter-rows-cells
  "Create a cell seq of every row in `grid`"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? (s/coll-of ::cell-list?) %)]}
  (seq (for [y (range (:rows grid))]
         (map (partial get-cell grid) (iter-single-row grid y)))))

(defn iter-rows-coords
  "Create a coord vector of every row in `grid`"
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

(defn add-direction-to-coords
  "Add `direction` to `coords`"
  [coords direction]
  {:pre [(s/valid? ::coords coords)
         (s/valid? ::direction? direction)]}
  (let [[dx dy] (get dir-step direction)]
    (vector (+ dx (first coords)) (+ dy (second coords)))))

(defn cell-neighbour-at
  "Find neighour in `direction` of cell at `coords in `grid`"
  [grid coords direction]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? ::coords coords)
         (s/valid? ::direction? direction)]
   :post [(s/valid? (s/nilable ::coords) %)]}
  (let [neighbour-coords (add-direction-to-coords coords direction)]
    (when (get-cell grid neighbour-coords)
      neighbour-coords)))

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
  {:pre [(s/valid? ::coords from)
         (s/valid? ::coords to)]
   :post [(s/valid? (s/nilable ::direction?) %)]}
  (let [dx (- (first to) (first from))
        dy (- (second to) (second from))]
    (get step-dir [dx dy])))

(defn link-cell-towards
  "Add a link from `coords` to `direction`"
  [grid coords direction]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? ::coords coords)
         (s/valid? ::direction? direction)]
   :post [(s/valid? ::grid? %)]}
  (update-in grid [:cells coords] #(conj % direction)))

(defn link-cells
  "Record a link in `grid` from `src` to `dest` if they are neighbours. Default bidirectional"
  ([grid, src, dest] (link-cells grid src dest true))
  ([grid, src, dest, bidirectional]
   {:pre [(s/valid? ::grid? grid)
          (s/valid? ::coords src)
          (s/valid? ::coords dest)
          (s/valid? boolean? bidirectional)]
    :post [(s/valid? ::grid? grid)]}
   (let [direction (direction-between src dest)
         reverse (direction-between dest src)]
     (cond-> grid
       (some? direction) (link-cell-towards src direction)
       (and bidirectional (some? reverse)) (link-cell-towards dest reverse)))))

;; TODO: use a seq?
(defn get-cell-neighbours
  "In `grid` get neighbours of cell at `coords` in `directions`"
  [grid coords directions]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? ::coords coords)
         (s/valid? (s/coll-of ::direction?) directions)]
   :post [(s/valid? ::coord-list %)]}
  (reduce #(if-let [neighbour (cell-neighbour-at grid coords %2)]
             (conj %1 neighbour)
             %1)
          []
          directions))

;; TODO: use a seq?
(defn get-cell-links
  "In `grid` get all cells linked to cell at `coords` in `directions`. Default all"
  ([grid coords] (get-cell-links grid coords directions))
  ([grid coords dirs]
   {:pre [(s/valid? ::grid? grid)
          (s/valid? ::coords coords)
          (s/valid? (s/coll-of ::direction?) dirs)]
    :post [(s/valid? ::coord-list %)]}
   (reduce #(if (cell-has-link? (get-cell grid coords) %2)
              (conj %1 (cell-neighbour-at grid coords %2))
              %1)
           []
           directions)))

