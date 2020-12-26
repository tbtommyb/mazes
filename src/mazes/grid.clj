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
(s/def ::bounded-coords? (fn [[maze coords]]
                           (and (>= (first coords) 0)
                                (>= (second coords) 0)
                                (< (first coords) (:cols maze))
                                (< (second coords) (:rows maze)))))

;; PROTOCOL STUFF
(defprotocol Grid
  "Grid represents the idea of a grid with accessible cells"
  (count-visible-cells [this]
    "Return the number of visible cells")
  (get-visible-cell [this coord]
    "Gets the cell located at `coord` if visible")
  (iter-visible-coords [this]
    "Iterate through every coordinate")
  (iter-visible-row-coords [this y]
    "Iterate through row `y` in `this`"))

(defrecord SimpleGrid [rows cols cells]
  Grid
  (count-visible-cells [this] (* rows cols))
  (get-visible-cell [this coord] (get-cell this coord))
  (iter-visible-coords [this] (all-coords-for rows cols))
  (iter-visible-row-coords [this y] (iter-row-coords this y)))

(defn new-simple-grid
  "Create a SimpleGrid of `rows` and `cols`"
  [rows cols]
  {:pre [(s/valid? ::rows rows)
         (s/valid? ::cols cols)]
   :post [(s/valid? ::Grid? %)]}
  (SimpleGrid. rows cols (init-cells rows cols)))

;; Base Grid utilities
(defn init-cells
  "Create a grid of size `rows` * `cols`"
  [rows cols]
  {:pre [(s/valid? ::rows rows)
         (s/valid? ::cols cols)]
   :post [(s/valid? ::cells %)]}
   (reduce #(assoc %1 %2 (set '())) {} (all-coords-for rows cols)))

(defn all-coords-for
  "Generate every coordinate for grid of size `rows` by `cols`"
  [rows cols]
  {:pre [(s/valid? ::rows rows)
         (s/valid? ::cols cols)]
   :post [(s/valid? ::coord-list %)]}
  (for [x (range cols) y (range rows)] [x y]))

(defn get-cell
  "Return the cell located in `grid` at `coords`, ignoring visibility"
  [grid coords]
  {:pre [(s/valid? ::coords coords)
         (s/valid? ::grid? grid)]
   :post [(s/valid? (s/nilable ::cell?) %)]}
  (when-let [links (get-in grid [:cells coords])]
    (make-cell coords links)))

(defn iter-row-coords
  "Create a vector of every coord in row `y` of `grid`, ignoring visibility"
  [grid y]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? int? y)]
   :post [(s/valid? ::coord-list %)]}
  (vec (for [x (range (:cols grid))] [x y])))

(defn iter-rows-cells
  "Create a vector of every cell by row in `grid`, ignoring visibility"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? (s/coll-of ::cell-list?) %)]}
  (seq (for [y (range (:rows grid))]
         (map (partial get-cell grid) (iter-row-coords grid y)))))

(defn iter-cells
  "Iterate through `grid` by column, returning each cell, ignoring visibility"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? ::cell-list? %)]}
  (map (partial get-cell grid) (all-coords-for (:rows grid) (:cols grid))))

;; Cell stuff (to be moved)
(defn grid-key
  "Create the key to look up `cell`"
  [cell]
  {:pre [(s/valid? ::cell? cell)]
   :post [(s/valid? ::coords %)]}
  (:coords cell))

(defn make-cell
  "Make a cell from `coords` and optional `links`"
  ([coords]
   {:pre [(s/valid? ::coords coords)]}
   (make-cell coords #{}))
  ([coords links]
  {:pre [(s/valid? ::coords coords)
         (s/valid? ::links links)]}
   (hash-map :coords coords :links links)))

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

(defn cell-has-link?
  "Boolean whether the `cell` has a link to `direction`"
  [cell direction]
  {:pre [(s/valid? (s/nilable ::cell?) cell)
         (s/valid? ::direction? direction)]
   :post [(s/valid? boolean? %)]}
  (contains? (:links cell) direction))

(defn cell-visited?
  "Boolean whether `cell` has any links"
  [cell]
  {:pre [(s/valid? ::cell? cell)]
   :post [(s/valid? boolean? %)]}
  (not (empty? (:links cell))))

(defn coords-from-cell
  "Get coordinate of `direction` from `cell`"
  [cell direction]
  {:pre [(s/valid? ::cell? cell)
         (s/valid? ::direction? direction)]}
  (let [[dx dy] (get dir-step direction)]
    (vector (+ dx (get-cell-x cell)) (+ dy (get-cell-y cell)))))

;; Protocol-dependent functions
(defn iter-visible-cells
  "Iterate through `grid` by column, returning each visible cell"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? ::cell-list? %)]}
  (map (partial get-cell grid) (iter-visible-coords grid)))

(defn iter-visible-rows-cells
  "Create a seq of every visible cell by row in `grid`"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? (s/coll-of ::cell-list?) %)]}
  (seq (for [y (range (:rows grid))]
         (map (partial get-cell grid) (iter-visible-row-coords grid y)))))

(defn iter-visible-rows-coords
  "Create a coord vector of every row in `grid`"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? (s/coll-of ::coord-list) %)]}
  (vec (for [y (range (:rows grid))] (iter-visible-row-coords grid y))))

(defn visible-neighbour-coords
  "Find visible neighbour from `src` in `direction` in `grid`"
  [grid src direction]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? ::coords src)
         (s/valid? ::direction? direction)]
   :post [(s/valid? (s/nilable ::coords) %)]}
  (let [neighbour-coords (add-direction-to-coords src direction)]
    (when (get-visible-cell grid neighbour-coords)
      neighbour-coords)))

;; TODO: use a seq?
(defn get-cell-links
  "In `grid` get all cells linked to cell at `coords` in `directions`. Default all"
  ([grid coords] (get-cell-links grid coords directions))
  ([grid coords dirs]
   {:pre [(s/valid? ::grid? grid)
          (s/valid? ::coords coords)
          (s/valid? (s/coll-of ::direction?) dirs)]
    :post [(s/valid? ::coord-list %)]}
   (reduce #(if (cell-has-link? (get-visible-cell grid coords) %2)
              (conj %1 (visible-neighbour-coords grid coords %2))
              %1)
           []
           directions)))

;; Direction stuff
(defn add-direction-to-coords
  "Add `direction` to `coords`"
  [coords direction]
  {:pre [(s/valid? ::coords coords)
         (s/valid? ::direction? direction)]}
  (let [[dx dy] (get dir-step direction)]
    (vector (+ dx (first coords)) (+ dy (second coords)))))

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
(defn get-neighbouring-coords
  "In `grid` get neighbours of cell at `coords` in `directions`"
  [grid coords directions]
  {:pre [(s/valid? ::grid? grid)
         (s/valid? ::coords coords)
         (s/valid? (s/coll-of ::direction?) directions)]
   :post [(s/valid? ::coord-list %)]}
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

