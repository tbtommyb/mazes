(ns mazes.grid.grid
  (:require
   [mazes.cell.cell :as cell]
   [mazes.specs :as spec]
   [clojure.spec.alpha :as s]))

(def step-dir-cartesian {[0 1] :north
                         [-1 0] :west
                         [0 -1] :south
                         [1 0] :east})
(def dir-step-cartesian {:north [0 1]
                         :west [-1 0]
                         :south [0 -1]
                         :east [1 0]})
(def cartesian-dirs #{:north :south :east :west})

;; TODO simply check if grid has a :mask key
(defmulti size :mask-type)
(defmulti get-cell :mask-type)

;; move to cell?
(defn direction-between-coords
  [from to]
  {:pre [(s/valid? ::spec/coords from)
         (s/valid? ::spec/coords to)]
   :post [(s/valid? (s/nilable ::spec/cartesian-direction?) %)]}
  (let [dx (- (first to) (first from))
        dy (- (second to) (second from))]
    (get step-dir-cartesian [dx dy])))

;; (defn get-all-neighbouring-cells
;;   "In `grid` get all cells neighbouring `coords`"
;;   [grid coords]
;;   (map (partial get-cell grid)
;;        (get-neighbouring-coords grid coords '(:north :east :south :west))))

;; (defn neighbours-at-dir-cartesian
;;   [coords direction]
;;   {:pre [(s/valid? (s/nilable ::spec/coords) coords)
;;          (s/valid? ::spec/cartesian-direction? direction)]
;;    :post [(s/valid? ::spec/coords %)]}
;;   (let [[dx dy] (get dir-step-cartesian direction)]
;;     (vector (+ dx (first coords)) (+ dy (second coords)))))

;; (defn visible-neighbour-coords
;;   "Find visible neighbour from `src` in `direction` in `grid`"
;;   [grid src direction]
;;   {:pre [(s/valid? ::spec/grid? grid)
;;          (s/valid? ::spec/coords src)
;;          (s/valid? ::spec/cartesian-direction? direction)]
;;    :post [(s/valid? (s/nilable ::spec/coords) %)]}
;;   (let [neighbour-coords (neighbours-at-dir grid src direction)]
;;     (when (get-visible-cell grid neighbour-coords)
;;       neighbour-coords)))

;; (defn get-neighbouring-coords
;;   "In `grid` get neighbours of `coords` in `directions`"
;;   [grid coords directions]
;;   {:pre [(s/valid? ::spec/grid? grid)
;;          (s/valid? ::spec/coords coords)]
;;    :post [(s/valid? ::spec/coord-list %)]}
;;   (reduce #(if-let [neighbour (visible-neighbour-coords grid coords %2)]
;;              (conj %1 neighbour)
;;              %1)
;;           []
;;           directions))

;; (defn get-all-neighbouring-coords-cartesian
;;   "In `grid` get all coords neighbouring `coord`"
;;   [grid coord]
;;   (get-neighbouring-coords grid coord '(:north :east :south :west)))

;; new, keep all this
(defn generate-coords
  "Generate every coordinate for grid of size `rows` by `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/coord-list %)]}
  (for [x (range cols) y (range rows)] [x y]))

(defn iter-grid
  "Iterate through `grid` by column, returning each accessible cell"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (keep (partial get-cell grid)
        (generate-coords (:rows grid) (:cols grid))))

(defn iter-row
  "Return vector of cells in `row` in `grid`"
  [grid y]
  (keep (partial get-cell grid)
        (for [x (range (:cols grid))] [x y])))

(defn iter-rows
  "Return a vector of cell vectors for each row in `grid`"
  [grid]
  (for [y (range (:rows grid))] (iter-row grid y)))

(defn init-cells
  "Create cells for a grid of size `rows` * `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/cells %)]}
   (reduce #(assoc %1 %2 {}) {} (generate-coords rows cols)))

(defn get-cell-helper
  "Return the cell located in `grid` at `coord`"
  [grid coord]
  {:pre [(s/valid? ::spec/coords coord)
         (s/valid? ::spec/grid? grid)]
   :post [(s/valid? (s/nilable ::spec/cell?) %)]}
  (when-let [links (get-in grid [:cells coord])]
    (cell/make-cell coord links)))

(defn get-linked-cells
  "In `grid` get all cells linked to `cell` in `directions`. Default all"
  ([grid cell] (get-linked-cells grid cell cartesian-dirs))
  ([grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? cell)
          (s/valid? (s/coll-of ::spec/cartesian-direction?) dirs)]
    :post [(s/valid? ::spec/coord-list %)]}
   (mapcat (fn [dir] (map (partial get-cell grid) (cell/links-at cell dir))) dirs)))

(defn add-link
  [grid src dest dir] (update-in grid [:cells src dir] #(conj % dest)))

(defn link-coords
  "Record a link in `grid` from `src` to `dest` if they are neighbours. Default bidirectional"
  ([grid src dest] (link-coords grid src dest true))
  ([grid src dest bidirectional]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/coords src)
          (s/valid? ::spec/coords dest)
          (s/valid? boolean? bidirectional)]
    :post [(s/valid? ::spec/grid? grid)]}
   (let [direction (direction-between-coords src dest)
         reverse (direction-between-coords dest src)]
     (cond-> grid
       (some? direction) (add-link src dest direction)
       (and bidirectional (some? reverse)) (add-link dest src reverse)))))

(defn new-grid
  "Create a grid of `rows` and `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/grid? %)]}
  {:mask-type :unmasked :rows rows :cols cols :cells (init-cells rows cols)})

(defmethod size :unmasked [grid] (* (:rows grid) (:cols grid)))
(defmethod get-cell :unmasked [grid coord] (get-cell-helper grid coord))
