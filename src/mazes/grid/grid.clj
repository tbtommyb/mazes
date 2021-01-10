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
(defn direction-between-cells
  [from to]
  {:pre [(s/valid? ::spec/cell? from)
         (s/valid? ::spec/cell? to)]
   :post [(s/valid? (s/nilable ::spec/cartesian-direction?) %)]}
  (let [dx (- (cell/get-x to) (cell/get-x from))
        dy (- (cell/get-y to) (cell/get-y from))]
    (get step-dir-cartesian [dx dy])))

(defn get-neighbour-at
  "Find neighbour from `src` in `direction` in `grid`"
  [grid src direction]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? src)
         (s/valid? ::spec/cartesian-direction? direction)]
   :post [(s/valid? (s/nilable ::spec/cell?) %)]}
  (let [[dx dy] (get dir-step-cartesian direction)
        coord (vector (+ dx (cell/get-x src)) (+ dy (cell/get-y src)))]
    (get-cell grid coord)))

(defn get-neighbouring-cells
  "In `grid` get neighbours of `cell` in `directions`"
  ([grid cell] (get-neighbouring-cells grid cell '(:north :south :east :west)))
  ([grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? cell)]
    :post [(s/valid? ::spec/cell-list? %)]}
   (keep #(get-neighbour-at grid cell %) dirs)))

;; new, keep all this
(defn get-cell-helper
  "Return the cell located in `grid` at `coord`"
  [grid coord]
  {:pre [(s/valid? ::spec/coords coord)
         (s/valid? ::spec/grid? grid)]
   :post [(s/valid? (s/nilable ::spec/cell?) %)]}
  (when-let [links (get-in grid [:cells coord])]
    (cell/make coord links)))

(defn generate-coords
  "Generate every coordinate for grid of size `rows` by `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/coord-list %)]}
  (for [x (range cols) y (range rows)] [x y]))

(defn iter-grid
  "Iterate through `grid` by column, returning each accessible cell"
  [grid & [opt]]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (let [getter (if (:ignore-mask opt) get-cell-helper get-cell)]
    (keep (partial getter grid)
          (sort-by (juxt first last) (keys (:cells grid))))))

(defn iter-row
  "Return vector of cells in `row` in `grid`"
  [grid y & [opt]]
  (filter #(= y (cell/get-y %)) (iter-grid grid opt)))

(defn iter-rows
  "Return a vector of cell vectors for each row in `grid`"
  [grid & [opt]]
  (for [y (range (:rows grid))] (iter-row grid y opt)))

(defn init-cells
  "Create cells for a grid of size `rows` * `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/cells %)]}
   (reduce #(assoc %1 %2 {}) {} (generate-coords rows cols)))

(defn get-linked-cells
  "In `grid` get all cells linked to `cell` in `directions`. Default all"
  ([grid cell] (get-linked-cells grid cell cartesian-dirs))
  ([grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? cell)
          (s/valid? (s/coll-of ::spec/cartesian-direction?) dirs)]
    :post [(s/valid? ::spec/cell-list? %)]}
   (mapcat (fn [dir] (map (partial get-cell grid) (cell/links-at cell dir))) dirs)))

(defn add-link
  [grid src dest dir]
  (update-in grid [:cells (cell/coords src) dir] #(conj % (cell/coords dest))))

(defn link-cells
  "Record a link in `grid` from `src` to `dest` if they are neighbours. Default bidirectional"
  ([grid src dest] (link-cells grid src dest true))
  ([grid src dest bidirectional]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? src)
          (s/valid? ::spec/cell? dest)
          (s/valid? boolean? bidirectional)]
    :post [(s/valid? ::spec/grid? grid)]}
   (let [direction (direction-between-cells src dest)
         reverse (direction-between-cells dest src)]
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
