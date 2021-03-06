(ns mazes.grid.grid
  (:require
   [clojure.data.generators :as gen]
   [mazes.cell.cell :as cell]
   [mazes.utils :as utils]
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

(defn has-mask? [grid & args] (if (:mask grid) :masked :unmasked))
(defmulti size has-mask?)
(defmulti get-cell has-mask?)
(defmulti get-neighbouring-cells (fn [grid & args] [(:type grid) (:weave grid)]))
(defmulti direction-between-cells (fn [grid & args] [(:type grid) (:weave grid)]))
(defmulti get-linked-cells (fn [grid & args] [(:type grid) (:weave grid)]))
(defmulti get-neighbour-at (fn [grid & args] [(:type grid) (:weave grid)]))

;; move to cell?
(defmethod direction-between-cells [:cartesian nil]
  [grid from to]
  {:pre [(s/valid? ::spec/cell? from)
         (s/valid? ::spec/cell? to)]
   :post [(s/valid? (s/nilable ::spec/cartesian-direction?) %)]}
  (let [dx (- (cell/get-x to) (cell/get-x from))
        dy (- (cell/get-y to) (cell/get-y from))]
    (get step-dir-cartesian [dx dy])))

(defmethod get-neighbour-at [:cartesian nil]
  [grid src direction]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? (s/nilable ::spec/cell?) src)
         (s/valid? ::spec/cartesian-direction? direction)]
   :post [(s/valid? (s/nilable ::spec/cell?) %)]}
  (let [[dx dy] (get dir-step-cartesian direction)
        coord (vector (+ dx (cell/get-x src)) (+ dy (cell/get-y src)))]
    (get-cell grid coord)))

(defmethod get-neighbouring-cells [:cartesian nil]
  ([grid cell] (get-neighbouring-cells grid cell '(:north :south :east :west)))
  ([grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? (s/nilable ::spec/cell?) cell)]
    :post [(s/valid? ::spec/cell-list? %)]}
   (keep #(get-neighbour-at grid cell %) dirs)))

(defn visited-neighbours
  [grid cell]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (filter cell/visited? (get-neighbouring-cells grid cell)))

(defn unvisited-neighbours
  [grid cell]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (remove cell/visited? (get-neighbouring-cells grid cell)))

(defn cell-has-visited-neighbours?
  [grid cell]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? (s/nilable boolean?) %)]}
  (some cell/visited? (get-neighbouring-cells grid cell)))

(defn get-cell-helper
  "Return the cell located in `grid` at `coord`"
  [grid coord]
  {:pre [(s/valid? (s/nilable ::spec/coords) coord)
         (s/valid? ::spec/grid? grid)]
   :post [(s/valid? (s/nilable ::spec/cell?) %)]}
  (when-let [cell-body (get-in grid [:cells coord])]
    (cell/make coord cell-body)))

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
   (reduce #(assoc %1 %2 {:links {}}) {} (generate-coords rows cols)))

(defn get-linked-cells-helper
  "In `grid` get all cells linked to `cell` in `directions`. Default all"
  ([grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? cell)]
    :post [(s/valid? ::spec/cell-list? %)]}
   (mapcat (fn [dir] (map (partial get-cell grid) (cell/links-at cell dir))) dirs)))

(defmethod get-linked-cells [:cartesian nil]
  ([grid cell] (get-linked-cells-helper grid cell cartesian-dirs))
  ([grid cell dirs] (get-linked-cells-helper grid cell dirs)))

(defn add-link
  [grid src dest dir]
  (update-in grid
             [:cells (cell/coords src) :links dir]
             #(conj % (cell/coords dest))))

;; TODO so gross
(defn remove-link
  [grid cell neighbour]
  (-> (update-in grid [:cells (cell/coords cell) :links]
                 #(apply hash-map (mapcat (fn [[k v]] [k (remove #{(cell/coords neighbour)} v)]) %)))
      (update-in [:cells (cell/coords cell) :links]
                 #(into {} (filter (fn [[k v]] (not (empty? v))) %)))))

(defn unlink-cells
  ([grid src dest] (unlink-cells grid src dest true))
  ([grid src dest bidirectional]
   (-> (remove-link grid src dest)
       (remove-link dest src))))

(defn link-cells-helper
  "Record a link in `grid` from `src` to `dest` if they are neighbours. Default bidirectional"
  [grid src dest bidirectional]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? src)
         (s/valid? ::spec/cell? dest)
         (s/valid? boolean? bidirectional)]
   :post [(s/valid? ::spec/grid? grid)]}
  (let [direction (direction-between-cells grid src dest)
        reverse (direction-between-cells grid dest src)]
    (cond-> grid
      (some? direction) (add-link src dest direction)
      (and bidirectional (some? reverse)) (add-link dest src reverse))))

(defn link-cells
  ([grid src dest] (link-cells grid src dest true))
  ([grid src dest bidirectional]
   (let [src-cell (cond
                    (s/valid? ::spec/direction? src) (get-neighbour-at grid dest src)
                    (s/valid? ::spec/coords src) (get-cell grid src)
                    :else src)
         dest-cell (cond
                     (s/valid? ::spec/direction? dest) (get-neighbour-at grid src dest)
                     (s/valid? ::spec/coords dest) (get-cell grid dest)
                     :else dest)]
     (link-cells-helper grid src-cell dest-cell bidirectional))))

(defn get-unlinked-neighbours
  [grid cell]
  (filter (complement (partial cell/linked-to? cell)) (get-neighbouring-cells grid cell)))

(defn dead-end-neighbours
  [grid cell]
  (filter cell/dead-end? (get-unlinked-neighbours grid cell)))

(defn refresh-cells
  "Pull all coords of `cells` from `grid`, to refresh links etc"
  [grid cells]
  (map #(get-cell grid (cell/coords %)) cells))

(defn braid
  [grid & [opt]]
  (let [p (:p opt 1.0)]
    (loop [maze grid
           dead-ends (gen/shuffle (filter cell/dead-end? (iter-grid maze)))]
      (if (empty? dead-ends)
        maze
        (if (> (rand) p)
          (recur maze (rest dead-ends))
          (let [dead-end (first dead-ends)
                neighbour (or (utils/safe-rand-nth (dead-end-neighbours maze dead-end))
                              (utils/safe-rand-nth (get-unlinked-neighbours maze dead-end)))
                new-maze (link-cells maze dead-end neighbour)]
            (recur new-maze
                   (remove (complement cell/dead-end?)
                           (refresh-cells new-maze (rest dead-ends))))))))))

(defn new-grid
  "Create a grid of `rows` and `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/grid? %)]}
  {:type :cartesian
   :weighting :unweighted
   :rows rows
   :cols cols
   :cells (init-cells rows cols)})

(defmethod size :unmasked [grid] (count (:cells grid)))
(defmethod get-cell :unmasked [grid coord] (get-cell-helper grid coord))
