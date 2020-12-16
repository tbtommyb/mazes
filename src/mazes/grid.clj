(ns mazes.grid
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

(def directions [:north :west :south :east])

(def coords
  (zipmap directions [[0 1]
                       [-1 0]
                       [0 -1]
                       [1 0]]))

(s/def ::coords (s/and #(= 2 (count %))
                       #(every? int? %)))
(s/def ::coord-list (s/coll-of ::coords))
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
  "Create the key to look up `coords`"
  [coords]
  {:pre [(s/valid? ::coords coords)]}
  coords)

(defn init
  "Create a grid of size `rows` * `cols`"
  [rows cols]
  {:pre [(s/valid? ::rows rows)
         (s/valid? ::cols cols)]
   :post [(s/valid? ::grid? %)]}
  {:rows rows
   :cols cols
   :cells (reduce #(assoc %1 (grid-key %2) (set '()))
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
  "Return the cell located at `coords` in `grid`"
  [grid coords]
  {:pre [(s/valid? ::coords coords)
         (s/valid? ::grid? grid)]
   :post [(s/valid? (s/nilable ::cell?) %)]}
  (when-let [links (get-in grid [:cells (grid-key coords)])]
    (make-cell coords links)))

(defn iter-grid-cells
  "Iterate through `grid` by column then row, returning each cell"
  [grid]
  {:pre [(s/valid? ::grid? grid)]
   :post [(s/valid? ::cell-list? %)]}
  (map (partial get-cell grid)
       (all-coords-for (:rows grid) (:cols grid))))

;; TODO can extract these with common iterator
;; (defn iter-row-links [grid y]
;;   (vec (for [x (range (:cols grid))] (hash-map [x y] (get-links grid x y)))))

;; (defn iter-row [grid y]
;;   (vec (for [x (range (:cols grid))] (vector x y))))

;; (defn iter-rows
;;   "Create a vector of `grid`'s rows"
;;   [grid]
;;   (vec (for [y (range (:rows grid))] (iter-row grid y))))

;; (defn direction-from-cell [cell direction]
;;   "get coordinate of direction from a given cell"
;;   (let [[dx dy] (get coords direction)]
;;     (vector (+ dx (first cell)) (+ dy (second cell)))))

;; (defn cell-has-neighbour? [grid cell direction]
;;   (let [[x y] (direction-from-cell cell direction)]
;;     (not (nil? (get-links grid x y)))))

;; (defn cell-has-link? [cell direction]
;;   (contains? cell direction))

;; (defn cell-at-dir [grid cell direction]
;;   "Get the cell at direction"
;;   (direction-from-cell cell direction))

;; (defn direction-between [from to]
;;   "find the direction between two cells"
;;   (let [dy (- (second to) (second from))
;;         dx (- (first to) (first from))]
;;     (-> coords
;;         (select-keys (for [[k [x y]] coords :when (and (= x dx) (= y dy))] k))
;;         keys
;;         first)))

;; (defn update-link [grid cell dir]
;;   "Add dir to cell links"
;;   (update-in grid [:cells (grid-key cell)] #(conj % dir)))

;; (defn link-cells
;;   "Record a link between two cells"
;;   ([grid, src, dest] (link-cells grid src dest true))
;;   ([grid, src, dest, bidirectional]
;;    (let [direction (direction-between src dest)
;;          reverse (direction-between dest src)]
;;      (cond-> grid
;;        (some? direction) (update-link src direction)
;;        (and bidirectional (some? reverse)) (update-link dest reverse)))))

;; (defn get-neighbour [grid cell directions]
;;   "Get all cells neighbouring cell at specified directions"
;;   (reduce (fn [neighbours dir] (if (cell-has-neighbour? grid cell dir)
;;                                  (conj neighbours (cell-at-dir grid cell dir))
;;                                  neighbours))
;;           []
;;           directions))

;; (defn get-link-cell [grid cell directions]
;;   "Get all linked cell at specified directions"
;;   (reduce (fn [links dir]
;;             (if (cell-has-link? (get-links grid cell) dir)
;;               (conj links (cell-at-dir grid cell dir))
;;               links))
;;           []
;;           directions))

;; (defn get-linked-cells [grid cell]
;;   (get-link-cell grid cell [:north :east :south :west]))

