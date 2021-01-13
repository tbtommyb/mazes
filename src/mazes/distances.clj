(ns mazes.distances
  (:require
   [clojure.spec.alpha :as s]
   [mazes.specs :as spec]
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as grid]))

(defmulti dijkstra (fn [grid start] (:weighting grid)))
(defmulti init-distances (fn [grid & args] (:weighting grid)))

(defn set-distance
  "Set distance for `cell` in `distances` to `value`"
  [distances cell value]
  {:pre [(s/valid? ::spec/distances? distances)
         (s/valid? ::spec/cell? cell)
         (s/valid? ::spec/distance? value)]
   :post [(s/valid? ::spec/distances? %)]}
  (assoc distances (cell/coords cell) value))

(defn get-distance
  "Get distance of `cell` in `distances`"
  [distances cell]
  {:pre [(s/valid? ::spec/distances? distances)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/distance? %)]}
  (get distances (cell/coords cell)))

(defmethod init-distances :unweighted
  [grid & args]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/distances? % )]}
  (-> (reduce #(set-distance %1 %2 nil)
          {}
          (grid/iter-grid grid))))

(defmethod init-distances :weighted
  [grid start-coord]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/distances? % )]}
  (-> (reduce #(set-distance %1 %2 nil)
          {}
          (grid/iter-grid grid))
      (assoc start-coord 0)))

(defn iter-dijkstra-unweighted
  "Determine the distance of every cell in `grid` from `start`"
  [distances grid frontier]
  {:pre [(s/valid? ::spec/distances? distances)
         (s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? frontier)]
   :post [(s/valid? ::spec/distances? %)]}
  (letfn [(update-dist [distances cell distance]
            (if (or (nil? (get-distance distances cell))
                    (< distance (get-distance distances cell)))
              (iter-helper
               (set-distance distances cell distance)
               (grid/get-linked-cells grid cell)
               (inc distance))
              distances))
          (iter-helper [distances cells distance]
            (reduce #(update-dist %1 %2 distance) distances cells))]
    (iter-helper distances (list frontier) 0)))

(declare process-neighbour)

(defn iter-dijkstra-weighted
  "Determine the distance of every cell in `grid` from `start`"
  [weights grid pending]
  {:pre [(s/valid? ::spec/distances? weights)
         (s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell-list? pending)]
   :post [(s/valid? ::spec/distances? %)]}
  (if (empty? pending)
    weights
    (let [sorted-pending (sort-by :weight pending)
          cell (first sorted-pending)]
      (reduce (partial process-neighbour grid cell (rest sorted-pending))
              weights
              (grid/get-linked-cells grid cell)))))

(defn process-neighbour
  [grid cell pending weights neighbour]
  (let [total-weight (+ (get-distance weights cell) (:weight neighbour))]
    (if (or (nil? (get-distance weights neighbour))
            (< total-weight (get-distance weights neighbour)))
      (iter-dijkstra-weighted (set-distance weights neighbour total-weight)
                     grid
                     (cons neighbour pending))
      (iter-dijkstra-weighted weights grid pending))))

;; TODO: validate start is within bounds of grid
;; TODO: check that start is unmasked
(defmethod dijkstra :unweighted
  [grid start]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords start)]
   :post [(s/valid? ::spec/distances? %)]}
  (-> (init-distances grid)
      (iter-dijkstra-unweighted grid (grid/get-cell grid start))))

(defmethod dijkstra :weighted
  [grid start]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords start)]
   :post [(s/valid? ::spec/distances? %)]}
  (-> (init-distances grid start)
      (iter-dijkstra-weighted grid (list (grid/get-cell grid start)))))

(defn find-closer-neighbour
  "Find the first of `cells` with a distance less than `distance`"
  [distances cells distance]
  {:pre [(s/valid? ::spec/distances? distances)
         (s/valid? ::spec/cell-list? cells)
         (s/valid? ::spec/distance? distance)]
   :post [(s/valid? ::spec/cell? %)]}
  (first (filter #(or (nil? (get-distance distances %))
                      (< (get-distance distances %) distance))
                 cells)))

(defn shortest-path
  "Find the shortest path in `maze` from `start` to `goal`"
  [maze start goal]
  {:pre [(s/valid? ::spec/grid? maze)
         (s/valid? ::spec/coords start)
         (s/valid? ::spec/coords goal)
         (s/valid? ::spec/bounded-coord? [maze goal])
         (s/valid? ::spec/bounded-coord? [maze start])]
   :post [(s/valid? ::spec/distances? %)]}
  (let [distances-to-goal (dijkstra maze goal)
        distances-from-start (dijkstra maze start)
        path (-> (init-distances maze start) (set-distance (grid/get-cell maze start) 0))]
    (letfn [(build-path [path curr]
              (if (= (cell/coords curr) goal)
                path
                (let [next-step (find-closer-neighbour distances-to-goal
                                                       (grid/get-linked-cells maze curr)
                                                       (get-distance distances-to-goal curr))
                      next-path (set-distance path next-step (get-distance distances-from-start next-step))]
                  (build-path next-path next-step))))]
      (build-path path (grid/get-cell maze start)))))

(defn furthest-coords
  "Find the coordinates of the furthest cell in `distances`"
  [distances]
  {:pre [(s/valid? ::spec/distances? distances)]
   :post [(s/valid? ::spec/coords %)]}
  (key (apply max-key val (into {} (filter (comp some? val) distances)))))

(defn longest-path
  "Find the longest path within `maze`"
  [maze]
  {:pre [(s/valid? ::spec/grid? maze)]
   :post [(s/valid? ::spec/distances? %)]}
  (let [goal (furthest-coords (dijkstra maze [0 0]))
        start (furthest-coords (dijkstra maze goal))]
    (shortest-path maze start goal)))
