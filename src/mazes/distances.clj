(ns mazes.distances
  (:require
   [clojure.spec.alpha :as s]
   [mazes.specs :as spec]
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as grid]))

(defmulti dijkstra (fn [grid start] (:weighting grid)))

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

(defn init-distances
  [grid start-coord]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/distances? % )]}
  (-> (reduce #(set-distance %1 %2 nil) {} (grid/iter-grid grid))
      (assoc start-coord 0)))

(defmethod dijkstra :unweighted
  [grid start]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords start)]
   :post [(s/valid? ::spec/distances? %)]}
  (letfn [(process-neighbour [distance distances neighbour]
            (if (or (nil? (get-distance distances neighbour))
                    (< distance (get-distance distances neighbour)))
              (helper
               (set-distance distances neighbour distance)
               (grid/get-linked-cells grid neighbour)
               (inc distance))
              distances))
          (helper [distances frontier distance]
            (reduce (partial process-neighbour distance) distances frontier))]
    (let [start-cell (grid/get-cell grid start)]
      (-> (init-distances grid start)
          (helper (grid/get-linked-cells grid start-cell) 1)))))

(defmethod dijkstra :weighted
  [grid start]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords start)]
   :post [(s/valid? ::spec/distances? %)]}
  (letfn [(process-neighbour [cell pending weights neighbour]
            (let [total-weight (+ (get-distance weights cell) (:weight neighbour))
                  current-neighbour-weight (get-distance weights neighbour)]
              (if (or (nil? current-neighbour-weight)
                      (< total-weight current-neighbour-weight))
                (helper (set-distance weights neighbour total-weight)
                        (cons neighbour pending))
                (helper weights pending))))
          (helper [weights pending]
            (if (empty? pending)
              weights
              (let [sorted-pending (sort-by :weight pending)
                    cell (first sorted-pending)]
                (reduce (partial process-neighbour cell (rest sorted-pending))
                        weights
                        (grid/get-linked-cells grid cell)))))]
    (-> (init-distances grid start)
        (helper (list (grid/get-cell grid start))))))

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
         (s/valid? ::spec/coords goal)]
   :post [(s/valid? ::spec/distances? %)]}
  (let [distances-to-goal (dijkstra maze goal)
        distances-from-start (dijkstra maze start)
        path (init-distances maze start)]
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
