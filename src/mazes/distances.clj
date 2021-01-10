(ns mazes.distances
  (:require
   [clojure.spec.alpha :as s]
   [mazes.specs :as spec]
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as grid]))

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
  (get distances (cell/coords cell) Integer/MAX_VALUE))

(defn init-distances
  "Initialise a `distances` map for `grid`"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/distances? % )]}
  (reduce #(set-distance %1 %2 Integer/MAX_VALUE)
          {}
          (grid/iter-grid grid)))

(defn iter-dijkstra
  "Populate `distances` from `start` in `grid`"
  [distances grid start]
  {:pre [(s/valid? ::spec/distances? distances)
         (s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? start)]
   :post [(s/valid? ::spec/distances? %)]}
  (letfn [(update-dist [distances cell distance]
            (if (< distance (get-distance distances cell))
              (iter-helper
               (set-distance distances cell distance)
               (grid/get-linked-cells grid cell)
               (inc distance))
              distances))
          (iter-helper [distances cells distance]
            (reduce #(update-dist %1 %2 distance) distances cells))]
    (iter-helper distances (list start) 0)))

;; TODO: validate start is within bounds of grid
;; TODO: check that start is unmasked
(defn dijkstra
  "Determine the distance of every cell in `grid` from `start`"
  [grid start]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords start)]
   :post [(s/valid? ::spec/distances? %)]}
  (-> (init-distances grid)
      (iter-dijkstra grid (grid/get-cell grid start))))

(defn find-closer-neighbour
  "Find the first of `cells` with a distance less than `distance`"
  [distances cells distance]
  {:pre [(s/valid? ::spec/distances? distances)
         (s/valid? ::spec/cell-list? cells)
         (s/valid? ::spec/distance? distance)]
   :post [(s/valid? ::spec/cell? %)]}
  (first (filter #(< (get-distance distances %) distance) cells)))

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
        path (-> (init-distances maze) (set-distance (grid/get-cell maze start) 0))]
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
  (key (apply max-key val distances)))

(defn longest-path
  "Find the longest path within `maze`"
  [maze]
  {:pre [(s/valid? ::spec/grid? maze)]
   :post [(s/valid? ::spec/distances? %)]}
  (let [goal (furthest-coords (dijkstra maze [0 0]))
        start (furthest-coords (dijkstra maze goal))]
    (shortest-path maze start goal)))
