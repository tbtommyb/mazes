(ns mazes.distances
  (:require
   [clojure.spec.alpha :as s]
   [mazes.specs :as spec]
   [mazes.grid :as gr]))

(s/def ::distances? map?)
(s/def ::distance? int?)

(defn set-distance
  "Set distance for `coords` in `distances` to `value`"
  [distances coords value]
  {:pre [(s/valid? ::distances? distances)
         (s/valid? ::spec/coords coords)
         (s/valid? ::distance? value)]
   :post [(s/valid? ::distances? %)]}
  (assoc distances coords value))

(defn get-distance
  "Get distance of `coords` in `distances`"
  [distances coords]
  {:pre [(s/valid? ::distances? distances)
         (s/valid? ::spec/coords coords)]
   :post [(s/valid? ::distance? %)]}
  (get distances coords))

;; TODO use function that gets all coords
(defn init-distances
  "Initialise a `distances` map for `grid`"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::distances? % )]}
  (reduce #(set-distance %1 %2 Integer/MAX_VALUE)
          {}
          (gr/all-coords-for (:rows grid) (:cols grid))))

(defn iter-dijkstra
  "Populate `distances` from `start` in `grid`"
  [distances grid start]
  {:pre [(s/valid? ::distances? distances)
         (s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords start)]
   :post [(s/valid? ::distances? %)]}
  (letfn [(update-dist
            [distances cell distance]
            (if (< distance (get-distance distances cell))
              (iter-helper
               (set-distance distances cell distance)
               (gr/get-cell-links grid cell)
               (inc distance))
              distances))
          (iter-helper
            [distances coords distance]
            (reduce #(update-dist %1 %2 distance) distances coords))]
    (iter-helper distances (list start) 0)))

;; TODO: validate start is within bounds of grid
;; TODO: check that start is unmasked
(defn dijkstra
  "Determine the distance of every cell in `grid` from `start`"
  [grid start]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/coords start)]
   :post [(s/valid? ::distances? %)]}
  (-> (init-distances grid)
      (iter-dijkstra grid start)))

(defn find-closer-neighbour
  "Find the first of `cells` with a distance less than `distance`"
  [distances coords distance]
  {:pre [(s/valid? ::distances? distances)
         (s/valid? ::spec/coord-list coords)
         (s/valid? ::distance? distance)]
   :post [(s/valid? ::spec/coords %)]}
  (first (filter #(< (get-distance distances %) distance) coords)))

(defn shortest-path
  "Find the shortest path in `maze` from `start` to `goal`"
  [maze start goal]
  {:pre [(s/valid? ::spec/grid? maze)
         (s/valid? ::spec/coords start)
         (s/valid? ::spec/coords goal)
         (s/valid? ::spec/bounded-coords? [maze goal])
         (s/valid? ::spec/bounded-coords? [maze start])]
   :post [(s/valid? ::distances? %)]}
  (let [distances-to-goal (dijkstra maze goal)
        distances-from-start (dijkstra maze start)
        path (-> (init-distances maze) (set-distance start 0))]
    (letfn [(build-path [path curr]
              (if (= curr goal)
                path
                (let [next-step (find-closer-neighbour distances-to-goal
                                                       (gr/get-cell-links maze curr)
                                                       (get-distance distances-to-goal curr))
                      next-path (set-distance path next-step (get-distance distances-from-start next-step))]
                  (build-path next-path next-step))))]
      (build-path path start))))

(defn furthest-coords
  "Find the coordinates of the furthest cell in `distances`"
  [distances]
  {:pre [(s/valid? ::distances? distances)]
   :post [(s/valid? ::spec/coords %)]}
  (key (apply max-key val distances)))

(defn longest-path
  "Find the longest path within `maze`"
  [maze]
  {:pre [(s/valid? ::spec/grid? maze)]
   :post [(s/valid? ::distances? %)]}
  (let [goal (furthest-coords (dijkstra maze [0 0]))
        start (furthest-coords (dijkstra maze goal))]
    (shortest-path maze start goal)))
