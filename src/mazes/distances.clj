(ns mazes.distances
  (:require
   [clojure.spec.alpha :as s]
   [mazes.grid :as gr]))

(s/def ::distances? map?)
(s/def ::distance? int?)

(defn set-distance
  "Set distance for `coords` in `distances` to `value`"
  [distances coords value]
  {:pre [(s/valid? ::distances? distances)
         (s/valid? ::gr/coords coords)
         (s/valid? ::distance? value)]
   :post [(s/valid? ::distances? %)]}
  (assoc distances coords value))

(defn get-distance
  "Get distance of `coords` in `distances`"
  [distances coords]
  {:pre [(s/valid? ::distances? distances)
         (s/valid? ::gr/coords coords)]
   :post [(s/valid? ::distance? %)]}
  (get distances coords))

(defn init-distances
  "Initialise a `distances` map for `grid`"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::distances? % )]}
  (reduce #(set-distance %1 %2 Integer/MAX_VALUE)
          {}
          (gr/iter-coords grid)))

(defn iter-dijkstra
  "Populate `distances` from `start` in `grid`"
  [distances grid start]
  {:pre [(s/valid? ::distances? distances)
         (s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coords start)]
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
(defn dijkstra
  "Determine the distance of every cell in `grid` from `start`"
  [grid start]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coords start)]
   :post [(s/valid? ::distances? %)]}
  (-> (init-distances grid)
      (iter-dijkstra grid start)))

(defn find-closer-neighbour
  "Find the first of `cells` with a distance less than `distance`"
  [distances coords distance]
  {:pre [(s/valid? ::distances? distances)
         (s/valid? ::gr/coord-list coords)
         (s/valid? ::distance? distance)]
   :post [(s/valid? ::gr/coords %)]}
  (first (filter #(< (get-distance distances %) distance) coords)))

(defn build-path
  "Recursively generate a list of coordinates from `curr` to `goal` in `maze`"
  [maze distances path curr goal]
  (if (= curr goal)
    path
    (let [next-step (find-closer-neighbour distances
                                           (gr/get-cell-links maze curr)
                                           (get-distance distances curr))
          next-path (set-distance path next-step (get-distance distances next-step))]
      (build-path maze distances next-path next-step goal))))

;; TODO validate that start > 0,0
(defn shortest-path
  "Find the shortest path in `maze` from `start` to `goal`"
  [maze start goal]
  {:pre [(s/valid? ::gr/grid? maze)
         (s/valid? ::gr/coords start)
         (s/valid? ::gr/coords goal)]
   :post [(s/valid? ::distances? %)]}
  (let [distances (dijkstra maze goal)
        starting-distance (get-distance distances start)
        path (-> (init-distances maze)
                 (set-distance start starting-distance))]
    (build-path maze distances path start goal)))

(defn furthest-coords
  "Find the coordinates of the furthest cell in `distances`"
  [distances]
  {:pre [(s/valid? ::distances? distances)]
   :post [(s/valid? ::gr/coords %)]}
  (key (apply max-key val distances)))

(defn longest-path
  "Find the longest path within `maze`"
  [maze]
  {:pre [(s/valid? ::gr/grid? maze)]
   :post [(s/valid? ::distances? %)]}
  (let [start (furthest-coords (dijkstra maze [0 0]))
        goal (furthest-coords (dijkstra maze start))]
    (shortest-path maze start goal)))
