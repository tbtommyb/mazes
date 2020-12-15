(ns mazes.distances
  (:require
   [mazes.grid :as gr]))

(defn cell-key [cell]
  "Create the key to look up a cell in grid"
  (cond
    (map? cell) (vector (:column cell) (:row cell))
    (vector? cell) cell))

(defn set-distance [distances cell value]
  (assoc distances (cell-key cell) value))

(defn get-distance [distances cell]
  (get distances (cell-key cell)))

(defn init [grid]
  (reduce #(set-distance %1 %2 Integer/MAX_VALUE)
          {}
          (gr/iter-grid grid)))

(declare iter-dijkstra)

(defn update-dist [distances cell distance grid]
  (if (< distance (get-distance distances cell))
    (iter-dijkstra (set-distance distances cell distance) grid cell (inc distance))
    distances))

(defn iter-dijkstra [distances grid current distance]
  (reduce #(update-dist %1 %2 distance grid) distances (gr/get-linked-cells grid current)))

(defn iter-dijkstra-path [distances grid current bc]
  (reduce #(update-dist %1 %2 distances grid) bc (gr/get-linked-cells grid current)))

;; TODO: validate x and y
(defn dijkstra [grid coords]
  (-> (init grid)
      (set-distance coords 0)
      (iter-dijkstra grid coords 1)))

(defn closer-neighbour [distances cells distance]
  (first (filter #(< (get-distance distances %) distance) cells)))

(defn path-between-helper [crumbs distances maze curr goal]
  (if (= curr goal)
    crumbs
    (let [next-step (closer-neighbour distances
                                      (gr/get-linked-cells maze curr)
                                      (get-distance distances curr))]
          (path-between-helper (cons next-step crumbs) distances maze next-step goal))))

;; TODO validate that start > 0,0
(defn shortest-path [distances maze start goal]
  (path-between-helper (list start) distances maze start goal))

(defn furthest-cell [distances]
  (key (apply max-key val distances)))

