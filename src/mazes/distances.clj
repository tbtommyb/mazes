(ns mazes.distances
  (:require
   [mazes.grid :as gr]))

(defn init [grid]
  (reduce (fn [distances cell] (assoc distances (gr/grid-key cell) Integer/MAX_VALUE))
          {}
          (gr/iter-grid grid)))

(defn set-distance [distances cell value]
  (assoc distances (gr/grid-key cell) value))

(defn get-distance [distances cell]
  (get distances (gr/grid-key cell)))

(declare iter-dijkstra)

(defn update-dist [distances cell distance grid]
  (if (< distance (get-distance distances cell))
    (iter-dijkstra (set-distance distances cell distance) grid cell (inc distance))
    distances))

(defn iter-dijkstra [distances grid current distance]
  (reduce #(update-dist %1 %2 distance grid) distances (gr/get-linked-cells grid current)))

;; TODO: validate x and y
(defn dijkstra [grid x y]
  (-> (init grid)
      (set-distance (gr/get-cell grid x y) 0)
      (iter-dijkstra grid (gr/get-cell grid x y) 1)))
