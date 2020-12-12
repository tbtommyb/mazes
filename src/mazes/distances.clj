(ns mazes.distances
  (:require
   [mazes.grid :as gr]))

(defn set-distance [distances cell value]
  (assoc distances (gr/grid-key cell) value))

(defn get-distance [distances cell]
  (get distances (gr/grid-key cell)))

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
(defn dijkstra [grid x y]
  (let [curr (gr/get-cell grid x y)]
    (-> (init grid)
        (set-distance curr 0)
        (iter-dijkstra grid curr 1))))

(defn closer-neighbour [distances cells distance]
  (first (filter #(< (get-distance distances %) distance) cells)))

(defn path-between [crumbs distances grid curr goal]
  (if (= curr goal)
    crumbs
    (let [next-step (closer-neighbour distances
                                      (gr/get-linked-cells grid curr)
                                      (get-distance distances curr))]
          (path-between (cons next-step crumbs) distances grid next-step goal))))

