(ns mazes.core
  (:require
   [mazes.grid :as gr]
   [mazes.printer :as pr]
   [mazes.distances :as dist]
   [mazes.algorithms :as algo]))

(def grid (gr/init 7 7))
(def maze (algo/binary-tree grid))
(def distances (dist/dijkstra maze [0 0]))

;; TODO: sort out x and y again
(defn print-shortest-path [x y]
  (let [maze (algo/sidewinder (gr/init y x))
        distances (dist/dijkstra maze [0 0])
        start (vector 0 0)
        end (vector (dec x) (dec y))
        path (dist/shortest-path distances maze end start)]
    (pr/ascii-path maze distances path)))


;; TODO: tidy up this abomination
(defn print-longest-path [x y]
  (let [maze (algo/sidewinder (gr/init y x))
        dist1 (dist/dijkstra maze [0 0])
        new_s (dist/furthest-cell dist1)
        dist2 (dist/dijkstra maze new_s)
        new_g (dist/furthest-cell dist2)
        path (dist/shortest-path dist2 maze new_g new_s)]
    (pr/ascii-path maze dist2 path)))
