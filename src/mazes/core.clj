(ns mazes.core
  (:require
   [mazes.grid :as gr]
   [mazes.printer :as pr]
   [mazes.distances :as dist]
   [mazes.algorithms :as algo]))

(def grid (gr/init 10 10))
(def maze (algo/sidewinder grid))
(def distances (dist/dijkstra maze 0 0))

(pr/ascii-distances maze distances)
;; (pr/svg-grid grid)
