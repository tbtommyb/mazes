(ns mazes.core
  (:require
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as grid]
   [mazes.grid.masked :as masked]
   [mazes.grid.polar :as polar]
   [mazes.printer :as pr]
   [mazes.distances :as dist]
   [mazes.algorithms :as algo]
   [mazes.utils :as utils]))

(def maze (algo/recursive-backtracker (polar/new-grid 20)))
(def distances (dist/dijkstra maze [5 18]))
(pr/polar-out maze "polar-maze-distances.png" {:distances distances})
(pr/polar-out maze "polar-maze.png")
;; (pr/out (pr/ascii-grid maze {:distances distances}))
;; (pr/png-out maze {:distances distances})
;; (def my-grid (masked/new-grid "input.txt"))
;; (def my-grid (grid/new-grid 6 12))
