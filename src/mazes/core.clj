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

;; (def my-grid (masked/new-grid "input.txt"))
;; (def my-grid (grid/new-grid 6 12))
(def my-grid (polar/new-grid 5))
(def maze (algo/recursive-backtracker my-grid))
(def distances (dist/dijkstra maze [0 0]))
(pr/polar-out maze "polar-maze.png" {:distances distances})
;; (pr/out (pr/ascii-grid maze {:distances distances}))
;; (pr/png-out maze {:distances distances})
