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
(def my-grid (polar/new-grid 3))
(def maze (algo/aldous-broder my-grid))
;; (def distances (dist/dijkstra maze [3 3]))
(pr/polar-out maze "polar-maze.png")
;; (pr/out (pr/ascii-grid maze {:distances distances}))
;; (pr/png-out maze {:distances distances})
