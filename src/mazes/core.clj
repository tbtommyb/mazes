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

;; (def maze (algo/recursive-backtracker (polar/new-grid 20)))
;; (def distances (dist/dijkstra maze [5 18]))
;; (pr/png-out maze "images/polar-maze-distances.png" {:distances distances})
;; (pr/png-out maze "images/polar-maze.png")

;; (def square (-> (grid/new-grid 10 10)
;;                 (algo/hunt-and-kill)
;;                 (grid/braid {:p 0.3})))
;; (pr/png-out square "images/square-maze.png")

;; (def masked (algo/hunt-and-kill (masked/new-grid "mask.txt")))
;; (def masked-distances (dist/dijkstra masked [3 3]))
;; (pr/png-out masked "images/masked-maze-distances.png" {:distances masked-distances})
;; (pr/png-out masked "images/masked-maze.png")
