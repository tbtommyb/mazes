(ns mazes.core
  (:require
   [mazes.grid.grid :as grid]
   [mazes.grid.masked :as masked]
   [mazes.printer :as pr]
   [mazes.distances :as dist]
   [mazes.algorithms :as algo]
   [mazes.utils :as utils]))

;; (def my-grid (masked/new-grid "test/mazes/test-mask.txt"))
;; (def my-grid (grid/new-grid 6 6))
;; (def maze (algo/aldous-broder my-grid))
;; (def distances (dist/dijkstra maze [0 0]))
;; (pr/out (pr/ascii-grid maze {:distances distances}))
;; (pr/png-out maze {:distances distances})
