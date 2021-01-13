(ns mazes.core
  (:require
   [clojure.data.generators :as gen]
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as grid]
   [mazes.grid.masked :as masked]
   [mazes.grid.polar :as polar]
   [mazes.grid.weighted :as weighted]
   [mazes.printer :as pr]
   [mazes.distances :as dist]
   [mazes.algorithms :as algo]
   [mazes.utils :as utils]))

;; (def maze (algo/recursive-backtracker (polar/new-grid 10)))
;; (def distances (dist/dijkstra maze [5 8]))
;; (pr/png-out maze "images/polar-maze-distances.png" {:distances distances})
;; (pr/png-out maze "images/polar-maze.png")

(binding [gen/*rnd* (java.util.Random. 4)]
  (def square (-> (weighted/new-grid 3 3)
                  (algo/recursive-backtracker)
                  (grid/braid)))
  (def with-lava (assoc-in square [:cells [2 0] :weight] 50))
  (prn (dist/shortest-path square [0 0] [2 2]))
  (prn (dist/shortest-path with-lava [0 0] [2 2])))

;; (def masked (algo/hunt-and-kill (masked/new-grid "mask.txt")))
;; (def masked-distances (dist/dijkstra masked [3 3]))
;; (pr/png-out masked "images/masked-maze-distances.png" {:distances masked-distances})
;; (pr/png-out masked "images/masked-maze.png")
