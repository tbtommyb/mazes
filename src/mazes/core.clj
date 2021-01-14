(ns mazes.core
  (:require
   [clojure.data.generators :as gen]
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as grid]
   [mazes.grid.masked :as masked]
   [mazes.grid.polar :as polar]
   [mazes.grid.weighted :as weighted]
   [mazes.grid.weave :as weave]
   [mazes.printer :as pr]
   [mazes.distances :as dist]
   [mazes.algorithms :as algo]
   [mazes.utils :as utils]))

;; (def maze (-> (weave/new-grid 30 30)
;;               (algo/recursive-backtracker)))
;; (pr/png-out maze "images/1-weave.png" {:inset 0.1})

;; (def square (-> (grid/new-grid 30 30)
;;               (algo/recursive-backtracker)))
;; (pr/png-out square "images/1-grid.png" {:inset 0.1})

;; (def polar (-> (polar/new-grid 10)
;;               (algo/recursive-backtracker)))
;; (pr/png-out polar "images/1-polar.png")
;; (pr/png-out square "images/weave-none.png")
;; (pr/png-out square "images/weave.png" {:inset 0.1})
;; (def masked (algo/hunt-and-kill (masked/new-grid "mask.txt")))
;; (def masked-distances (dist/dijkstra masked [3 3]))
;; (pr/png-out masked "images/masked-maze-distances.png" {:distances masked-distances})
;; (pr/png-out masked "images/masked-maze.png")
