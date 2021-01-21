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

;; (def grid (grid/new-grid 6 6))
;; (def maze (algo/recursive-division grid {:room-p 0.1}))

;; (pr/png-out maze "images/room-test.png")

;; (def random (algo/growing-tree grid (comp first shuffle) {:start [5 5]}))
;; (pr/png-out random "images/growing-tree-random.png"
;;             {:distances (dist/dijkstra random [5 5])})

;; (def first-cell (algo/growing-tree grid first {:start [5 5]}))
;; (pr/png-out first-cell "images/growing-tree-first.png"
;;             {:distances (dist/dijkstra first-cell [5 5])})

;; (def rb (algo/recursive-backtracker grid))
;; (pr/png-out rb "images/growing-recursive.png"
;;             {:distances (dist/dijkstra rb [0 0])})
;; (def mixed (algo/growing-tree grid #(if (< (rand-int 9) 5)
;;                                       (utils/safe-rand-nth %)
;;                                       (first %)) {:start [5 5]}))
;; (pr/png-out mixed "images/growing-tree-mixed.png"
;;             {:distances (dist/dijkstra mixed [5 5])})
