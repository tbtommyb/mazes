(ns mazes.core
  (:require
   ;; [mazes.algorithms :as algo]
   ;; [mazes.distances :as dist]
   [mazes.grid.masked :as grmk]
   [mazes.grid.grid :as grid]
   ;; [mazes.printer :as pr]
   [mazes.utils :as utils]))


(def masked-grid (grmk/new-masked-grid "input.txt"))

(grid/iter-grid masked-grid)
