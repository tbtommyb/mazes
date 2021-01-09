(ns mazes.core
  (:require
   [mazes.grid.grid :as grid]
   [mazes.grid.masked :as masked]
   [mazes.printer :as pr]
   [mazes.algorithms :as algo]
   [mazes.utils :as utils]))

(def my-grid (masked/new-grid "input.txt"))
(pr/out (pr/ascii-grid (algo/recursive-backtracker (grid/new-grid 10 10))))
