(ns mazes.grid.polar
  (:require
   [clojure.spec.alpha :as s]
   [mazes.algorithms :as algo]
   [mazes.utils :as utils]
   [mazes.grid.grid :as grid]))

(defn init-cells
  [row-count]
  (let [row-height (/ 1.0 row-count)]
    {[0 0] {}
     [0 1] {}
     [1 1] {}
     [2 1] {}
     [3 1] {}
     [4 1] {}
     [0 2] {}
     [1 2] {}
     [2 2] {}
     [3 2] {}
     [4 2] {}
     [0 3] {}
     [1 3] {}
     [2 3] {}
     [4 3] {}
     [5 3] {}
     [6 3] {}
     [7 3] {}
     [8 3] {}
     [9 3] {}
     [10 3] {}}))

(defn new
  "Create a polar grid with `rows` rows"
  [rows]
  {:mask-type :unmasked
   :rows rows
   :cells (init-cells rows)})
