(ns mazes.grid.weighted
  (:require
   [clojure.spec.alpha :as s]
   [mazes.specs :as spec]
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as grid]))

(defn init-cells
  "Create cells for a grid of size `rows` * `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/cells %)]}
   (reduce #(assoc %1 %2 {:links {} :weight 1}) {} (grid/generate-coords rows cols)))

(defn new-grid
  "Create a grid of `rows` and `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/grid? %)]}
  {:type :cartesian
   :weighting :weighted
   :rows rows
   :cols cols
   :cells (init-cells rows cols)})
