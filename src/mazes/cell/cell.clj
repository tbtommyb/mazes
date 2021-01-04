(ns mazes.cell.cell
  (:require
   [mazes.specs :as spec]
   [mazes.cell.cell :as cell]
   [clojure.spec.alpha :as s]))

(defn make-cell
  ([coords] (make-cell coords {}))
  ([coords links] (hash-map :coords coords :links links)))

(defn links-at
  "Returns any linked cells to `cell` in cartesian `direction`"
  [cell direction]
  {:pre [(s/valid? (s/nilable ::spec/cell?) cell)
         (s/valid? ::spec/cartesian-direction? direction)]}
  (get-in cell [:links direction]))

