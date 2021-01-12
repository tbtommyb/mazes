(ns mazes.cell.cell
  (:require
   [mazes.specs :as spec]
   [mazes.cell.cell :as cell]
   [clojure.spec.alpha :as s]))

(defn make
  ([coords] (make coords {}))
  ([coords links] (hash-map :coords coords :links links)))

(defn links-at
  "Returns any linked cells to `cell` in `direction`"
  [cell direction]
  {:pre [(s/valid? (s/nilable ::spec/cell?) cell)]}
  (get-in cell [:links direction]))

(defn get-x
  "Return x coordinate of `cell`"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? int? %)]}
  (get-in cell [:coords 0]))

(defn get-y
  "Return y coordinate of `cell`"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? int? %)]}
  (get-in cell [:coords 1]))

(defn coords
  "Get coords from `cell`"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/coords %)]}
  (:coords cell))

(defn visited?
  "Boolean whether `cell` has any links"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? boolean? %)]}
  ((complement empty?) (:links cell)))

(defn links
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]}
  (apply concat (vals (:links cell))))

(defn dead-end?
  "Boolean whether `cell` is a dead end"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? boolean? %)]}
  (= 1 (count (links cell))))

(defn linked-to?
  "Determine whether `cell` is linked to `target`"
  [target cell]
  {:pre [(s/valid? ::spec/cell? cell)
         (s/valid? ::spec/cell? target)]}
  (some #{(coords target)} (links cell)))
