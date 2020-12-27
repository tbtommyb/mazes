(ns mazes.cell
  (:require
   [mazes.specs :as spec]
   [clojure.spec.alpha :as s]))

(def step-dir {[0 1] :north
               [-1 0] :west
               [0 -1] :south
               [1 0] :east})
(def dir-step {:north [0 1]
               :west [-1 0]
               :south [0 -1]
               :east [1 0]})

(defn grid-key
  "Create the key to look up `cell`"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/coords %)]}
  (:coords cell))

(defn make-cell
  "Make a cell from `coords` and optional `links`"
  ([coords]
   {:pre [(s/valid? ::spec/coords coords)]}
   (make-cell coords #{}))
  ([coords links]
  {:pre [(s/valid? ::spec/coords coords)
         (s/valid? ::spec/links links)]}
   (hash-map :coords coords :links links)))

(defn get-cell-x
  "Return x coordinate of `cell`"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? int? %)]}
  (get-in cell [:coords 0]))

(defn get-cell-y
  "Return y coordinate of `cell`"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? int? %)]}
  (get-in cell [:coords 1]))

(defn cell-has-link?
  "Boolean whether the `cell` has a link to `direction`"
  [cell direction]
  {:pre [(s/valid? (s/nilable ::spec/cell?) cell)
         (s/valid? ::spec/direction? direction)]
   :post [(s/valid? boolean? %)]}
  (contains? (:links cell) direction))

(defn cell-visited?
  "Boolean whether `cell` has any links"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? boolean? %)]}
  (not (empty? (:links cell))))

(defn coords-from-cell
  "Get coordinate of `direction` from `cell`"
  [cell direction]
  {:pre [(s/valid? ::spec/cell? cell)
         (s/valid? ::spec/direction? direction)]}
  (let [[dx dy] (get dir-step direction)]
    (vector (+ dx (get-cell-x cell)) (+ dy (get-cell-y cell)))))
