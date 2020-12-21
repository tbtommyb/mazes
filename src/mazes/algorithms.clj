(ns mazes.algorithms
  (:require
   [clojure.data.generators :as gen]
   [clojure.spec.alpha :as s]
   [mazes.grid :as gr]))

(defn binary-tree
  "Generate links in `grid` using a binary tree algorithm"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (reduce #(if-let [neighbours (not-empty (gr/get-cell-neighbours %1 %2 '(:north :east)))]
             (gr/link-cells %1 %2 (gen/rand-nth neighbours))
             %1)
          grid
          (apply concat (gr/iter-rows-coords grid))))

(defn link-random-north
  "Link a random cell in `run` to the north"
  [grid run]
  (let [random-cell (gen/rand-nth run)]
    (if-let [northern-neighbour (gr/cell-neighbour-at grid random-cell :north)]
      (gr/link-cells grid random-cell northern-neighbour)
      grid)))

(defn link-east
  "Link every coord in `run` eastwards into `grid`"
  [grid run]
  (reduce (fn [grid [fst snd]] (gr/link-cells grid fst snd))
          grid
          (partition 2 1 run)))

(defn connect-run
  "Link `run` into `grid`"
  [grid run]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coord-list run)]
   :post [(s/valid? ::gr/grid? %)]}
  (-> grid
      (link-random-north run)
      (link-east run)))

(defn should-close-out?
  "Decide whether `coord` should close the current run"
  [grid coord]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coords coord)]}
  (and (not-empty (gr/cell-neighbour-at grid coord :north))
       (even? (gen/rand-nth '(0 1)))))

(defn generate-runs
  "Create randomly sized runs of cells from `row`"
  [grid row]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coord-list row)]
   :post [(s/valid? (s/coll-of ::gr/coord-list) %)]}
  (partition-by #(should-close-out? grid %) row))

(defn sidewinder
  "Generate links in `grid` using a binary tree algorithm"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (let [runs (mapcat #(generate-runs grid %) (gr/iter-rows-coords grid))]
    (reduce connect-run grid runs)))
