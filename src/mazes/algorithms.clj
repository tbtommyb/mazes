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
  (reduce #(if-let [neighbours (not-empty (gr/get-neighbouring-coords %1 %2 '(:north :east)))]
             (gr/link-cells %1 %2 (gen/rand-nth neighbours))
             %1)
          grid
          (gr/iter-coords grid)))

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

(defn aldous-broder
  "Generate links in `grid` using Aldous-Broder algorithm"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (loop [maze grid
         unvisited (dec (* (:cols grid) (:rows grid)))
         coords (gen/rand-nth (gr/iter-coords grid))]
    (if (not (pos-int? unvisited))
      maze
      (let [neighbour (gen/rand-nth (gr/get-all-neighbouring-cells maze coords))]
        (if (empty? (:links neighbour))
          (recur
           (gr/link-cells maze coords (gr/grid-key neighbour))
           (dec unvisited)
           (gr/grid-key neighbour))
          (recur maze unvisited (gr/grid-key neighbour)))))))

(defn coll-contains?
  [x coll]
  (some #(= x %) coll))

(defn walk-loop-erased-path
  [grid coords unvisited]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coords coords)
         (s/valid? ::gr/coord-list unvisited)]
   :post [(s/valid? ::gr/coord-list %)]}
  (let [cell-visited? #(not (coll-contains? % unvisited))]
    (loop [curr coords
           path '()]
      (if (cell-visited? curr)
        (cons curr path)
        (recur
           (gen/rand-nth (gr/get-all-neighbouring-coords grid curr))
           (if (coll-contains? curr path)
             (drop-while #(not= % curr) path)
             (cons curr path)))))))

(defn remove-rand-nth
  [coll]
  (remove #{(gen/rand-nth coll)} coll))

(defn safe-rand-nth
  [coll]
  (when (not (empty? coll))
    (gen/rand-nth coll)))

(defn wilson
  "Generate links in `grid` using Wilson's algorithm"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (loop [maze grid
         unvisited (remove-rand-nth (gr/iter-coords grid))
         curr (safe-rand-nth unvisited)]
    (if (empty? unvisited)
      maze
      (let [next-steps (walk-loop-erased-path maze curr unvisited)
            remaining-unvisited (remove (set next-steps) unvisited)]
        (recur (link-east maze next-steps)
               remaining-unvisited
               (safe-rand-nth remaining-unvisited))))))
