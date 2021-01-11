(ns mazes.algorithms
  (:require
   [clojure.data.generators :as gen]
   [clojure.spec.alpha :as s]
   [mazes.utils :as utils]
   [mazes.specs :as spec]
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as gr]))

(defn binary-tree
  "Generate links in `grid` using a binary tree algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (reduce #(if-let [neighbours (not-empty (gr/get-neighbouring-cells %1 %2 '(:north :east)))]
             (gr/link-cells %1 %2 (utils/safe-rand-nth neighbours))
             %1)
          grid
          (gr/iter-grid grid)))

(defn link-random-north
  "Link a random cell in `run` to the north"
  [grid run]
  (let [random-cell (utils/safe-rand-nth run)]
    (if-let [northern-neighbour (utils/safe-rand-nth (gr/get-neighbouring-cells grid random-cell '(:north)))]
      (gr/link-cells grid random-cell northern-neighbour)
      grid)))

(defn link-run
  "Link every coord in `run` into `grid`"
  [grid run]
  (reduce (fn [grid [fst snd]] (gr/link-cells grid fst snd))
          grid
          (partition 2 1 run)))

(defn connect-run
  "Link `run` into `grid` and add a random northern link"
  [grid run]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell-list? run)]
   :post [(s/valid? ::spec/grid? %)]}
  (-> grid
      (link-random-north run)
      (link-run run)))

(defn should-close-out?
  "Decide whether `cell` should close the current run"
  [grid cell]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)]}
  (and (not-empty (gr/get-neighbouring-cells grid cell '(:north)))
       (even? (gen/rand-nth '(0 1)))))

(defn generate-runs
  "Create randomly sized runs of cells from `row`"
  [grid row]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell-list? row)]
   :post [(s/valid? (s/coll-of ::spec/cell-list?) %)]}
  (partition-by #(should-close-out? grid %) row))

(defn sidewinder
  "Generate links in `grid` using a sidewinder algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (let [runs (mapcat #(generate-runs grid %) (gr/iter-rows grid))]
    (reduce connect-run grid runs)))

(defn aldous-broder
  "Generate links in `grid` using Aldous-Broder algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (loop [maze grid
         unvisited (dec (gr/size grid))
         cell (gen/rand-nth (gr/iter-grid grid))]
    (if (not (pos-int? unvisited))
      maze
      (let [neighbour (utils/safe-rand-nth (gr/get-neighbouring-cells maze cell))]
        (if (empty? (:links neighbour))
          (recur
           (gr/link-cells maze cell neighbour)
           (dec unvisited)
           neighbour)
          (recur maze unvisited neighbour))))))

(defn walk-loop-erased-path
  [grid cell unvisited]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)
         (s/valid? ::spec/cell-list? unvisited)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (let [cell-visited? #(not (utils/coll-contains? % unvisited))]
    (loop [curr cell
           path '()]
      (if (cell-visited? curr)
        (cons curr path)
        (recur
           (gen/rand-nth (gr/get-neighbouring-cells grid curr))
           (if (utils/coll-contains? curr path)
             (drop-while #(not= % curr) path)
             (cons curr path)))))))

(defn wilson
  "Generate links in `grid` using Wilson's algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (loop [maze grid
         unvisited (utils/remove-rand-nth (gr/iter-grid grid))
         curr (utils/safe-rand-nth unvisited)]
    (if (empty? unvisited)
      maze
      (let [next-steps (walk-loop-erased-path maze curr unvisited)
            remaining-unvisited (remove (set next-steps) unvisited)]
        (recur (link-run maze next-steps)
               remaining-unvisited
               (utils/safe-rand-nth remaining-unvisited))))))

(defn visited-neighbours
  [grid cell]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (filter cell/visited? (gr/get-neighbouring-cells grid cell)))

(defn unvisited-neighbours
  [grid cell]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (remove cell/visited? (gr/get-neighbouring-cells grid cell)))

(defn cell-has-visited-neighbours?
  [grid cell]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? boolean? %)]}
  (some cell/visited? (gr/get-neighbouring-cells grid cell)))

(defn hunt-for-cell
  [grid]
  (->> grid
      gr/iter-grid
      (remove cell/visited?)
      (filter (partial cell-has-visited-neighbours? grid))
      first))

(defn hunt-and-kill
  "Generate links in `grid` using the hunt and kill algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (loop [maze grid
         curr (utils/safe-rand-nth (gr/iter-grid grid))]
    (if (nil? curr)
      maze
      (if-let [unvisited-neighbour (utils/safe-rand-nth (unvisited-neighbours maze curr))]
        (recur (gr/link-cells maze curr unvisited-neighbour)
               unvisited-neighbour)
        (if-let [new-curr (hunt-for-cell maze)]
          (let [visited-neighbour (utils/safe-rand-nth (visited-neighbours maze new-curr))]
            (recur (gr/link-cells maze new-curr visited-neighbour)
                   new-curr))
          maze)))))

(defn recursive-backtracker
  "Generate links in `grid` using the recursive backtracking algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (loop [maze grid
         visited [(utils/safe-rand-nth (gr/iter-grid grid))]]
    (if-let [curr (first visited)]
      (if-let [unvisited-neighbour (utils/safe-rand-nth (unvisited-neighbours maze curr))]
        (recur (gr/link-cells maze curr unvisited-neighbour)
               (conj visited unvisited-neighbour))
        (recur maze (rest visited)))
      maze)))
