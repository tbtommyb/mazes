(ns mazes.algorithms
  (:require
   [clojure.data.generators :as gen]
   [clojure.spec.alpha :as s]
   [mazes.utils :as utils]
   [mazes.grid :as gr]))

(defn binary-tree
  "Generate links in `grid` using a binary tree algorithm"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (reduce #(if-let [neighbours (not-empty (gr/get-neighbouring-coords %1 %2 '(:north :east)))]
             (gr/link-cells %1 %2 (utils/safe-rand-nth neighbours))
             %1)
          grid
          (gr/iter-visible-coords grid)))

(defn link-random-north
  "Link a random cell in `run` to the north"
  [grid run]
  (let [random-cell (utils/safe-rand-nth run)]
    (if-let [northern-neighbour (gr/visible-neighbour-coords grid random-cell :north)]
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
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coord-list run)]
   :post [(s/valid? ::gr/grid? %)]}
  (-> grid
      (link-random-north run)
      (link-run run)))

(defn should-close-out?
  "Decide whether `coord` should close the current run"
  [grid coord]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coords coord)]}
  (and (not-empty (gr/visible-neighbour-coords grid coord :north))
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
  (let [runs (mapcat #(generate-runs grid %) (gr/iter-visible-rows-coords grid))]
    (reduce connect-run grid runs)))

(defn aldous-broder
  "Generate links in `grid` using Aldous-Broder algorithm"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (loop [maze grid
         unvisited (dec (gr/count-visible-cells grid))
         coords (gen/rand-nth (gr/iter-visible-coords grid))]
    (if (not (pos-int? unvisited))
      maze
      (let [neighbour (utils/safe-rand-nth (gr/get-all-neighbouring-cells maze coords))]
        (if (empty? (:links neighbour))
          (recur
           (gr/link-cells maze coords (gr/grid-key neighbour))
           (dec unvisited)
           (gr/grid-key neighbour))
          (recur maze unvisited (gr/grid-key neighbour)))))))

(defn walk-loop-erased-path
  [grid coords unvisited]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::gr/coords coords)
         (s/valid? ::gr/coord-list unvisited)]
   :post [(s/valid? ::gr/coord-list %)]}
  (let [cell-visited? #(not (utils/coll-contains? % unvisited))]
    (loop [curr coords
           path '()]
      (if (cell-visited? curr)
        (cons curr path)
        (recur
           (gen/rand-nth (gr/get-all-neighbouring-coords grid curr))
           (if (utils/coll-contains? curr path)
             (drop-while #(not= % curr) path)
             (cons curr path)))))))

(defn wilson
  "Generate links in `grid` using Wilson's algorithm"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (loop [maze grid
         unvisited (utils/remove-rand-nth (gr/iter-visible-coords grid))
         curr (utils/safe-rand-nth unvisited)]
    (if (empty? unvisited)
      maze
      (let [next-steps (walk-loop-erased-path maze curr unvisited)
            remaining-unvisited (remove (set next-steps) unvisited)]
        (recur (link-run maze next-steps)
               remaining-unvisited
               (utils/safe-rand-nth remaining-unvisited))))))

(defn visited-neighbours
  [grid coords]
  (->> (gr/get-all-neighbouring-cells grid coords)
       (filter gr/cell-visited?)
       (map gr/grid-key)))

(defn unvisited-neighbours
  [grid coords]
  (->> (gr/get-all-neighbouring-cells grid coords)
       (remove gr/cell-visited?)
       (map gr/grid-key)))

(defn cell-has-visited-neighbours?
  [grid coords]
  (->> (gr/get-all-neighbouring-cells grid coords)
       (some gr/cell-visited?)))

(defn hunt-for-cell
  [grid]
  (->> grid
      gr/iter-visible-cells
      (remove gr/cell-visited?)
      (map gr/grid-key)
      (filter (partial cell-has-visited-neighbours? grid))
      first))

(defn hunt-and-kill
  "Generate links in `grid` using the hunt and kill algorithm"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (loop [maze grid
         curr (utils/safe-rand-nth (gr/iter-visible-coords grid))]
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
  {:pre [(s/valid? ::gr/grid? grid)]
   :post [(s/valid? ::gr/grid? %)]}
  (loop [maze grid
         visited [(utils/safe-rand-nth (gr/iter-visible-coords grid))]]
    (if-let [curr (first visited)]
      (if-let [unvisited-neighbour (utils/safe-rand-nth (unvisited-neighbours maze curr))]
        (recur (gr/link-cells maze curr unvisited-neighbour)
               (conj visited unvisited-neighbour))
        (recur maze (rest visited)))
      maze)))
