(ns mazes.algorithms
  (:require
   [mazes.grid :as gr]))

(defn binary-tree [grid]
  (reduce (fn [grid cell]
            (let [neighbours (gr/get-neighbours grid cell '(:north :east))]
              (if (not (empty? neighbours))
                (gr/link-cells grid cell (rand-nth neighbours))
                grid)))
          grid
          (gr/iter-grid grid)))

;; TODO: utility (link-cell-dir grid cell :direction)
(defn link-random-north [grid run]
  (let [random-cell (rand-nth run)]
    (if (gr/cell-has-neighbour grid random-cell :north)
      (gr/link-cells grid random-cell (gr/cell-at-dir grid random-cell :north))
      grid)))

(defn link-east [grid run]
  (reduce (fn [grid [fst snd]] (gr/link-cells grid fst snd))
          grid
          (partition 2 1 run)))

(defn connect-run [grid run]
  (-> grid
      (link-random-north run)
      (link-east run)))

(defn should-close-out [grid cell]
  (and (gr/cell-has-neighbour grid cell :north)
       (even? (rand-nth '(0 1)))))

(defn generate-runs [grid row]
  (partition-by #(should-close-out grid %) row))

(defn sidewinder [grid]
  (let [runs (mapcat #(generate-runs grid %) (gr/iter-rows grid))]
    (reduce connect-run grid runs)))
