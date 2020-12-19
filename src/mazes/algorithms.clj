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

;; TODO: utility (link-cell-dir grid cell :direction)
;; (defn link-random-north [grid run]
;;   (let [random-cell (gen/rand-nth run)]
;;     (if (gr/cell-has-neighbour? grid random-cell :north)
;;       (gr/link-cells grid random-cell (gr/cell-at-dir grid random-cell :north))
;;       grid)))

;; (defn link-east [grid run]
;;   (reduce (fn [grid [fst snd]] (gr/link-cells grid fst snd))
;;           grid
;;           (partition 2 1 run)))

;; (defn connect-run [grid run]
;;   (-> grid
;;       (link-random-north run)
;;       (link-east run)))

;; (defn should-close-out? [grid cell]
;;   (and (gr/cell-has-neighbour? grid cell :north)
;;        (even? (gen/rand-nth '(0 1)))))

;; (defn generate-runs [grid row]
;;   (partition-by #(should-close-out? grid %) row))

;; (defn sidewinder [grid]
;;   (let [runs (mapcat #(generate-runs grid %) (gr/iter-rows grid))]
;;     (reduce connect-run grid runs)))
