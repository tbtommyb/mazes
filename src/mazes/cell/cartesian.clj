(ns mazes.cell.cartesian
  (:require
   [mazes.specs :as spec]
   [mazes.cell.cell :as common]
   [clojure.spec.alpha :as s]))

(defrecord CartesianCell [coord links]
  common/Cell
  (links-at [this direction] (links-at this direction)))

;; (defn neighbours-at-dir-polar
;;   [cell direction]
;;   {:pre [(s/valid? (s/nilable ::spec/cell?) cell)
;;          (s/valid? ::spec/polar-direction? direction)]
;;    :post [(s/valid? ::spec/coords %)]}
;;   (let [[dx dy] (get dir-step-polar direction)]
;;     (vector (+ dx (get-cell-x cell)) (+ dy (get-cell-y cell)))))

;; (defn direction-between-polar
;;   [from to]
;;   {:pre [(s/valid? ::spec/coords from)
;;          (s/valid? ::spec/coords to)]
;;    :post [(s/valid? (s/nilable ::spec/polar-direction?) %)]}
;;   (let [dx (- (first to) (first from))
;;         dy (- (second to) (second from))]
;;     (get step-dir-polar [dx dy])))

;; TODO this will break if the map has keys but no vals?
(defn cell-visited?
  "Boolean whether `cell` has any links"
  [cell]
  {:pre [(s/valid? ::spec/cell? cell)]
   :post [(s/valid? boolean? %)]}
  (not-every? empty? (vals (:links cell))))

