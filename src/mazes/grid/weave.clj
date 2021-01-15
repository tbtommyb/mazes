(ns mazes.grid.weave
  (:require
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as grid]
   [mazes.utils :as utils]
   [mazes.specs :as spec]
   [clojure.spec.alpha :as s]))

(def step-dir-weave {[0 1] :north
                     [0 2] :north-north
                     [-1 0] :west
                     [-2 0] :west-west
                     [0 -1] :south
                     [0 -2] :south-south
                     [1 0] :east
                     [2 0] :east-east})
(def dir-step-weave {:north [0 1]
                     :north-north [0 2]
                     :west [-1 0]
                     :west-west [-2 0]
                     :south [0 -1]
                     :south-south [0 -2]
                     :east [1 0]
                     :east-east [2 0]})
(def weave-dirs #{:north :north-north
                  :south :south-south
                  :east :east-east
                  :west :west-west})

(defmethod grid/get-neighbour-at [:cartesian :weave]
  [grid src direction]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? src)
         (s/valid? ::spec/weave-direction? direction)]
   :post [(s/valid? (s/nilable ::spec/cell?) %)]}
  (let [[dx dy] (get dir-step-weave direction)
        coord (vector (+ dx (cell/get-x src)) (+ dy (cell/get-y src)))]
    (grid/get-cell grid coord)))

(defn is-horizontal-passage?
  [cell]
  (and (cell/links-at cell :east)
       (cell/links-at cell :west)
       (not (cell/links-at cell :north))
       (not (cell/links-at cell :south))))

(defn is-vertical-passage?
  [cell]
  (and (cell/links-at cell :north)
       (cell/links-at cell :south)
       (not (cell/links-at cell :east))
       (not (cell/links-at cell :west))))

(defn can-tunnel-north?
  [grid cell]
  (let [northern (grid/get-neighbour-at grid cell :north)]
    (and northern
         (grid/get-neighbour-at grid cell :north-north)
         (is-horizontal-passage? northern))))

(defn can-tunnel-south?
  [grid cell]
  (let [southern (grid/get-neighbour-at grid cell :south)]
    (and southern
         (grid/get-neighbour-at grid cell :south-south)
         (is-horizontal-passage? southern))))

(defn can-tunnel-east?
  [grid cell]
  (let [eastern (grid/get-neighbour-at grid cell :east)]
    (and eastern
         (grid/get-neighbour-at grid cell :east-east)
         (is-vertical-passage? eastern))))

(defn can-tunnel-west?
  [grid cell]
  (let [western (grid/get-neighbour-at grid cell :west)]
    (and western
         (grid/get-neighbour-at grid cell :west-west)
         (is-vertical-passage? western))))

;; TODO both these methods are ugly
(defmethod grid/direction-between-cells [:cartesian :weave]
  [grid from to]
  {:pre [(s/valid? ::spec/cell? from)
         (s/valid? ::spec/cell? to)]
   :post [(s/valid? (s/nilable ::spec/weave-direction?) %)]}
  (let [dx (- (cell/get-x to) (cell/get-x from))
        dy (- (cell/get-y to) (cell/get-y from))
        dir (get step-dir-weave [dx dy])]
    (cond
      (utils/coll-contains? dir '(:north :south :east :west)) dir
      (and (= dir :north-north) (can-tunnel-north? grid from)) :north-north
      (and (= dir :south-south) (can-tunnel-south? grid from)) :south-south
      (and (= dir :east-east) (can-tunnel-east? grid from)) :east-east
      (and (= dir :west-west) (can-tunnel-west? grid from)) :west-west)))

(defmethod grid/get-neighbouring-cells [:cartesian :weave]
  ([grid cell] (grid/get-neighbouring-cells grid cell '(:north :north-north
                                                          :south :south-south
                                                          :east :east-east
                                                          :west :west-west)))
  ([grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? cell)]
    :post [(s/valid? ::spec/cell-list? %)]}
   (let [find-neighbour (fn [dir]
                  (let [neighbour (grid/get-neighbour-at grid cell dir)]
                    (when (and neighbour
                               (= dir (grid/direction-between-cells grid cell neighbour)))
                      neighbour)))]
     (keep find-neighbour dirs))))

(defmethod grid/get-linked-cells [:cartesian :weave]
  ([grid cell] (grid/get-linked-cells-helper grid cell weave-dirs))
  ([grid cell dirs] (grid/get-linked-cells-helper grid cell dirs)))

(defn new-grid
  "Create a grid of `rows` and `cols`"
  [rows cols]
  {:pre [(s/valid? ::spec/rows rows)
         (s/valid? ::spec/cols cols)]
   :post [(s/valid? ::spec/grid? %)]}
  {:type :cartesian
   :weave :weave
   :weighting :unweighted
   :rows rows
   :cols cols
   :cells (grid/init-cells rows cols)})
