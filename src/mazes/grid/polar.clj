(ns mazes.grid.polar
  (:require
   [clojure.spec.alpha :as s]
   [mazes.cell.cell :as cell]
   [mazes.algorithms :as algo]
   [mazes.utils :as utils]
   [mazes.specs :as spec]
   [mazes.grid.grid :as grid]))

(def step-dir-polar {[0 1] :outer
                         [-1 0] :ccw
                         [0 -1] :inner
                         [1 0] :cw})
(def dir-step-polar {:outer [0 1]
                         :ccw [-1 0]
                         :inner [0 -1]
                         :cw [1 0]})
(defn count-row
  [cells y]
  (count (filter (fn [coord] (= y (second coord))) (keys cells))))

(defn populate-rows
  [cells row-height row-count]
  (let [f (fn [cells row]
            (let [radius (float (/ row row-count))
                  circ (* 2 Math/PI radius)
                  prev-count (count-row cells (dec row))
                  est-width (/ circ prev-count)
                  ratio (int (/ est-width row-height))
                  cell-count (* ratio prev-count)]
              (reduce #(assoc %1 [%2 row] {}) cells (range cell-count))))]
    (reduce f cells (range 1 row-count))))

(defn init-cells
  [row-count]
  (let [row-height (/ 1.0 row-count)]
    (-> {}
        (assoc [0 0] {})
        (populate-rows row-height row-count))))

;; TODO refactor
(defmethod grid/direction-between-cells :polar
  [grid from to]
  {:pre [(s/valid? ::spec/cell? from)
         (s/valid? ::spec/cell? to)]}
  (let [dx (- (cell/get-x to) (cell/get-x from))
        dy (- (cell/get-y to) (cell/get-y from))]
    (get step-dir-polar [dx dy])))

(defn get-neighbour-at
  "Find neighbour from `src` in `direction` in `grid`"
  [grid src direction]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? src)]
   :post [(s/valid? (s/nilable ::spec/cell?) %)]}
  (let [[dx dy] (get dir-step-polar direction)
        coord (vector (+ dx (cell/get-x src)) (+ dy (cell/get-y src)))]
    (grid/get-cell grid coord)))

(defn get-neighbours-helper
  [grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? cell)]
    :post [(s/valid? ::spec/cell-list? %)]}
   (keep #(get-neighbour-at grid cell %) dirs))

(defmethod grid/get-neighbouring-cells :polar
  ([grid cell] (get-neighbours-helper grid cell '(:cw :ccw :inner :outer)))
  ([grid cell dirs] (get-neighbours-helper grid cell dirs)))

(defn new
  "Create a polar grid with `rows` rows"
  [rows]
  {:mask-type :unmasked
   :type :polar
   :rows rows
   :cells (init-cells rows)})
