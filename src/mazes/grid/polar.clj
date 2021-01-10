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
  (cond
    (some #{to} (grid/get-neighbouring-cells grid from '(:cw))) :cw
    (some #{to} (grid/get-neighbouring-cells grid from '(:ccw))) :ccw
    (some #{to} (grid/get-neighbouring-cells grid from '(:inner))) :inner
    (some #{to} (grid/get-neighbouring-cells grid from '(:outer))) :outer))

(declare get-neighbours-helper)
;; TODO this is inefficient but works
(defn find-children-of
  [grid parent]
  (filter (fn [cell] (some #{parent} (get-neighbours-helper grid cell '(:inner))))
   (grid/iter-grid grid)))

(defn get-neighbours-at
  "Find neighbours from `src` in `direction` in `grid`"
  [grid src direction]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? src)]
   :post [(s/valid? (s/coll-of (s/nilable ::spec/cell?)) %)]}
  (if (= (cell/coords src) [0 0])
    (if (= direction :outer)
      (grid/iter-row grid 1)
      '())
    (let [[x y] (cell/coords src)
          ratio (/ (count-row (:cells grid) (cell/get-y src))
                   (count-row (:cells grid) (dec (cell/get-y src))))]
      (cond
        (= direction :cw) (list (grid/get-cell grid [(inc x) y]))
        (= direction :ccw) (list (grid/get-cell grid [(dec x) y]))
        (= direction :inner) (list (grid/get-cell grid [(int (/ x ratio)) (dec y)]))
        (= direction :outer) (find-children-of grid src)))))

(defn get-neighbours-helper
  [grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? cell)]
    :post [(s/valid? ::spec/cell-list? %)]}
   (mapcat #(remove nil? (get-neighbours-at grid cell %)) dirs))

(defmethod grid/get-neighbouring-cells :polar
  ([grid cell] (get-neighbours-helper grid cell '(:cw :ccw :inner :outer)))
  ([grid cell dirs] (get-neighbours-helper grid cell dirs)))

(defn new-grid
  "Create a polar grid with `rows` rows"
  [rows]
  {:mask-type :unmasked
   :type :polar
   :rows rows
   :cells (init-cells rows)})
