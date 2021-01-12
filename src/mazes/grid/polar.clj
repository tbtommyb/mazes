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
(def polar-dirs #{:outer :inner :ccw :cw})

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
  (let [is-neighbour #(some #{(cell/coords to)}
                            (map cell/coords (grid/get-neighbouring-cells grid from (list %))))]
    (first (filter is-neighbour '(:cw :ccw :inner :outer) ))))

(defn get-outer-neighbours
  [grid src]
  (if (= (dec (cell/get-y src)) (:rows grid))
    '()
    (let [[x y] (cell/coords src)
          outer-ratio (/ (count-row (:cells grid) (inc y))
                         (count-row (:cells grid) y))]
      (if (= 1 outer-ratio)
        (list (grid/get-cell grid [x (inc y)]))
        (list (grid/get-cell grid [(* x 2) (inc y)])
              (grid/get-cell grid [(inc (* x 2)) (inc y)]))))))

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
          cols-in-row (count-row (:cells grid) y)
          ratio (/ cols-in-row (count-row (:cells grid) (dec y)))
          cell-list #(list (grid/get-cell grid %))]
      (cond
        (= direction :cw) (cell-list [(rem (inc x) cols-in-row) y])
        (= direction :ccw) (cell-list [(mod (dec x) cols-in-row) y])
        (= direction :inner) (cell-list [(int (/ x ratio)) (dec y)])
        (= direction :outer) (get-outer-neighbours grid src)))))

(defn get-neighbours-helper
  [grid cell dirs]
   {:pre [(s/valid? ::spec/grid? grid)
          (s/valid? ::spec/cell? cell)]
    :post [(s/valid? ::spec/cell-list? %)]}
   (mapcat #(remove nil? (get-neighbours-at grid cell %)) dirs))

(defmethod grid/get-neighbouring-cells :polar
  ([grid cell] (get-neighbours-helper grid cell '(:cw :ccw :inner :outer)))
  ([grid cell dirs] (get-neighbours-helper grid cell dirs)))

(defmethod grid/get-linked-cells :polar
  ([grid cell] (grid/get-linked-cells-helper grid cell polar-dirs))
  ([grid cell dirs] (grid/get-linked-cells-helper grid cell dirs)))

(defn new-grid
  "Create a polar grid with `rows` rows"
  [rows]
  {:type :polar
   :rows rows
   :cells (init-cells rows)})
