(ns mazes.grid.polar
  (:require
   [clojure.spec.alpha :as s]
   [mazes.algorithms :as algo]
   [mazes.utils :as utils]
   [mazes.grid.grid :as grid]))

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

(defn new
  "Create a polar grid with `rows` rows"
  [rows]
  {:mask-type :unmasked
   :rows rows
   :cells (init-cells rows)})
