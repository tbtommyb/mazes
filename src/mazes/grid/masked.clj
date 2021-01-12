(ns mazes.grid.masked
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [mazes.specs :as spec]
   [mazes.grid.grid :as grid]))

(s/def ::mask? map?)

(defn get-bit
  [mask coord]
  {:pre [(s/valid? ::mask? mask)
         (s/valid? ::spec/coords coord)]
   :post [(s/valid? (s/nilable boolean?) %)]}
  (get mask coord))

(defn mask-count
  [mask]
  {:pre [(s/valid? ::mask? mask)]
   :post [(s/valid? pos-int? %)]}
  (count (filter true? (vals mask))))

(defn init-mask
  [coords char-at]
  (reduce #(assoc %1 %2 (not= \X (char-at %2))) {} coords))

(defn new-grid
  "Create a new grid using the mask at `path`"
  [path]
  (let [data (str/split (slurp path) #"\n")
        rows (count data)
        cols (count (first data))
        cells (grid/init-cells rows cols)
        char-at (fn [[x y]] (get (get data (- rows (inc y))) x))
        mask (init-mask (grid/generate-coords rows cols) char-at)]
    {:type :cartesian
     :rows rows
     :cols cols
     :cells cells
     :mask mask}))

(defmethod grid/size :masked [grid] (mask-count (:mask grid)))

(defmethod grid/get-cell :masked [grid coord]
  (when (get-bit (:mask grid) coord)
    (grid/get-cell-helper grid coord)))
