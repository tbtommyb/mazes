(ns mazes.mask
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [mazes.algorithms :as algo]
   [mazes.grid :as gr]))

(s/def ::mask? map?)

(defn init
  "Initialise a mask of `rows` rows and `cols` cols"
  [rows cols]
  {:pre [(s/valid? ::gr/rows rows)
         (s/valid? ::gr/cols cols)]
   :post [(s/valid? ::mask? %)]}
   (reduce #(assoc %1 %2 true) {} (gr/all-coords-for rows cols)))

(defn get-bit
  [mask coords]
  {:pre [(s/valid? ::mask? mask)
         (s/valid? ::gr/coords coords)]
   :post [(s/valid? (s/nilable boolean?) %)]}
  (get mask coords))

(defn set-bit
  [mask coords value]
  {:pre [(s/valid? ::mask? mask)
         (s/valid? ::gr/coords coords)
         (s/valid? boolean? value)]
   :post [(s/valid? ::mask? %)]}
  (assoc mask coords value))

(defn mask-count
  [mask]
  {:pre [(s/valid? ::mask? mask)]
   :post [(s/valid? pos-int? %)]}
  (count (filter true? (vals (:bits mask)))))

(defn mask-random-nth
  [mask]
  {:pre [(s/valid? ::mask? mask)]
   :post [(s/valid? ::gr/coords %)]}
  (algo/safe-rand-nth (gr/iter-visible-coords mask)))

(defn set-mask-for-coord
  [mask coord value]
  (assoc mask coord value))

(defrecord MaskedGrid [rows cols cells mask]
  gr/Grid
  (count-visible-cells [this] (mask-count mask))
  (get-visible-cell [this coord]
    (when (get-bit mask coord) (gr/get-cell this coord)))
  (iter-visible-coords [this]
    (filter (partial get-bit mask) (gr/all-coords-for rows cols)))
  (iter-visible-row-coords [this y]
    (vec (for [x (range cols) :when (get-bit mask [x y])] [x y]))))

(defn new-masked-grid
  [path]
  (let [data (str/split (slurp path) #"\n")
        rows (count data)
        cols (count (first data))
        cells (gr/init-cells rows cols)
        char-at (fn [[x y]] (get (get data (- rows (inc y))) x))
        mask (reduce #(assoc %1 %2 (not= \X (char-at %2)))
                     {}
                     (gr/all-coords-for rows cols))]
    (MaskedGrid. rows cols cells mask)))
