(ns mazes.mask
  (:require
   [clojure.spec.alpha :as s]
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
  (loop [coords (algo/safe-rand-nth (gr/iter-coords mask))]
    (if (true? (get-bit mask coords))
      coords
      (recur (algo/safe-rand-nth (gr/iter-coords mask))))))

(defn set-mask-for-coord
  [mask coord value]
  (assoc mask coord value))

(defrecord MaskedGrid [rows cols cells mask]
  gr/Grid
  (get-cell [this coord]
    (when (get-bit mask coord)
      (gr/get-cell-simple this coord)))
  (iter-coords [this]
    (filter (partial get-bit mask) (gr/all-coords-for rows cols)))
  (iter-row [this y]
    (vec (for [x (range cols) :when (get-bit mask [x y])] [x y]))))

(defn new-masked-grid
  [rows cols]
  (let [cells (reduce #(assoc %1 %2 (set '()))
                      {}
                      (gr/all-coords-for rows cols))
        mask (-> (init rows cols) (set-mask-for-coord [1 1] false))]
    (MaskedGrid. rows cols cells mask)))
