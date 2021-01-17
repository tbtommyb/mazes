(ns mazes.algorithms
  (:require
   [clojure.data.generators :as gen]
   [clojure.spec.alpha :as s]
   [mazes.utils :as utils]
   [mazes.specs :as spec]
   [mazes.cell.cell :as cell]
   [mazes.grid.grid :as gr]))

(defn binary-tree
  "Generate links in `grid` using a binary tree algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (reduce #(if-let [neighbours (not-empty (gr/get-neighbouring-cells %1 %2 '(:north :east)))]
             (gr/link-cells %1 %2 (utils/safe-rand-nth neighbours))
             %1)
          grid
          (gr/iter-grid grid)))

(defn link-random-north
  "Link a random cell in `run` to the north"
  [grid run]
  (let [random-cell (utils/safe-rand-nth run)]
    (if-let [northern-neighbour (utils/safe-rand-nth (gr/get-neighbouring-cells grid random-cell '(:north)))]
      (gr/link-cells grid random-cell northern-neighbour)
      grid)))

(defn link-run
  "Link every coord in `run` into `grid`"
  [grid run]
  (reduce (fn [grid [fst snd]] (gr/link-cells grid fst snd))
          grid
          (partition 2 1 run)))

(defn connect-run
  "Link `run` into `grid` and add a random northern link"
  [grid run]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell-list? run)]
   :post [(s/valid? ::spec/grid? %)]}
  (-> grid
      (link-random-north run)
      (link-run run)))

(defn should-close-out?
  "Decide whether `cell` should close the current run"
  [grid cell]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)]}
  (and (not-empty (gr/get-neighbouring-cells grid cell '(:north)))
       (even? (gen/rand-nth '(0 1)))))

(defn generate-runs
  "Create randomly sized runs of cells from `row`"
  [grid row]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell-list? row)]
   :post [(s/valid? (s/coll-of ::spec/cell-list?) %)]}
  (partition-by #(should-close-out? grid %) row))

(defn sidewinder
  "Generate links in `grid` using a sidewinder algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (let [runs (mapcat #(generate-runs grid %) (gr/iter-rows grid))]
    (reduce connect-run grid runs)))

(defn aldous-broder
  "Generate links in `grid` using Aldous-Broder algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (loop [maze grid
         unvisited (dec (gr/size grid))
         cell (gen/rand-nth (gr/iter-grid grid))]
    (if (not (pos-int? unvisited))
      maze
      (let [neighbour (utils/safe-rand-nth (gr/get-neighbouring-cells maze cell))]
        (if (empty? (:links neighbour))
          (recur
           (gr/link-cells maze cell neighbour)
           (dec unvisited)
           neighbour)
          (recur maze unvisited neighbour))))))

(defn walk-loop-erased-path
  [grid cell unvisited]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? ::spec/cell? cell)
         (s/valid? ::spec/cell-list? unvisited)]
   :post [(s/valid? ::spec/cell-list? %)]}
  (let [cell-visited? #(not (utils/coll-contains? % unvisited))]
    (loop [curr cell
           path '()]
      (if (cell-visited? curr)
        (cons curr path)
        (recur
           (gen/rand-nth (gr/get-neighbouring-cells grid curr))
           (if (utils/coll-contains? curr path)
             (drop-while #(not= % curr) path)
             (cons curr path)))))))

(defn wilson
  "Generate links in `grid` using Wilson's algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (loop [maze grid
         unvisited (utils/remove-rand-nth (gr/iter-grid grid))
         curr (utils/safe-rand-nth unvisited)]
    (if (empty? unvisited)
      maze
      (let [next-steps (walk-loop-erased-path maze curr unvisited)
            remaining-unvisited (remove (set next-steps) unvisited)]
        (recur (link-run maze next-steps)
               remaining-unvisited
               (utils/safe-rand-nth remaining-unvisited))))))

(defn hunt-for-cell
  [grid]
  (->> grid
      gr/iter-grid
      (remove cell/visited?)
      (filter (partial gr/cell-has-visited-neighbours? grid))
      first))

(defn hunt-and-kill
  "Generate links in `grid` using the hunt and kill algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (loop [maze grid
         curr (utils/safe-rand-nth (gr/iter-grid grid))]
    (if (nil? curr)
      maze
      (if-let [unvisited-neighbour (utils/safe-rand-nth (gr/unvisited-neighbours maze curr))]
        (recur (gr/link-cells maze curr unvisited-neighbour)
               unvisited-neighbour)
        (if-let [new-curr (hunt-for-cell maze)]
          (let [visited-neighbour (utils/safe-rand-nth (gr/visited-neighbours maze new-curr))]
            (recur (gr/link-cells maze new-curr visited-neighbour)
                   new-curr))
          maze)))))

(defn recursive-backtracker
  "Generate links in `grid` using the recursive backtracking algorithm"
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (loop [maze grid
         visited [(utils/safe-rand-nth (gr/iter-grid grid))]]
    (if-let [curr (first visited)]
      (if-let [unvisited-neighbour (utils/safe-rand-nth (gr/unvisited-neighbours maze curr))]
        (recur (gr/link-cells maze curr unvisited-neighbour)
               (conj visited unvisited-neighbour))
        (recur maze (rest visited)))
      maze)))

(defn get-set-for-cell
  [state coord]
  {:pre [(s/valid? ::spec/coords coord)]}
  (get-in state [:set-for-cell coord]))

(defn can-merge?
  [state left right]
  {:pre [(s/valid? (s/nilable ::spec/coords) left)
         (s/valid? (s/nilable ::spec/coords) right)]}
  (and (and (not (nil? left))
            (not (nil? right)))
       (not= (get-set-for-cell state left) (get-set-for-cell state right))))

(defn kruskal-merge
  [state left right]
  {:pre [(s/valid? ::spec/coords left)
         (s/valid? ::spec/coords right)]}
  (let [winning-set (get-set-for-cell state left)
        losing-set (get-set-for-cell state right)
        losing-cells (or (get-in state [:cells-in-set losing-set]) (list right))]
    (-> (reduce (fn [curr-state loser]
                  (-> (update-in curr-state [:cells-in-set winning-set] #(cons loser %))
                      (assoc-in [:set-for-cell loser] winning-set)))
                state losing-cells)
        (update :cells-in-set #(dissoc % losing-set)))))

;; TODO: woah there, horsey. Let's tidy this up
;; Refactor link-cells to take coords and fetch from grid itself
(defn kruskal-add-crossing
  [[grid state] coord]
  {:pre [(s/valid? ::spec/coords coord)]}
  (let [cell (gr/get-cell grid coord)
        get-neighbour #(cell/coords (gr/get-neighbour-at grid cell %))]
    (if (or (not (can-merge? state coord (get-neighbour :east)))
            (not (can-merge? state (get-neighbour :north) coord))
            (not (can-merge? state coord (get-neighbour :south)))
            (not (can-merge? state (get-neighbour :west) coord))
            (not (can-merge? state (get-neighbour :east) (get-neighbour :west)))
            (not (can-merge? state (get-neighbour :west) (get-neighbour :east)))
            (not (can-merge? state (get-neighbour :south) (get-neighbour :north)))
            (not (can-merge? state (get-neighbour :north) (get-neighbour :south)))
            (cell/visited? cell))
      [grid state]
      (if (even? (gen/rand-nth '(0 1)))
        [(as-> grid new-grid
           (gr/add-link new-grid
                        (gr/get-neighbour-at new-grid cell :north)
                        (gr/get-neighbour-at new-grid cell :south)
                        :south-south)
           (gr/add-link new-grid
                        (gr/get-neighbour-at new-grid cell :south)
                        (gr/get-neighbour-at new-grid cell :north)
                        :north-north)
           (gr/link-cells new-grid (gr/get-neighbour-at new-grid cell :west) cell)
           (gr/link-cells new-grid cell (gr/get-neighbour-at new-grid cell :east)))
         (-> (update-in state [:neighbours] #(remove (partial utils/coll-contains? (cell/coords cell)) %))
             (kruskal-merge (get-neighbour :west) coord)
             (kruskal-merge coord (get-neighbour :east))
             (kruskal-merge (get-neighbour :north) (get-neighbour :south)))]
        [(as-> grid new-grid
           (gr/add-link new-grid
                        (gr/get-neighbour-at new-grid cell :east)
                        (gr/get-neighbour-at new-grid cell :west)
                        :west-west)
           (gr/add-link new-grid
                        (gr/get-neighbour-at new-grid cell :west)
                        (gr/get-neighbour-at new-grid cell :east)
                        :east-east)
           (gr/link-cells new-grid (gr/get-neighbour-at new-grid cell :north) cell)
           (gr/link-cells new-grid cell (gr/get-neighbour-at new-grid cell :south)))
         (-> (update-in state [:neighbours] #(remove (partial utils/coll-contains? (cell/coords cell)) %))
             (kruskal-merge (get-neighbour :north) coord)
             (kruskal-merge coord (get-neighbour :south))
             (kruskal-merge (get-neighbour :west) (get-neighbour :east)))]))))

(defn kruskal-state
  [grid]
  (let [initial {:neighbours '() :set-for-cell {} :cells-in-set {}}
        add-neighbour (fn [state cell dir]
                        (if-let [neighbour (gr/get-neighbour-at grid cell dir)]
                          (update state :neighbours #(cons (list (cell/coords cell) (cell/coords neighbour)) %))
                          state))]
    (reduce (fn [state cell]
              (let [set-length (count (:set-for-cell state))]
                (-> state
                 (assoc-in [:set-for-cell (cell/coords cell)] set-length)
                 (assoc-in [:cells-in-set set-length] (list (cell/coords cell)))
                 (add-neighbour cell :south)
                 (add-neighbour cell :east))))
            initial
            (gr/iter-grid grid))))

(defn add-random-crossings
  [grid state]
  (let [size (gr/size grid)]
    (loop [curr 1
           grid-state [grid state]]
      (if (= curr size)
        grid-state
        (let [row (int (rand (- (:rows grid) 2)))
              col (int (rand (- (:cols grid) 2)))]
          (recur (inc curr)
                 (kruskal-add-crossing grid-state [col row])))))))

(defn simple-kruskal
  [grid]
  (let [init-state (kruskal-state grid)]
    (first (reduce (fn [[maze curr-state] [left right]]
                     (if (can-merge? curr-state left right)
                       [(gr/link-cells maze (gr/get-cell maze left) (gr/get-cell maze right))
                        (kruskal-merge curr-state left right)]
                       [maze curr-state])) [grid init-state] (shuffle (:neighbours init-state))))))

(defn randomised-kruskal
  [grid]
  (let [init-state (kruskal-state grid)
        grid-state (add-random-crossings grid init-state)]
    (first (reduce (fn [[maze curr-state] [left right]]
                     (if (can-merge? curr-state left right)
                       [(gr/link-cells maze (gr/get-cell maze left) (gr/get-cell maze right))
                        (kruskal-merge curr-state left right)]
                       [maze curr-state])) grid-state (shuffle (:neighbours (second grid-state)))))))

(defn simplified-prims
  [grid & [opt]]
  (let [start (or (gr/get-cell grid (:start opt))
                  (utils/safe-rand-nth (gr/iter-grid grid)))]
    (loop [curr-grid grid
           active-cells (list start)]
      (if (empty? active-cells)
        curr-grid
        (let [cell (utils/safe-rand-nth active-cells)
              available-neighbours (filter (complement cell/visited?)
                                           (gr/get-neighbouring-cells curr-grid cell))
              neighbour (utils/safe-rand-nth available-neighbours)]
          (if (nil? neighbour)
            (recur curr-grid (remove #{cell} active-cells))
            (recur (gr/link-cells curr-grid cell neighbour)
                   (cons neighbour active-cells))))))))

(defn true-prims
  [grid & [opt]]
  (let [start (or (:start opt)
                  (utils/safe-rand-nth (map cell/coords (gr/iter-grid grid))))
        costs (reduce #(assoc %1 %2 (rand 100)) {} (map cell/coords (gr/iter-grid grid)))]
    (loop [curr-grid grid
           active-coords (list start)]
      (if (empty? active-coords)
        curr-grid
        (let [coord (apply min-key #(get costs %) active-coords)
              available-neighbours (->> (gr/get-neighbouring-cells curr-grid (gr/get-cell curr-grid coord))
                                        (filter (complement cell/visited?))
                                        (map cell/coords))]
          (if (empty? available-neighbours)
            (recur curr-grid (remove #{coord} active-coords))
            (let [neighbour-coord (apply min-key #(get costs %) available-neighbours)]
              (recur (gr/link-cells curr-grid (gr/get-cell curr-grid coord) (gr/get-cell curr-grid neighbour-coord))
                     (cons neighbour-coord active-coords)))))))))

(defn growing-tree
  [grid f & [opt]]
  (let [start (or (:start opt)
                  (utils/safe-rand-nth (map cell/coords (gr/iter-grid grid))))]
    (loop [curr-grid grid
           active-coords (list start)]
      (if (empty? active-coords)
        curr-grid
        (let [coord (f active-coords)
              available-neighbours (->> (gr/get-neighbouring-cells curr-grid (gr/get-cell curr-grid coord))
                                        (filter (complement cell/visited?))
                                        (map cell/coords))
              neighbour (utils/safe-rand-nth available-neighbours)]
          (if (nil? neighbour)
            (recur curr-grid (remove #{coord} active-coords))
            (recur (gr/link-cells curr-grid (gr/get-cell curr-grid coord) (gr/get-cell curr-grid neighbour))
                   (cons neighbour active-coords))))))))

;; Eller's algorithm
(defn make-row-state
  ([] (make-row-state 0))
  ([starting-set]
   {:cells-in-set {}
    :set-for-cell {}
    :next-set starting-set}))

(defn row-state-record
  [state set-id cell]
  {:pre [(s/valid? ::spec/eller-state? state)
         (s/valid? ::spec/next-set set-id)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/eller-state? %)]}
  (-> state
      (assoc-in [:set-for-cell (cell/get-x cell)] set-id)
      (update-in [:cells-in-set set-id] #(conj % (cell/coords cell)))))

(defn row-state-increment-cell
  [state cell]
  {:pre [(s/valid? ::spec/eller-state? state)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/eller-state? %)]}
  (-> (row-state-record state (:next-set state) cell)
      (update :next-set inc)))

(defn get-cell-set
  [state cell]
  {:pre [(s/valid? ::spec/eller-state? state)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? (s/nilable ::spec/next-set) %)]}
  (get-in state [:set-for-cell (cell/get-x cell)]))

(defn row-state-set-for
  [state cell]
  {:pre [(s/valid? ::spec/eller-state? state)
         (s/valid? ::spec/cell? cell)]
   :post [(s/valid? ::spec/eller-state? (first %))
          (s/valid? ::spec/next-set (second %))]}
  (if-let [cell-set (get-cell-set state cell)]
    [state cell-set]
    (let [new-state (row-state-increment-cell state cell)]
      [new-state (get-cell-set new-state cell)])))

(defn row-state-merge
  [state winner loser]
  {:pre [(s/valid? ::spec/eller-state? state)
         (s/valid? ::spec/next-set winner)
         (s/valid? ::spec/next-set loser)]
   :post [(s/valid? ::spec/eller-state? %)]}
  (-> (reduce (fn [state coord]
            (-> (assoc-in state [:set-for-cell (first coord)] winner)
                (update-in [:cells-in-set winner] #(conj % coord))))
            state
            (get-in state [:cells-in-set loser]))
      (update :cells-in-set #(dissoc % loser))))

(defn row-state-next
  [state]
  {:pre [(s/valid? ::spec/eller-state? state)]
   :post [(s/valid? ::spec/eller-state? %)]}
  (make-row-state (:next-set state)))

(defn row-state-each-set
  [state]
  {:pre [(s/valid? ::spec/eller-state? state)]}
  (map identity (:cells-in-set state)))

(defn should-link?
  [grid left-set right-set cell]
  {:pre [(s/valid? ::spec/next-set left-set)
         (s/valid? ::spec/next-set right-set)
         (s/valid? ::spec/cell? cell)]}
  (and (not= left-set right-set)
       (or (nil? (gr/get-neighbour-at grid cell :south))
           (= (rand-int 2) 0))))

;; TODO - monads?
(defn current-and-prior-set
  [state cell west]
  {:pre [(s/valid? ::spec/eller-state? state)
         (s/valid? ::spec/cell? cell)
         (s/valid? ::spec/cell? west)]
   :post [(s/valid? ::spec/eller-state? (first %))
          (s/valid? ::spec/next-set (second %))
          (s/valid? ::spec/next-set (nth % 2))]}
  (let [[new-state curr-set] (row-state-set-for state cell)
        [newer-state prior-set] (row-state-set-for new-state west)]
    [newer-state curr-set prior-set]))

(defn link-row-horizontally
  [grid-state row]
  {:pre [(s/valid? ::spec/eller-grid-state? grid-state)
         (s/valid? ::spec/cell-list? row)]
   :post [(s/valid? ::spec/eller-grid-state? %)]}
  (reduce (fn [[curr-grid curr-row-state] cell]
            (if-let [west (gr/get-neighbour-at curr-grid cell :west)]
              (let [[latest-row-state cell-set prior-set] (current-and-prior-set curr-row-state cell west)]
                (if (should-link? curr-grid cell-set prior-set cell)
                  [(gr/link-cells curr-grid cell west)
                   (row-state-merge latest-row-state prior-set cell-set)]
                  [curr-grid latest-row-state]))
              [curr-grid curr-row-state])) grid-state row))

(defn make-southern-connections
  [[grid row-state next-row-state] [set-id coord-list]]
  (reduce (fn [[curr-grid curr-row-state curr-next-row-state] [idx coord]]
            (if (or (= 0 idx) (= 0 (rand-int 3)))
              (let [cell (gr/get-cell curr-grid coord)
                    southern (gr/get-neighbour-at curr-grid cell :south)
                    [latest-row-state cell-set] (row-state-set-for curr-row-state cell)]
                [(gr/link-cells curr-grid cell southern)
                 latest-row-state
                 (row-state-record curr-next-row-state cell-set southern)])
              [curr-grid curr-row-state curr-next-row-state]))
          [grid row-state next-row-state]
          (map-indexed list (shuffle coord-list))))

(defn link-to-south
  [grid-state row]
  {:pre [(s/valid? ::spec/eller-grid-state? grid-state)
         (s/valid? ::spec/cell-list? row)]
   :post [(s/valid? ::spec/eller-grid-state? %)]}
  (let [[grid row-state] grid-state]
    ;; Check if it's the final row and do nothing if so
    (if (nil? (gr/get-neighbour-at grid (first row) :south))
      grid-state
      (-> (reduce make-southern-connections
                  [grid row-state (row-state-next row-state)]
                  (row-state-each-set row-state))
          (mapv [0 2])))))

(defn process-row
  [grid-state row]
  {:pre [(s/valid? ::spec/eller-grid-state? grid-state)
         (s/valid? ::spec/cell-list? row)]
   :post [(s/valid? ::spec/eller-grid-state? %)]}
  (-> grid-state
      (link-row-horizontally row)
      (link-to-south row)))

(defn eller
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (first (reduce process-row [grid (make-row-state) nil] (reverse (gr/iter-rows grid)))))

(defn link-all-cell-neighbours
  [grid cell]
  (reduce #(gr/link-cells %1 cell %2 false) grid (gr/get-neighbouring-cells grid cell)))

(defn link-every-cell
  [grid]
  (reduce link-all-cell-neighbours grid (gr/iter-grid grid)))

(declare divide-grid)

(defn divide-grid-horizontally
  [grid row col height width]
  (let [divide-south-of (utils/safe-rand-nth (range 1 height))
        passage-at (rand-int width)
        unlink-cell (fn [curr-grid n]
                      (if (= passage-at n)
                        curr-grid
                        (let [coord [(+ col n) (+ row divide-south-of)]
                              cell (gr/get-cell curr-grid coord)
                              neighbour (gr/get-neighbour-at curr-grid cell :south)]
                          (when (nil? neighbour)
                            (prn "unlink horizontal" divide-south-of coord neighbour))
                          (gr/unlink-cells curr-grid cell neighbour))))]
    (-> (reduce unlink-cell grid (range width))
        (divide-grid row col (dec divide-south-of) width)
        (divide-grid (+ row (dec divide-south-of)) col (- height (dec divide-south-of)) width))))

(defn divide-grid-vertically
  [grid row col height width]
  (let [divide-east-of (rand-int (dec width))
        passage-at (rand-int height)
        unlink-cell (fn [curr-grid n]
                      (if (= passage-at n)
                        curr-grid
                        (let [coord [(+ col divide-east-of) (+ row n)]
                              cell (gr/get-cell curr-grid coord)
                              neighbour (gr/get-neighbour-at curr-grid cell :east)]
                          (when (nil? neighbour)
                            (prn "unlink vertical" divide-east-of coord neighbour))
                          (gr/unlink-cells curr-grid cell neighbour))))]
    (-> (reduce unlink-cell grid (range height))
        (divide-grid row col height (inc divide-east-of))
        (divide-grid row (+ col 1 divide-east-of) height (- width divide-east-of 1)))))

(defn divide-grid
  [grid row col height width]
  (if (or (<= height 1) (<= width 1))
    grid
    (if (> height width)
      (divide-grid-horizontally grid row col height width)
      (divide-grid-vertically grid row col height width))))

(defn recursive-division
  [grid]
  {:pre [(s/valid? ::spec/grid? grid)]
   :post [(s/valid? ::spec/grid? %)]}
  (-> (link-every-cell grid)
      (divide-grid 0 0 (:rows grid) (:cols grid))))
