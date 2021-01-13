(ns mazes.printer
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [mazes.grid.grid :as grid]
   [mazes.cell.cell :as cell]
   [mazes.distances :as dist]
   [mazes.specs :as spec]
   [dali.io :as io]
   [dali.layout.stack]
   [dali.layout.align]))

(defmulti to-svg :type)

(defn ascii-upper-row
  "Generate an ASCII representation of the upper half of `row`"
  [grid row cell-renderer]
  {:pre [(s/valid? ::spec/cell-list? row)]}
  (let [render-cell #(if (cell/links-at % :east)
                       (format " %s  " (cell-renderer %))
                       (format " %s |" (cell-renderer %)))]
    (flatten (list "|" (map render-cell row) "\n"))))

(defn ascii-lower-row
  "Generate an ASCII representation of the lower half of `row`"
  [grid row]
  {:pre [(s/valid? ::spec/cell-list? row)]}
  (let [render-cell #(if (cell/links-at % :south) "   +" "---+")]
    (flatten (list "+" (map render-cell row) "\n"))))

(defn ascii-row
  "Generate an ASCII representation of `row`"
  [grid row cell-renderer]
  (concat (ascii-upper-row grid row cell-renderer) (ascii-lower-row grid row)))

(defn ascii-grid-renderer
  "Generate an ASCII representation of `grid` using `cell-renderer"
  [grid cell-renderer]
  (let [top-wall (flatten (list "+" (repeat (:cols grid) "---+") "\n"))]
    (concat top-wall
            (mapcat #(ascii-row grid % cell-renderer)
                    (reverse (grid/iter-rows grid {:ignore-mask true}))))))

(defn cell-renderer
  "Generate a text representation of cell, optionally using `distances`"
  [distances cell]
  {:pre [(s/valid? (s/nilable ::spec/distances?) distances)
         (s/valid? ::spec/cell? cell)]}
  (if (nil? distances)
    " "
    (let [distance (dist/get-distance distances cell)]
      (if (not (nil? distance))
        (Integer/toString distance 36)
        " "))))

(defn ascii-grid
  "Print an ASCII representation of `grid` with `distances`"
  [grid & [opt]]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? (s/nilable ::spec/distances?) (:distances opt))]}
   (let [distances (:distances opt)]
     (ascii-grid-renderer grid (partial cell-renderer distances))))

(defn out
  [ascii-output]
  (print (str/join "" ascii-output)))

;; images
(def cell-size 50)

(defn cell-coordinate-with-inset
  [x y cell-size inset]
  (let [x1 x
        x4 (+ x cell-size)
        x2 (+ x1 inset)
        x3 (- x4 inset)
        y1 y
        y4 (+ y cell-size)
        y2 (+ y1 inset)
        y3 (- y4 inset)]
    [x1 x2 x3 x4 y1 y2 y3 y4]))

(defn background-colour-for
  "Select the background colour for `cell` based on `distances` or default white"
  [distances cell]
  {:pre [(s/valid? (s/nilable ::spec/distances?) distances)
         (s/valid? ::spec/cell? cell)]}
  (if (and (not (nil? (:weight cell)))
           (> (:weight cell) 1))
    "rgb(255,0,0)"
    (if (or (nil? distances)
            (nil? (dist/get-distance distances cell)))
      :none
      (let [distance (dist/get-distance distances cell)
            furthest (apply max (remove nil? (vals distances)))
            intensity (/ (float (- furthest distance)) furthest)
            dark (unchecked-int (* 255 intensity))
            bright (unchecked-int (+ (* 127 intensity) 128))]
        (format "rgb(%d,%d,%d)" dark bright dark)))))

(defn svg-cell
  "Generate an SVG representation of a single `cell` in grid of `grid-height` coloured using `distances`"
  [grid grid-height distances cell]
  {:pre [(s/valid? pos-int? grid-height)
         (s/valid? (s/nilable ::spec/distances?) distances)
         (s/valid? ::spec/cell? cell)]}
  (let [[x y] (cell/coords cell)
        link? (fn [dir] (not-empty (grid/get-linked-cells grid cell (list dir))))
        x1 (* cell-size x)
        y1 (- grid-height (* cell-size y))
        x2 (* cell-size (inc x))
        y2 (- grid-height (* cell-size (inc y)))
        colour (background-colour-for distances cell)]
    [:dali/page
     [:line {:stroke (if (link? :north) colour :black)} [x1 y2] [x2 y2]]
     [:line {:stroke (if (link? :east) colour :black)} [x2 y1] [x2 y2]]
     [:line {:stroke (if (link? :south) colour :black)} [x1 y1] [x2 y1]]
     [:line {:stroke (if (link? :west) colour :black)} [x1 y1] [x1 y2]]]))

(defn svg-cell-inset
  "Generate an SVG representation of a single `cell` in grid of `grid-height` coloured using `distances`"
  [inset grid grid-height distances cell]
  {:pre [(s/valid? pos-int? grid-height)
         (s/valid? (s/nilable ::spec/distances?) distances)
         (s/valid? ::spec/cell? cell)]}
  (let [[x y] (map (partial * cell-size) (cell/coords cell))
        link? (fn [dir] (not-empty (grid/get-linked-cells grid cell (list dir))))
        [x1 x2 x3 x4 y1 y2 y3 y4] (cell-coordinate-with-inset x
                                                              (- (- grid-height y) cell-size)
                                                              cell-size
                                                              inset)
        colour (background-colour-for distances cell)]
    [:dali/page
     (if (link? :north)
       [:path {:stroke :black} :M [x2 y1] :L [x2 y2] :M [x3 y1] :L [x3 y2]]
       [:line {:stroke :black} [x2 y2] [x3 y2]])
     (if (link? :south)
       [:path {:stroke :black} :M [x2 y3] :L [x2 y4] :M [x3 y3] :L [x3 y4]]
       [:line {:stroke :black} [x2 y3] [x3 y3]])
     (if (link? :west)
       [:path {:stroke :black} :M [x1 y2] :L [x2 y2] :M [x1 y3] :L [x2 y3]]
       [:line {:stroke :black} [x2 y2] [x2 y3]])
     (if (link? :east)
       [:path {:stroke :black} :M [x3 y2] :L [x4 y2] :M [x3 y3] :L [x4 y3]]
       [:line {:stroke :black} [x3 y2] [x3 y3]])]))

(defn svg-cell-background
  "Generate an SVG representation of a single `cell` background in grid of `grid-height` coloured using `distancess`"
  [grid-height distances cell]
  {:pre [(s/valid? pos-int? grid-height)
         (s/valid? (s/nilable ::spec/distances?) distances)
         (s/valid? ::spec/cell? cell)]}
  (let [[x y] (cell/coords cell)
        x1 (* cell-size x)
        y2 (- grid-height (* cell-size (inc y)))
        colour (background-colour-for distances cell)]
    [:rect {:stroke colour :fill colour}
     [x1 y2] [cell-size cell-size]]))

(defmethod to-svg :cartesian
  [grid & [opt]]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? (s/nilable ::spec/distances?) (:distances opt))]}
  (let [distances (:distances opt)
        inset (* cell-size (:inset opt 0))
        width (* (:cols grid) cell-size)
        height (* (:rows grid) cell-size)
        cell-renderer (if (= 0 inset) svg-cell (partial svg-cell-inset inset))]
    [:dali/page {:width width :height height}
     [:rect {:fill :white} [0 0] [width height]]
     (map (partial svg-cell-background height distances) (grid/iter-grid grid))
     (map (partial cell-renderer grid height distances) (grid/iter-grid grid))]))

(defn png-out
  [grid filename & [opt]]
  (io/render-png (to-svg grid opt) filename))

(defn svg-out
  [grid filename & [opt]]
  (io/render-svg (to-svg grid opt) filename))

(defn svg-polar-cell
  [grid distances center theta inner-radius outer-radius idx cell]
  (if (= (cell/coords cell) [0 0])
    [:dali/page {:fill (background-colour-for distances cell)}
     [:circle {:cx center :cy center :r outer-radius}]]
    (let [theta-ccw (* theta idx)
          theta-cw (* theta (inc idx))
          link? (fn [dir] (not-empty (grid/get-linked-cells grid cell (list dir))))
          colour (background-colour-for distances cell)
          ax (+ center (unchecked-int (* inner-radius (Math/cos theta-ccw))))
          ay (+ center (unchecked-int (* inner-radius (Math/sin theta-ccw))))
          bx (+ center (unchecked-int (* outer-radius (Math/cos theta-ccw))))
          by (+ center (unchecked-int (* outer-radius (Math/sin theta-ccw))))
          cx (+ center (unchecked-int (* inner-radius (Math/cos theta-cw))))
          cy (+ center (unchecked-int (* inner-radius (Math/sin theta-cw))))
          dx (+ center (unchecked-int (* outer-radius (Math/cos theta-cw))))
          dy (+ center (unchecked-int (* outer-radius (Math/sin theta-cw))))]
      [:dali/page
       [:path {:fill colour :stroke :none}
        :M [ax ay]
        :L [bx by]
        :A [outer-radius outer-radius] theta-ccw false true [dx dy]
        :L [cx cy]
        :A [inner-radius inner-radius] theta-ccw false false [ax ay]
        :z]
       [:path {:stroke (if (link? :inner) colour :black) :fill :none} :M [ax ay] :A [inner-radius inner-radius] theta-ccw false true [cx cy]]
       [:line {:stroke (if (link? :cw) colour :black) :fill :none} [cx cy] [dx dy]]])))

(defn svg-polar-row
  [grid distances center idx row]
  (let [theta (/ (* 2 Math/PI) (count row))
        inner-radius (* idx cell-size)
        outer-radius (* (inc idx) cell-size)]
    (concat (map-indexed
             (partial svg-polar-cell grid distances center theta inner-radius outer-radius)
             row))))

(defmethod to-svg :polar
  [grid & [opt]]
  {:pre [(s/valid? ::spec/grid? grid)
         (s/valid? (s/nilable ::spec/distances?) (:distances opt))]}
  (let [distances (:distances opt)
        image-size (* 2 cell-size (:rows grid))]
    [:dali/page {:width (inc image-size) :height (inc image-size)}
     [:rect {:fill :white} [0 0] [(inc image-size) (inc image-size)]]
     (apply concat (map-indexed (partial svg-polar-row grid distances (/ image-size 2)) (grid/iter-rows grid)))
     [:circle {:fill :none :stroke :black } [(/ image-size 2) (/ image-size 2)] (* (:rows grid) cell-size)]]))
