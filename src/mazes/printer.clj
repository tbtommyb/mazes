(ns mazes.printer
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [mazes.grid :as gr]
   [mazes.distances :as dist]
   [dali.io :as io]
   [dali.layout.stack]
   [dali.layout.align]))

(defn ascii-upper-row
  "Generate an ASCII representation of the upper half of `row`"
  [row cell-renderer]
  {:pre [(s/valid? ::gr/cell-list? row)]}
  (let [render-cell #(if (gr/cell-has-link? % :east)
                       (format " %s  " (cell-renderer %))
                       (format " %s |" (cell-renderer %)))]
    (flatten (list "|" (map render-cell row) "\n"))))

(defn ascii-lower-row
  "Generate an ASCII representation of the lower half of `row`"
  [row]
  {:pre [(s/valid? ::gr/cell-list? row)]}
  (let [render-cell #(if (gr/cell-has-link? % :south) "   +" "---+")]
    (flatten (list "+" (map render-cell row) "\n"))))

(defn ascii-row
  "Generate an ASCII representation of `row`"
  [row cell-renderer]
  (concat (ascii-upper-row row cell-renderer) (ascii-lower-row row)))

(defn ascii-grid-renderer
  "Generate an ASCII representation of `grid` using `cell-renderer"
  [grid cell-renderer]
  (let [top-wall (flatten (list "+" (repeat (:cols grid) "---+") "\n"))]
    (concat top-wall
         (mapcat #(ascii-row % cell-renderer) (reverse (gr/iter-rows-cells grid))))))

(defn cell-renderer
  "Generate a text representation of cell, optionally using `distances`"
  [distances cell]
  {:pre [(s/valid? (s/nilable ::dist/distances?) distances)
         (s/valid? ::gr/cell? cell)]}
  (if (nil? distances)
    " "
    (let [distance (dist/get-distance distances (gr/grid-key cell))]
      (if (< distance Integer/MAX_VALUE)
        (Integer/toString distance 36)
        " "))))

(defn ascii-grid
  "Print an ASCII representation of `grid` with `distances`"
  [grid & [opt]]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? (s/nilable ::dist/distances?) (:distances opt))]}
   (let [distances (:distances opt)]
     (ascii-grid-renderer grid (partial cell-renderer distances))))

(defn out
  [ascii-output]
  (print (str/join "" ascii-output)))

;; images
(def cell-size 50)

(defn background-colour-for
  "Select the background colour for `coords` based on `distances` or default white"
  [distances coords]
  {:pre [(s/valid? (s/nilable ::dist/distances?) distances)
         (s/valid? ::gr/coords coords)]}
  (if (nil? distances)
    :white
    (let [distance (dist/get-distance distances coords)
          furthest (apply max (vals distances))
          intensity (/ (float (- furthest distance)) furthest)
          dark (int (* 255 intensity))
          bright (int (+ (* 127 intensity) 128))]
      (format "rgb(%d,%d,%d)" dark bright dark))))

(defn svg-cell
  "Generate an SVG representation of a single `cell` in grid of `grid-height` coloured using `distancess`"
  [grid-height distances cell]
  {:pre [(s/valid? pos-int? grid-height)
         (s/valid? (s/nilable ::dist/distances?) distances)
         (s/valid? ::gr/cell? cell)]}
  (let [[x y] (gr/grid-key cell)
        link? (partial gr/cell-has-link? cell)
        x1 (* cell-size x)
        y1 (- grid-height (* cell-size y))
        x2 (* cell-size (+ 1 x))
        y2 (- grid-height (* cell-size (+ 1 y)))
        colour (background-colour-for distances [x y])]
    [:dali/align {:axis :left}
     [:line {:stroke (if (link? :south) colour :black)} [x1 y1] [x2 y1]]
     [:line {:stroke (if (link? :west) colour :black)} [x1 y1] [x1 y2]]]))

(defn svg-cell-background
  "Generate an SVG representation of a single `cell` background in grid of `grid-height` coloured using `distancess`"
  [grid-height distances cell]
  {:pre [(s/valid? pos-int? grid-height)
         (s/valid? (s/nilable ::dist/distances?) distances)
         (s/valid? ::gr/cell? cell)]}
  (let [[x y] (gr/grid-key cell)
        x1 (* cell-size x)
        y2 (- grid-height (* cell-size (+ 1 y)))
        colour (background-colour-for distances [x y])]
    [:rect {:stroke colour :fill colour}
     [x1 y2] [cell-size cell-size]]))

(defn to-svg
  "Generate an SVG representation of `grid` and optionally `distances`"
  [grid distances]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? (s/nilable ::dist/distances?) distances)]}
  (let [width (* (:cols grid) cell-size)
        height (* (:rows grid) cell-size)]
    [:dali/page {:width width :height height}
     [:rect {:fill :white} [0 0] [width height]]
     (map (partial svg-cell-background height distances) (gr/iter-cells grid))
     (map (partial svg-cell height distances) (gr/iter-cells grid))]))

(defn png-out
  [grid & [opt]]
  (let [distances (:distances opt)]
    (io/render-png (to-svg grid distances) "output.png")))

(defn svg-out
  [grid & [opt]]
  (let [distances (:distances opt)]
    (io/render-svg (to-svg grid distances) "output.svg")))
