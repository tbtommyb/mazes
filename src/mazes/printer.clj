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

(defn ascii-grid
  "Print an ASCII representation of `grid`"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)]}
  (ascii-grid-renderer grid (fn [cell] " ")))

(defn ascii-distances
  "Print an ASCII representation of `grid` with `distances`"
  [grid distances]
  {:pre [(s/valid? ::gr/grid? grid)
         (s/valid? ::dist/distances? distances)]}
  (let [cell-renderer (fn [cell]
                        (let [distance (dist/get-distance distances (gr/grid-key cell))]
                          (if (< distance Integer/MAX_VALUE)
                            (Integer/toString distance 36)
                            " ")))]
    (ascii-grid-renderer grid cell-renderer)))

(defn out
  [ascii-output]
  (print (str/join "" ascii-output)))

;; images
;; (def cell-size 50)

;; (defn background-colour-for [distances cell]
;;   (if (nil? distances)
;;     :white
;;     (let [distance (dist/get-distance distances cell)
;;           furthest (apply max (vals distances))
;;           intensity (/ (float (- furthest distance)) furthest)
;;           dark (int (* 255 intensity))
;;           bright (int (+ (* 127 intensity) 128))]
;;       (format "rgb(%d,%d,%d)" dark bright dark))))

;; (defn svg-print-cell [height distances cell]
;;   (let [coords (first (keys cell))
;;         link? (partial gr/cell-has-link? (first (vals cell)))
;;         x1 (* cell-size (first coords))
;;         y1 (- height (* cell-size (second coords)))
;;         x2 (* cell-size (+ 1 (first coords)))
;;         y2 (- height (* cell-size (+ 1 (second coords))))
;;         colour (background-colour-for distances coords)]
;;     [:dali/align {:axis :left}
;;      [:line {:stroke (if (link? :south) colour :black)} [x1 y1] [x2 y1]]
;;      [:line {:stroke (if (link? :west) colour :black)} [x1 y1] [x1 y2]]]))

;; (defn svg-print-cell-background [height distances cell]
;;   (let [coords (first (keys cell))
;;         x1 (* cell-size (first coords))
;;         y2 (- height (* cell-size (+ 1 (second coords))))
;;         colour (background-colour-for distances coords)]
;;     [:rect {:stroke colour :fill colour}
;;      [x1 y2] [cell-size cell-size]]))

;; (defn svg-print [grid distances]
;;   (let [width (* (:cols grid) cell-size)
;;         height (* (:rows grid) cell-size)]
;;     [:dali/page {:width width :height height}
;;      [:rect {:fill :white}
;;       [0 0] [width height]]
;;      (map (partial svg-print-cell-background height distances) (gr/iter-grid-links grid))
;;      (map (partial svg-print-cell height distances) (gr/iter-grid-links grid))]))

;; (defn png [grid distances]
;;   (io/render-png (svg-print grid distances) "output.png"))

;; (defn svg [grid distances]
;;   (io/render-svg (svg-print grid distances) "output.svg"))
