(ns mazes.printer
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [mazes.grid :as gr]
   [mazes.distances :as dist]
   [dali.io :as io]
   [dali.layout.stack]
   [dali.layout.align]))

;; TODO: clean up this entire module
(defn ascii-upper-row
  "Generate an ASCII representation of the upper half of `row`"
  [row]
  {:pre [(s/valid? ::gr/cell-list? row)]}
  (let [render-cell #(if (gr/cell-has-link? % :east) "    " "   |")]
    (flatten (list "|" (map render-cell row) "\n"))))

(defn ascii-lower-row
  "Generate an ASCII representation of the lower half of `row`"
  [row]
  {:pre [(s/valid? ::gr/cell-list? row)]}
  (let [render-cell #(if (gr/cell-has-link? % :south) "   +" "---+")]
    (flatten (list "+" (map render-cell row) "\n"))))

(defn ascii-row
  "Generate an ASCII representation of `row`"
  [row]
  (concat (ascii-upper-row row) (ascii-lower-row row)))

(defn ascii-grid
  "Generate an ASCII representation of `grid`"
  [grid]
  {:pre [(s/valid? ::gr/grid? grid)] }
  (let [top-wall (flatten (list "+" (repeat (:cols grid) "---+") "\n"))]
    (concat top-wall
         (mapcat ascii-row (reverse (gr/iter-rows-cells grid))))))

(defn ascii
  "Print an ASCII representation of `grid`"
  [grid]
  (print (str/join (ascii-grid grid))))

;; TODO: tidy up and remove duplication
;; (defn str-row-upper-distances [row distances]
;;   (str "|"
;;        (str/join ""
;;                  (map (fn [cell] (str " "
;;                                       (str (Integer/toString (get distances (gr/grid-key (first (keys cell)))) 36) " ")
;;                                       (str (if (contains? (first (vals cell)) :east) " " "|"))))
;;                         row))
;;        "\n"))

;; (defn str-row-lower-distances [row distances]
;;   (str "+"
;;        (str/join
;;              ""
;;              (map (fn [cell] (str (if (contains? (first (vals cell)) :south)
;;                                     (str "   ")
;;                                     "---") "+"))
;;                   row))
;;        "\n"))

;; (defn str-row-distances [row distances]
;;   (str (str-row-upper-distances row distances) (str-row-lower-distances row distances)))

;; (defn str-grid-distances [grid distances]
;;   (str "+" (apply str (repeat (:cols grid) "---+")) "\n"
;;        (str (str/join "" (map (fn [x] (str-row-distances (gr/iter-row-links grid x) distances)) (reverse (range (:rows grid))))))))

;; (defn ascii-distances [grid distances]
;;   (print (str-grid-distances grid distances)))

;; (defn str-distances-upper-row [row distances path]
;;   (str "|"
;;        (str/join ""
;;                  (map (fn [cell] (str " "
;;                                       (if (some #(= (first (keys cell)) %) path)
;;                                         (str (Integer/toString (get distances (gr/grid-key (first (keys cell)))) 36) " ")
;;                                         "  ")
;;                                       (str (if (contains? (first (vals cell)) :east) " " "|"))))
;;                         row))
;;        "\n"))

;; (defn str-distances-row [row distances path]
;;   (str (str-distances-upper-row row distances path) (str-row-lower-distances row distances)))

;; (defn str-distances-path [grid distances path]
;;   (str "+" (apply str (repeat (:cols grid) "---+")) "\n"
;;        (str (str/join "" (map (fn [y] (str-distances-row (gr/iter-row-links grid y) distances path)) (reverse (range (:rows grid))))))))

;; (defn ascii-path [grid distances path]
;;   (print (str-distances-path grid distances path)))

;; ;; images
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
