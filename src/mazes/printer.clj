(ns mazes.printer
  (:require
   [clojure.string :as str]
   [mazes.grid :as gr]
   [dali.io :as io]
   [dali.layout.stack]
   [dali.layout.align]))

(defn str-row-upper [row]
  (str "|"
       (str (str/join
             ""
             (map (fn [cell] (str "   " (if (contains? (:links cell) :east) " " "|")))
                  row)))
       "\n"))

(defn str-row-lower [row]
  (str "+"
       (str (str/join
             ""
             (map (fn [cell] (str (if (contains? (:links cell) :south) "   " "---") "+"))
                  row)))
       "\n"))

(defn str-row [row]
  (str (str-row-upper row) (str-row-lower row)))

(defn str-grid [grid]
  (str "+" (apply str (repeat (:cols grid) "---+")) "\n"
       (str (str/join "" (map (fn [x] (str-row (gr/get-row grid x))) (reverse (range (:rows grid))))))))

(defn ascii [grid]
  (print (str-grid grid)))

(defn png [grid]
  (io/render-png (svg-print grid) "output.png"))

(defn svg [grid]
  (io/render-svg (svg-print grid) "output.svg"))

(def *cell-size* 50)

(defn svg-print-cell [height cell]
  (let [link? (partial gr/cell-has-link? cell)
        x1 (* *cell-size* (:column cell))
        y1 (- height (* *cell-size* (:row cell)))
        x2 (* *cell-size* (+ 1 (:column cell)))
        y2 (- height (* *cell-size* (+ 1 (:row cell))))]
    [:dali/align {:axis :left}
     [:line {:stroke (if (link? :south) :white :black)} [x1 y1] [x2 y1]]
     [:line {:stroke (if (link? :west) :white :black)} [x1 y1] [x1 y2]]]))

(defn svg-print [grid]
  (let [width (* (:cols grid) *cell-size*)
        height (* (:rows grid) *cell-size*)]
    [:dali/page {:width width :height height}
     [:rect {:fill :white}
      [0 0] [width height]]
     (map (partial svg-print-cell height) (gr/iter-grid grid))]))
