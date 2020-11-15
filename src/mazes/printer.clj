(ns mazes.printer
  (:require
   [clojure.string :as str]
   [mazes.grid :as gr]
   [dali.io :as io]
   [dali.layout.stack]))

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

(defn ascii-print [grid]
  (print (str-grid grid)))

(defn png-print [input]
  (io/render-png input "output.png"))

(defn svg-print [input]
  (io/render-svg input "output.svg"))

(def *cell-size* 50)

(defn svg-print-cell [cell]
  (let [x1 (* *cell-size* (:column cell))
        y1 (* *cell-size* (:row cell))
        x2 (* *cell-size* (+ 1 (:column cell)))
        y2 (* *cell-size* (+ 1 (:row cell)))]
    [:dali/stack
     [:line {:stroke (if (gr/cell-has-link? cell :north) :white :black)} [x1 y1] [x2 y1]]
     [:line {:stroke (if (gr/cell-has-link? cell :west) :white :black)} [x1 y1] [x1 y2]]
     [:line {:stroke (if (gr/cell-has-link? cell :east) :white :black)} [x2 y1] [x2 y2]]
     [:line {:stroke (if (gr/cell-has-link? cell :south) :white :black)} [x1 y2] [x2 y2]]]))

(defn svg-grid [grid]
  [:dali/page {:width (* (:cols grid) *cell-size*)
               :height (* (:rows grid) *cell-size*)}
   [:rect {:fill :white}
    [0 0] [(* (:cols grid) *cell-size*) (* (:rows grid) *cell-size*)]]
   (map svg-print-cell (gr/iter-grid grid))])
