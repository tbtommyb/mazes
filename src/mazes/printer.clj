(ns mazes.printer
  (:require
   [clojure.string :as str]
   [mazes.grid :as gr]))

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
