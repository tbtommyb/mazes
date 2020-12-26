(ns mazes.utils
  (:require
   [clojure.data.generators :as gen]))

(defn remove-rand-nth
  [coll]
  (remove #{(gen/rand-nth coll)} coll))

(defn safe-rand-nth
  [coll]
  (when (not (empty? coll))
    (gen/rand-nth coll)))

(defn coll-contains?
  [x coll]
  (some #(= x %) coll))
