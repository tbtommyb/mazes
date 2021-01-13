(ns mazes.cell.weighted
  (:require
   [mazes.cell.cell :as cell]
   [clojure.spec.alpha :as s]))

(defn make
  ([coords] (make coords {} 1))
  ([coords links weight] (hash-map :type :weighted :coords coords :links links :weight weight)))


