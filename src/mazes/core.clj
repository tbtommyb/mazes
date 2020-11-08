(ns mazes.core
  (:require
   [mazes.grid :as gr]
   [mazes.printer :as pr]
   [mazes.algorithms :as algo]))

(def grid (gr/init-grid 10 10))
