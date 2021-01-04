(ns mazes.grid.interface)

;; TODO simply check if grid has a :mask key
(defmulti size :mask-type)
(defmulti get-cell :mask-type)
(defmulti iter-grid :mask-type)
