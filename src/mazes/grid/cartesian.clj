;; (ns mazes.grid.cartesian
;;   (:require
;;    [mazes.grid.interface :as interface]
;;    [mazes.cell :as cell]
;;    [mazes.specs :as spec]
;;    [mazes.grid.grid :as common]
;;    [clojure.spec.alpha :as s]
;;    [clojure.string :as str]))

;; (def step-dir-cartesian {[0 1] :north
;;                          [-1 0] :west
;;                          [0 -1] :south
;;                          [1 0] :east})
;; (def dir-step-cartesian {:north [0 1]
;;                          :west [-1 0]
;;                          :south [0 -1]
;;                          :east [1 0]})
;; (def cartesian-dirs #{:north :south :east :west})
