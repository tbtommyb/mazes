(ns mazes.specs
  (:require
   [clojure.spec.alpha :as s]))

(def directions #{:north :south :east :west})

(s/def ::coords (s/and #(= 2 (count %))
                       #(every? int? %)))
(s/def ::coord-list (s/coll-of ::coords))
(s/def ::direction? #(contains? directions %))
(s/def ::rows pos-int?)
(s/def ::cols pos-int?)
(s/def ::cells map?)
(s/def ::links set?)
(s/def ::grid? (s/keys :req-un [::rows ::cols ::cells]))
(s/def ::cell? (s/keys :req-un [::coords ::links]))
(s/def ::cell-list? (s/coll-of ::cell?))
(s/def ::bounded-coords? (fn [[maze coords]]
                           (and (>= (first coords) 0)
                                (>= (second coords) 0)
                                (< (first coords) (:cols maze))
                                (< (second coords) (:rows maze)))))
