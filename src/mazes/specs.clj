(ns mazes.specs
  (:require
   [clojure.spec.alpha :as s]))

(def cartesian-dirs #{:north :south :east :west})
(def polar-dirs #{:outward :inward :cw :ccw})

(s/def ::coords (s/and #(= 2 (count %))
                       #(every? int? %)))
(s/def ::coord-list (s/coll-of ::coords))
(s/def ::cartesian-direction? #(contains? cartesian-dirs %))
(s/def ::polar-direction? #(contains? polar-dirs %))
(s/def ::rows pos-int?)
(s/def ::cols pos-int?)
(s/def ::cells map?)
(s/def ::links map?)
;; TODO tighten up definition of grid to include links
(s/def ::grid? (s/keys :req-un [::rows ::cols ::cells]))
(s/def ::cell? (s/keys :req-un [::coords ::links]))
(s/def ::cell-list? (s/coll-of ::cell?))
(s/def ::bounded-coord? (fn [[maze coord]]
                           (and (>= (first coord) 0)
                                (>= (second coord) 0)
                                (< (first coord) (:cols maze))
                                (< (second coord) (:rows maze)))))
(s/def ::distances? map?)
(s/def ::distance? (s/nilable int?))

