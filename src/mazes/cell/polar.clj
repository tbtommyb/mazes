;; (ns mazes.cell.polar
;;   (:require
;;    [mazes.cell.cell :as common]))

;; (def step-dir-polar {[0 1] :outward
;;                      [-1 0] :cw
;;                      [0 -1] :inward
;;                      [1 0] :ccw})
;; (def dir-step-polar {:outward [0 1]
;;                      :cw [-1 0]
;;                      :inward [0 -1]
;;                      :ccw [1 0]})

;; (def polar-dirs #{:outward :inward :cw :ccw})

;; (defn links-at
;;   "Returns any linked cells to `cell` in polar `direction`"
;;   [cell direction]
;;   {:pre [(s/valid? (s/nilable ::spec/cell?) cell)
;;          (s/valid? ::spec/polar-direction? direction)]}
;;   (get-in cell [:links direction]))

;; (defrecord PolarCell [coord links]
;;   common/Cell
;;   (links-at [this direction] (links-at this direction)))
