(ns brickbreaker.ui.effects
  (:require
    [reagent.core :as r]))

(def initial-effects
  {:effects/frame 0
   :effects/explosions []})

(defonce effects
  (r/atom {}))

(def explosion-duration 45)
(def fragment-columns 3)
(def fragment-rows 2)
(def fragment-gravity 0.03)

(defn brick-color [row]
  (str "hsl(" (* row 36) ", 70%, 55%)"))

(defn advance-effects [{:effects/keys [frame explosions]} broken-cells]
  (let [next-frame (inc frame)]
    {:effects/frame next-frame
     :effects/explosions (-> (filterv (fn [{:explosion/keys [start-frame]}]
                                        (< (- next-frame start-frame) explosion-duration))
                                      explosions)
                             (into (map (fn [cell]
                                          {:explosion/cell cell
                                           :explosion/start-frame next-frame}))
                                   broken-cells))}))

(defn jitter
  "Deterministic pseudo-random number in [0, 1)."
  [seed]
  (/ (mod (hash seed) 1000) 1000))

(defn fragments [constants cell]
  (let [{:constant/keys [brick-width brick-height bricks-top]} constants
        [column row] cell
        fragment-width (/ brick-width fragment-columns)
        fragment-height (/ brick-height fragment-rows)
        center-x (* (+ column 0.5) brick-width)
        center-y (+ bricks-top (* (+ row 0.5) brick-height))]
    (for [fragment-column (range fragment-columns)
          fragment-row (range fragment-rows)]
      (let [offset-x (- (* (+ fragment-column 0.5) fragment-width) (/ brick-width 2))
            offset-y (- (* (+ fragment-row 0.5) fragment-height) (/ brick-height 2))
            random (jitter [cell fragment-column fragment-row])]
        {:fragment/x (+ center-x offset-x)
         :fragment/y (+ center-y offset-y)
         :fragment/velocity-x (+ (* offset-x 0.08) (* (- random 0.5) 0.3))
         :fragment/velocity-y (- (* offset-y 0.15) 0.3 (* random 0.3))
         :fragment/spin (* (- random 0.5) 30)
         :fragment/width fragment-width
         :fragment/height fragment-height}))))

(defn explosions-finished? [effects-by-player]
  (every? (fn [player-effects]
            (empty? (:effects/explosions player-effects)))
          (vals effects-by-player)))

(defn advance-effects-only! []
  (swap! effects update-vals (fn [player-effects]
                               (advance-effects player-effects #{}))))
