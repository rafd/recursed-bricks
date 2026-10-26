(ns brickbreaker.ui.curses
  (:require
    [clojure.string :as str]
    [brickbreaker.constants :as constants]
    [brickbreaker.ui.settings :as settings]
    [brickbreaker.ui.state :as state]
    [brickbreaker.ui.toasts :as toasts]))

(defn short-label [label]
  (str/replace label #" \(.*\)$" ""))

(defn change-arrow [old-value new-value]
  (cond
    (> new-value old-value) "↑"
    (< new-value old-value) "↓"))

(def mutation-factors [0.8 1.25])

(defn cursed-value [{:slider/keys [curse-step]} value]
  (if curse-step
    (+ value (rand-nth [(- curse-step) curse-step]))
    (* value (rand-nth mutation-factors))))

(defn mutable-slider? [slider]
  (and (not (:slider/needs-restart? slider))
       (get slider :slider/mutable? true)))

(defn mutate-slider! [player-id slider]
  (let [path (into [:app/players player-id] (settings/slider-path (:slider/key slider)))
        old-value (get-in @state/app-state path)
        new-value (settings/clamp-to-slider slider (cursed-value slider old-value))]
    (swap! state/app-state assoc-in path new-value)
    (when-let [arrow (change-arrow old-value new-value)]
      (toasts/show-toast! player-id (str (short-label (:slider/label slider)) " " arrow)))))

(defn switch-gravity-mode! [player-id]
  (let [path [:app/players player-id :state/tuning :tuning/gravity-mode]
        new-mode (if (= :gravity/linear (get-in @state/app-state path))
                   :gravity/radial
                   :gravity/linear)]
    (swap! state/app-state assoc-in path new-mode)
    (toasts/show-toast! player-id (str "Gravity mode: " (str/capitalize (name new-mode))))))

(defn curse-player! [player-id]
  (let [player-state (state/player-state player-id)
        curse (->> constants/tuning-sliders
                   (filter mutable-slider?)
                   (filter (fn [slider]
                             (settings/slider-active? player-state slider)))
                   (cons :curse/switch-gravity-mode)
                   rand-nth)]
    (if (= :curse/switch-gravity-mode curse)
      (switch-gravity-mode! player-id)
      (mutate-slider! player-id curse))))

(defn mutation-target [mode player-id]
  (case mode
    :mode/one-player player-id
    :mode/two-player (state/opponent player-id)))
