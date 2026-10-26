(ns brickbreaker.ui.state
  (:require
    [reagent.core :as r]))

(def world-height 100)

(defn player-ids [mode]
  (case mode
    :mode/one-player [:player/one]
    :mode/two-player [:player/one :player/two]))

(defn opponent [player-id]
  (case player-id
    :player/one :player/two
    :player/two :player/one))

(def player-label
  {:player/one "Player 1"
   :player/two "Player 2"})

(defn window-world-size [mode]
  (let [pane-width (/ js/window.innerWidth (count (player-ids mode)))]
    {:constant/world-width (* world-height (/ pane-width js/window.innerHeight))
     :constant/world-height world-height}))

(defonce app-state
  (r/atom {:app/screen :screen/menu
           :app/mode :mode/one-player
           :app/cursed? false
           :app/players {}}))

(defn player-state [player-id]
  (get-in @app-state [:app/players player-id]))

(defn update-all-players! [f]
  (swap! app-state update :app/players update-vals f))

(defonce sliders-visible?
  (r/atom false))

(defonce help-visible?
  (r/atom false))
