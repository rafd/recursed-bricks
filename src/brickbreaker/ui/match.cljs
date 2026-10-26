(ns brickbreaker.ui.match
  (:require
    [reagent.core :as r]
    [brickbreaker.game :as game]
    [brickbreaker.ui.effects :as effects]
    [brickbreaker.ui.settings :as settings]
    [brickbreaker.ui.state :as state]
    [brickbreaker.ui.toasts :as toasts]))

(def match-options
  [{:match/label "1 Player (Stable)"
    :match/key "1"
    :match/mode :mode/one-player
    :match/cursed? false}
   {:match/label "1 Player (Cursed)"
    :match/key "2"
    :match/mode :mode/one-player
    :match/cursed? true}
   {:match/label "2 Player (Stable)"
    :match/key "3"
    :match/mode :mode/two-player
    :match/cursed? false}
   {:match/label "2 Player (Cursed)"
    :match/key "4"
    :match/mode :mode/two-player
    :match/cursed? true}])

(defonce menu-index
  (r/atom 0))

(defn move-menu-cursor! [offset]
  (swap! menu-index (fn [index]
                      (mod (+ index offset) (count match-options)))))

(defn selected-match-option []
  (nth match-options @menu-index))

(defn match-option-for-key [key]
  (some (fn [match-option]
          (when (= key (:match/key match-option))
            match-option))
        match-options))

(defn start-match! [{:match/keys [mode cursed?]}]
  (let [seed (rand-int 1000000)
        stored-values (settings/clamp-slider-values (settings/load-slider-values))
        new-player-state (game/new-game {:seed seed
                                         :tuning (settings/values-in-namespace stored-values "tuning")
                                         :constants (merge (state/window-world-size mode)
                                                           (settings/values-in-namespace stored-values "constant"))})
        ids (state/player-ids mode)]
    (reset! effects/effects (zipmap ids (repeat effects/initial-effects)))
    (reset! toasts/toasts [])
    (reset! state/app-state {:app/screen :screen/playing
                             :app/mode mode
                             :app/cursed? cursed?
                             :app/players (zipmap ids (repeat new-player-state))})))

(defn new-game! []
  (let [{:app/keys [mode cursed?]} @state/app-state]
    (start-match! {:match/mode mode
                   :match/cursed? cursed?})))

(defn show-menu! []
  (swap! state/app-state assoc :app/screen :screen/menu))

(defn winners [{:app/keys [mode players]}]
  (filterv (fn [player-id]
             (= :status/won (get-in players [player-id :state/game-status])))
           (state/player-ids mode)))

(defn match-over? [app]
  (seq (winners app)))

(defn result-text [{:app/keys [mode] :as app}]
  (when (= :mode/two-player mode)
    (let [match-winners (winners app)]
      (if (= 1 (count match-winners))
        (str (state/player-label (first match-winners)) " wins!")
        "Draw!"))))

(defn match-running? [{:app/keys [screen] :as app}]
  (and (= :screen/playing screen)
       (not (match-over? app))))
