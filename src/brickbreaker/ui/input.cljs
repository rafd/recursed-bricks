(ns brickbreaker.ui.input
  (:require
    [brickbreaker.rcade :as rcade]
    [brickbreaker.ui.effects :as effects]
    [brickbreaker.ui.match :as match]
    [brickbreaker.ui.state :as state]))

(defonce held-keys
  (atom #{}))

(def arrow-and-ijkl->direction
  {"ArrowLeft" :action/left
   "ArrowRight" :action/right
   "ArrowUp" :action/up
   "ArrowDown" :action/down
   "j" :action/left
   "l" :action/right
   "i" :action/up
   "k" :action/down
   "J" :action/left
   "L" :action/right
   "I" :action/up
   "K" :action/down})

(def wasd->direction
  {"a" :action/left
   "d" :action/right
   "w" :action/up
   "s" :action/down
   "A" :action/left
   "D" :action/right
   "W" :action/up
   "S" :action/down})

(def key->direction
  (merge arrow-and-ijkl->direction wasd->direction))

(defn player-key-map [mode player-id]
  (case [mode player-id]
    [:mode/one-player :player/one] key->direction
    [:mode/two-player :player/one] wasd->direction
    [:mode/two-player :player/two] arrow-and-ijkl->direction))

(defn rcade-player-numbers [mode player-id]
  (case [mode player-id]
    [:mode/one-player :player/one] [1 2]
    [:mode/two-player :player/one] [1]
    [:mode/two-player :player/two] [2]))

(defn current-action [mode player-id]
  (into (into #{} (keep (player-key-map mode player-id)) @held-keys)
        (mapcat rcade/held-actions)
        (rcade-player-numbers mode player-id)))

(defn typing-in-number-field? [event]
  (= "number" (.. event -target -type)))

(defn on-key-down! [event]
  (cond
    (typing-in-number-field? event)
    (when (= "Enter" (.-key event))
      (.blur (.-target event)))

    (and (= "Escape" (.-key event))
         @state/help-visible?)
    (reset! state/help-visible? false)

    (contains? #{"Escape" "m" "M"} (.-key event))
    (match/show-menu!)

    (contains? #{"h" "H"} (.-key event))
    (swap! state/help-visible? not)

    (and (= :screen/playing (:app/screen @state/app-state))
         (contains? #{"x" "X"} (.-key event)))
    (swap! state/sliders-visible? not)

    (and (= :screen/menu (:app/screen @state/app-state))
         (match/match-option-for-key (.-key event)))
    (match/start-match! (match/match-option-for-key (.-key event)))

    (and (= :screen/menu (:app/screen @state/app-state))
         (contains? #{"ArrowUp" "ArrowDown"} (.-key event)))
    (do
      (.preventDefault event)
      (match/move-menu-cursor! (if (= "ArrowUp" (.-key event))
                                 -1
                                 1)))

    (and (= :screen/menu (:app/screen @state/app-state))
         (= "Enter" (.-key event)))
    (match/start-match! (match/selected-match-option))

    (and (= :screen/playing (:app/screen @state/app-state))
         (contains? #{"r" "R"} (.-key event)))
    (match/new-game!)

    :else
    (do
      (when (key->direction (.-key event))
        (.preventDefault event))
      (swap! held-keys conj (.-key event)))))

(defn on-key-up! [event]
  (swap! held-keys disj (.-key event)))

(def select-buttons
  #{:button/a
    :button/one-player
    :button/two-player})

(defn on-rcade-press! [{:press/keys [button]}]
  (let [{:app/keys [screen] :as app} @state/app-state]
    (cond
      (= :screen/menu screen)
      (case button
        :button/up (match/move-menu-cursor! -1)
        :button/down (match/move-menu-cursor! 1)
        (when (select-buttons button)
          (match/start-match! (match/selected-match-option))))

      (and (= :screen/playing screen)
           (match/match-over? app)
           (effects/explosions-finished? @effects/effects))
      (case button
        :button/a (match/new-game!)
        :button/b (match/show-menu!)
        nil))))
