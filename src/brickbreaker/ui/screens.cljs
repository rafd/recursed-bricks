(ns brickbreaker.ui.screens
  (:require
    [brickbreaker.constants :as constants]
    [brickbreaker.rcade :as rcade]
    [brickbreaker.ui.board :as board]
    [brickbreaker.ui.effects :as effects]
    [brickbreaker.ui.game-loop :as game-loop]
    [brickbreaker.ui.match :as match]
    [brickbreaker.ui.settings :as settings]
    [brickbreaker.ui.state :as state]))

(defn tuning-slider [{:slider/keys [key label min max step needs-restart?]}]
  (let [value (get-in (state/player-state :player/one) (settings/slider-path key))]
    [:div
     [:div label
      (when needs-restart?
        [:span {:style {:color "#f90"}}
         " (needs game restart)"])]
     [:input {:type "range"
              :style {:width 240
                      :vertical-align "middle"}
              :min min
              :max max
              :step step
              :value value
              :on-change (fn [event]
                           (settings/set-slider-value! key (.. event -target -value)))}]
     [:input {:type "number"
              :style {:width 72
                      :margin-left 8}
              :step "any"
              :value value
              :on-change (fn [event]
                           (settings/set-slider-value! key (.. event -target -value)))}]]))

(defn tuning-panel []
  [:div {:style {:position "absolute"
                 :top 8
                 :left 8
                 :max-height "calc(100vh - 16px)"
                 :overflow-y "auto"
                 :padding 12
                 :display "grid"
                 :grid-template-columns "auto auto"
                 :gap "8px 24px"
                 :background "rgba(0, 0, 0, 0.75)"
                 :border-radius 6}}
   [:div {:style {:grid-column "1 / -1"}}
    "Frame rate: " (.toFixed @game-loop/frame-rate 0) " fps"]
   [:div {:style {:grid-column "1 / -1"}}
    [:button {:on-click (fn [event]
                          (.blur (.-currentTarget event))
                          (settings/reset-sliders!))}
     "Reset to defaults"]]
   [:label {:style {:grid-column "1 / -1"}}
    "Gravity mode "
    [:select {:value (name (get-in (state/player-state :player/one) [:state/tuning :tuning/gravity-mode]))
              :on-change (fn [event]
                           (.blur (.-currentTarget event))
                           (settings/set-gravity-mode! (keyword "gravity" (.. event -target -value))))}
     [:option {:value "linear"} "Linear"]
     [:option {:value "radial"} "Radial"]]]
   (for [slider constants/tuning-sliders
         :when (settings/slider-active? (state/player-state :player/one) slider)]
     ^{:key (:slider/key slider)}
     [tuning-slider slider])])

(defn replay-button []
  [:button {:on-click (fn [event]
                        (.blur (.-currentTarget event))
                        (match/new-game!))}
   (if rcade/in-cabinet?
     "Replay (A)"
     "Replay (R)")])

(defn menu-button [{:keys [label selected? on-click]}]
  [:button {:style {:font-size 12
                    :padding "4px 10px"
                    :border (if selected?
                              "2px solid #fc0"
                              "2px solid transparent")}
            :on-click (fn [event]
                        (.blur (.-currentTarget event))
                        (on-click))}
   label])

(defn back-to-menu-button []
  [:button {:on-click (fn [event]
                        (.blur (.-currentTarget event))
                        (match/show-menu!))}
   (if rcade/in-cabinet?
     "Menu (B)"
     "Menu (M)")])

(def overlay-style
  {:position "absolute"
   :top "50%"
   :left "50%"
   :transform "translate(-50%, -50%)"
   :display "flex"
   :flex-direction "column"
   :gap 8
   :align-items "center"
   :padding 12
   :background "rgba(0, 0, 0, 0.75)"
   :border-radius 6})

(defn game-over-view [app]
  (let [game-tick (get-in app [:app/players :player/one :state/game-tick])]
    [:div {:style overlay-style}
     (when-let [text (match/result-text app)]
       [:div {:style {:font-size 18}} text])
     [:div "Time: " (.toFixed (/ game-tick game-loop/ticks-per-second) 1) " s"]
     [:div {:style {:display "flex"
                    :gap 8}}
      [replay-button]
      [back-to-menu-button]]]))

(defn menu-view []
  [:div {:style overlay-style}
   [:div {:style {:font-size 20
                  :margin-bottom 4}}
    "reCursed Bricks"]
   (for [[index {:match/keys [label key] :as match-option}] (map-indexed vector match/match-options)]
     ^{:key key}
     [menu-button {:label label
                   :selected? (= index @match/menu-index)
                   :on-click (fn []
                               (match/start-match! match-option))}])
   (when-not rcade/in-cabinet?
     [:div {:style {:opacity 0.6
                    :font-size 10
                    :margin-top 4}}
      "H: Help / Controls"])])

(def help-rows
  [["WASD / IJKL / Arrows" "Move paddle (1 Player)"]
   ["WASD" "Move Player 1 paddle (2 Player)"]
   ["IJKL / Arrows" "Move Player 2 paddle (2 Player)"]
   ["1 – 4 / ↑↓ Enter" "Choose mode (menu)"]
   ["R" "Replay"]
   ["M / Esc" "Menu"]
   ["X" "Show / hide sliders"]
   ["H" "Show / hide help"]])

(defn help-view []
  [:div {:style (assoc overlay-style
                       :z-index 1
                       :background "rgba(0, 0, 0, 0.92)")}
   [:div {:style {:font-size 24}}
    "Controls"]
   [:div {:style {:display "grid"
                  :grid-template-columns "auto auto"
                  :gap "6px 24px"}}
    (for [[keys-text description] help-rows]
      ^{:key keys-text}
      [:<>
       [:div {:style {:font-weight "bold"}} keys-text]
       [:div description]])]])

(defn hud-view [app]
  [:<>
   (when (and (match/match-over? app)
              (effects/explosions-finished? @effects/effects))
     [game-over-view app])])

(defn game-view []
  (let [{:app/keys [screen mode] :as app} @state/app-state]
    [:div {:style {:position "relative"
                   :width "100vw"
                   :height "100vh"
                   :font-family "sans-serif"
                   :font-size 12
                   :color "#eee"
                   :background "#111"}}
     (case screen
       :screen/menu
       [menu-view]

       :screen/playing
       [:<>
        [:div {:style {:display "flex"
                       :width "100%"
                       :height "100%"}}
         (for [player-id (state/player-ids mode)]
           ^{:key player-id}
           [board/player-view player-id])]
        [hud-view app]
        (when @state/sliders-visible?
          [tuning-panel])])
     (when @state/help-visible?
       [help-view])]))
