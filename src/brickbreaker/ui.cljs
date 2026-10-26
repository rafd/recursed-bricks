(ns brickbreaker.ui
  (:require
    [cljs.reader :as reader]
    [clojure.set :as set]
    [clojure.string :as str]
    [reagent.core :as r]
    [reagent.dom.client :as rdc]
    [brickbreaker.constants :as constants]
    [brickbreaker.game :as game]
    [brickbreaker.physics :as physics]
    [brickbreaker.rcade :as rcade]))

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

(def slider-storage-key "brickbreaker/slider-values")

(defn load-slider-values []
  (try
    (or (some-> (js/localStorage.getItem slider-storage-key)
                reader/read-string)
        {})
    (catch :default _
      {})))

(defn save-slider-value! [key value]
  (try
    (js/localStorage.setItem slider-storage-key
                             (pr-str (assoc (load-slider-values) key value)))
    (catch :default _
      nil)))

(defn clear-slider-values! []
  (try
    (js/localStorage.removeItem slider-storage-key)
    (catch :default _
      nil)))

(defn values-in-namespace [values namespace-name]
  (into {}
        (filter (fn [[key _value]]
                  (= namespace-name (namespace key))))
        values))

(defonce app-state
  (r/atom {:app/screen :screen/menu
           :app/mode :mode/one-player
           :app/cursed? false
           :app/players {}}))

(defn player-state [player-id]
  (get-in @app-state [:app/players player-id]))

(def initial-effects
  {:effects/frame 0
   :effects/explosions []})

(defonce effects
  (r/atom {}))

(def explosion-duration 45)
(def fragment-columns 3)
(def fragment-rows 2)
(def fragment-gravity 0.03)

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

(defonce sliders-visible?
  (r/atom false))

(defonce help-visible?
  (r/atom false))

(defonce frame-rate
  (r/atom 0))

(defonce frame-rate-sample
  (atom {:sample/start-time nil
         :sample/frame-count 0}))

(def frame-rate-sample-duration 500)

(defn record-frame! [timestamp]
  (let [{:sample/keys [start-time frame-count]} (swap! frame-rate-sample update :sample/frame-count inc)]
    (cond
      (nil? start-time)
      (reset! frame-rate-sample {:sample/start-time timestamp
                                 :sample/frame-count 0})

      (>= (- timestamp start-time) frame-rate-sample-duration)
      (do
        (reset! frame-rate (/ (* 1000 frame-count) (- timestamp start-time)))
        (reset! frame-rate-sample {:sample/start-time timestamp
                                   :sample/frame-count 0})))))

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

(defn explosions-view [{:effects/keys [frame explosions]} constants]
  [:g {:pointer-events "none"}
   (for [{:explosion/keys [cell start-frame]} explosions
         :let [age (- frame start-frame)
               [_column row] cell]
         [index {:fragment/keys [x y velocity-x velocity-y spin width height]}] (map-indexed vector (fragments constants cell))]
     ^{:key (str cell "-" start-frame "-" index)}
     [:rect {:x (- (/ width 2))
             :y (- (/ height 2))
             :width width
             :height height
             :fill (brick-color row)
             :opacity (- 1 (/ age explosion-duration))
             :transform (str "translate("
                             (+ x (* velocity-x age))
                             ","
                             (+ y (* velocity-y age) (* 0.5 fragment-gravity age age))
                             ") rotate("
                             (* spin age)
                             ")")}])])

(def gravity-arrow-spacing 10)
(def gravity-arrow-length 6)
(def gravity-arrow-head-length 1)
(def gravity-arrow-head-angle (/ js/Math.PI 6))
(def gravity-for-full-length 0.025)
(def gravity-arrow-opacity 0.35)

(defn gravity-arrow-path [[x y] direction length]
  (let [half-shaft (physics/scale direction (/ length 2))
        tail (physics/subtract [x y] half-shaft)
        tip (physics/add [x y] half-shaft)
        back (physics/scale direction (- gravity-arrow-head-length))
        head-left (physics/add tip (physics/rotate back gravity-arrow-head-angle))
        head-right (physics/add tip (physics/rotate back (- gravity-arrow-head-angle)))
        point (fn [[point-x point-y]]
                (str point-x " " point-y))]
    (str "M " (point tail) " L " (point tip)
         " M " (point head-left) " L " (point tip) " L " (point head-right))))

(defn gravity-field-view [{:state/keys [tuning constants]}]
  (let [{:constant/keys [world-width world-height]} constants
        strength (js/Math.abs (:tuning/gravity tuning))
        length (* gravity-arrow-length (min 1 (/ strength gravity-for-full-length)))]
    [:g {:stroke "white"
         :stroke-width 0.25
         :stroke-linecap "round"
         :stroke-linejoin "round"
         :fill "none"
         :opacity gravity-arrow-opacity
         :pointer-events "none"}
     (for [x (range (/ gravity-arrow-spacing 2) world-width gravity-arrow-spacing)
           y (range (/ gravity-arrow-spacing 2) world-height gravity-arrow-spacing)
           :let [gravity (physics/gravity-vector {:tuning tuning
                                                  :constants constants
                                                  :location {:location/x x
                                                             :location/y y}})]
           :when (> (physics/magnitude gravity) 1e-9)]
       ^{:key (str x "-" y)}
       [:path {:d (gravity-arrow-path [x y] (physics/normalize gravity) length)}])]))

(defn bricks-view [{:state/keys [bricks-alive constants]}]
  (let [{:constant/keys [brick-width brick-height bricks-top]} constants]
    [:g
     (for [[column row] bricks-alive]
       ^{:key (str column "-" row)}
       [:rect {:x (* column brick-width)
               :y (+ bricks-top (* row brick-height))
               :width brick-width
               :height brick-height
               :fill (brick-color row)
               :stroke "#111"
               :stroke-width 0.3}])]))

(def key->slider
  (into {}
        (map (fn [slider]
               [(:slider/key slider) slider]))
        constants/tuning-sliders))

(defn clamp-to-slider [{:slider/keys [wraps?] low :slider/min high :slider/max} value]
  (if wraps?
    (+ low (mod (- value low) (- high low)))
    (-> value
        (max low)
        (min high))))

(defn clamp-slider-values [values]
  (into {}
        (map (fn [[key value]]
               (if-let [slider (key->slider key)]
                 [key (clamp-to-slider slider value)]
                 [key value])))
        values))

(defn slider-path [key]
  [(if (= "constant" (namespace key))
     :state/constants
     :state/tuning)
   key])

(defonce toasts
  (r/atom []))

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

(defn start-match! [{:match/keys [mode cursed?]}]
  (let [seed (rand-int 1000000)
        stored-values (clamp-slider-values (load-slider-values))
        new-player-state (game/new-game {:seed seed
                                         :tuning (values-in-namespace stored-values "tuning")
                                         :constants (merge (window-world-size mode)
                                                           (values-in-namespace stored-values "constant"))})
        ids (player-ids mode)]
    (reset! effects (zipmap ids (repeat initial-effects)))
    (reset! toasts [])
    (reset! app-state {:app/screen :screen/playing
                       :app/mode mode
                       :app/cursed? cursed?
                       :app/players (zipmap ids (repeat new-player-state))})))

(defn new-game! []
  (let [{:app/keys [mode cursed?]} @app-state]
    (start-match! {:match/mode mode
                   :match/cursed? cursed?})))

(defn show-menu! []
  (swap! app-state assoc :app/screen :screen/menu))

(defn update-all-players! [f]
  (swap! app-state update :app/players update-vals f))

(defn set-slider-value! [key text]
  (let [parsed (js/parseFloat text)
        value (clamp-to-slider (key->slider key) parsed)]
    (when (js/isFinite parsed)
      (update-all-players! (fn [state]
                             (assoc-in state (slider-path key) value)))
      (save-slider-value! key value))))

(defn slider-active? [state slider]
  (let [gravity-mode (:slider/gravity-mode slider)]
    (or (nil? gravity-mode)
        (= gravity-mode (get-in state [:state/tuning :tuning/gravity-mode])))))

(defn set-gravity-mode! [gravity-mode]
  (update-all-players! (fn [state]
                         (assoc-in state [:state/tuning :tuning/gravity-mode] gravity-mode)))
  (save-slider-value! :tuning/gravity-mode gravity-mode))

(defn default-slider-value [key]
  (get (merge constants/default-constants
              (window-world-size (:app/mode @app-state))
              constants/default-tuning)
       key))

(defn reset-sliders! []
  (clear-slider-values!)
  (update-all-players!
    (fn [state]
      (reduce (fn [state {:slider/keys [key]}]
                (assoc-in state (slider-path key) (default-slider-value key)))
              (assoc-in state
                        [:state/tuning :tuning/gravity-mode]
                        (:tuning/gravity-mode constants/default-tuning))
              constants/tuning-sliders))))

(defonce toast-counter
  (atom 0))

(def toast-duration 3000)

(defn show-toast! [player-id text]
  (let [id (swap! toast-counter inc)]
    (swap! toasts conj {:toast/id id
                        :toast/player player-id
                        :toast/text text})
    (js/setTimeout (fn []
                     (swap! toasts (fn [current-toasts]
                                     (filterv (fn [toast]
                                                (not= id (:toast/id toast)))
                                              current-toasts))))
                   toast-duration)))

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
  (let [path (into [:app/players player-id] (slider-path (:slider/key slider)))
        old-value (get-in @app-state path)
        new-value (clamp-to-slider slider (cursed-value slider old-value))]
    (swap! app-state assoc-in path new-value)
    (when-let [arrow (change-arrow old-value new-value)]
      (show-toast! player-id (str (short-label (:slider/label slider)) " " arrow)))))

(defn switch-gravity-mode! [player-id]
  (let [path [:app/players player-id :state/tuning :tuning/gravity-mode]
        new-mode (if (= :gravity/linear (get-in @app-state path))
                   :gravity/radial
                   :gravity/linear)]
    (swap! app-state assoc-in path new-mode)
    (show-toast! player-id (str "Gravity mode: " (str/capitalize (name new-mode))))))

(defn curse-player! [player-id]
  (let [state (player-state player-id)
        curse (->> constants/tuning-sliders
                   (filter mutable-slider?)
                   (filter (fn [slider]
                             (slider-active? state slider)))
                   (cons :curse/switch-gravity-mode)
                   rand-nth)]
    (if (= :curse/switch-gravity-mode curse)
      (switch-gravity-mode! player-id)
      (mutate-slider! player-id curse))))

(def toast-style
  (if rcade/in-cabinet?
    {:toast/bottom 3
     :toast/gap 2
     :toast/font-size 7
     :toast/padding "1px 3px"
     :toast/border-radius 3}
    {:toast/bottom 8
     :toast/gap 4
     :toast/font-size 10
     :toast/padding "3px 6px"
     :toast/border-radius 6}))

(defn toasts-view [player-id]
  (let [{:toast/keys [bottom gap font-size padding border-radius]} toast-style]
    [:div {:style {:position "absolute"
                   :bottom bottom
                   :left "50%"
                   :transform "translateX(-50%)"
                   :display "flex"
                   :flex-direction "column"
                   :gap gap
                   :font-size font-size
                   :white-space "nowrap"
                   :align-items "center"
                   :pointer-events "none"}}
     (for [{:toast/keys [id text player]} @toasts
           :when (= player-id player)]
       ^{:key id}
       [:div {:style {:padding padding
                      :background "rgba(0, 0, 0, 0.75)"
                      :border-radius border-radius}}
        text])]))

(defn tuning-slider [{:slider/keys [key label min max step needs-restart?]}]
  (let [value (get-in (player-state :player/one) (slider-path key))]
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
                           (set-slider-value! key (.. event -target -value)))}]
     [:input {:type "number"
              :style {:width 72
                      :margin-left 8}
              :step "any"
              :value value
              :on-change (fn [event]
                           (set-slider-value! key (.. event -target -value)))}]]))

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
    "Frame rate: " (.toFixed @frame-rate 0) " fps"]
   [:div {:style {:grid-column "1 / -1"}}
    [:button {:on-click (fn [event]
                          (.blur (.-currentTarget event))
                          (reset-sliders!))}
     "Reset to defaults"]]
   [:label {:style {:grid-column "1 / -1"}}
    "Gravity mode "
    [:select {:value (name (get-in (player-state :player/one) [:state/tuning :tuning/gravity-mode]))
              :on-change (fn [event]
                           (.blur (.-currentTarget event))
                           (set-gravity-mode! (keyword "gravity" (.. event -target -value))))}
     [:option {:value "linear"} "Linear"]
     [:option {:value "radial"} "Radial"]]]
   (for [slider constants/tuning-sliders
         :when (slider-active? (player-state :player/one) slider)]
     ^{:key (:slider/key slider)}
     [tuning-slider slider])])

(defn replay-button []
  [:button {:on-click (fn [event]
                        (.blur (.-currentTarget event))
                        (new-game!))}
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
                        (show-menu!))}
   (if rcade/in-cabinet?
     "Menu (B)"
     "Menu (M)")])

(def ticks-per-second 60)

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

(defn winners [{:app/keys [mode players]}]
  (filterv (fn [player-id]
             (= :status/won (get-in players [player-id :state/game-status])))
           (player-ids mode)))

(defn match-over? [app]
  (seq (winners app)))

(defn result-text [{:app/keys [mode] :as app}]
  (when (= :mode/two-player mode)
    (let [match-winners (winners app)]
      (if (= 1 (count match-winners))
        (str (player-label (first match-winners)) " wins!")
        "Draw!"))))

(defn game-over-view [app]
  (let [game-tick (get-in app [:app/players :player/one :state/game-tick])]
    [:div {:style overlay-style}
     (when-let [text (result-text app)]
       [:div {:style {:font-size 18}} text])
     [:div "Time: " (.toFixed (/ game-tick ticks-per-second) 1) " s"]
     [:div {:style {:display "flex"
                    :gap 8}}
      [replay-button]
      [back-to-menu-button]]]))

(defn menu-view []
  [:div {:style overlay-style}
   [:div {:style {:font-size 20
                  :margin-bottom 4}}
    "reCursed Bricks"]
   (for [[index {:match/keys [label key] :as match-option}] (map-indexed vector match-options)]
     ^{:key key}
     [menu-button {:label label
                   :selected? (= index @menu-index)
                   :on-click (fn []
                               (start-match! match-option))}])
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

(defn explosions-finished? [effects-by-player]
  (every? (fn [player-effects]
            (empty? (:effects/explosions player-effects)))
          (vals effects-by-player)))

(defn hud-view [app]
  [:<>
   (when (and (match-over? app)
              (explosions-finished? @effects))
     [game-over-view app])])

(defn player-view [player-id]
  (let [{:state/keys [paddle-location paddle-angle ball-location constants] :as state} (player-state player-id)
        {:constant/keys [world-width world-height paddle-width paddle-height ball-radius]} constants]
    [:div {:style {:position "relative"
                   :flex 1
                   :min-width 0
                   :border-left (when (= :player/two player-id)
                                  "2px solid #555")}}
     [:svg {:view-box (str "0 0 " world-width " " world-height)
            :style {:width "100%"
                    :height "100%"
                    :display "block"}}
      [:rect {:width world-width
              :height world-height
              :fill "#222"}]
      [gravity-field-view state]
      [bricks-view state]
      [explosions-view (get @effects player-id) constants]
      [:rect {:x (- (/ paddle-width 2))
              :y (- (/ paddle-height 2))
              :width paddle-width
              :height paddle-height
              :fill "#eee"
              :transform (str "translate("
                              (:location/x paddle-location)
                              ","
                              (:location/y paddle-location)
                              ") rotate("
                              (* paddle-angle (/ 180 js/Math.PI))
                              ")")}]
      [:circle {:cx (:location/x ball-location)
                :cy (:location/y ball-location)
                :r ball-radius
                :fill "#fff"}]]
     [toasts-view player-id]]))

(defn game-view []
  (let [{:app/keys [screen mode] :as app} @app-state]
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
         (for [player-id (player-ids mode)]
           ^{:key player-id}
           [player-view player-id])]
        [hud-view app]
        (when @sliders-visible?
          [tuning-panel])])
     (when @help-visible?
       [help-view])]))


(defn typing-in-number-field? [event]
  (= "number" (.. event -target -type)))

(defn match-option-for-key [key]
  (some (fn [match-option]
          (when (= key (:match/key match-option))
            match-option))
        match-options))

(defn on-key-down! [event]
  (cond
    (typing-in-number-field? event)
    (when (= "Enter" (.-key event))
      (.blur (.-target event)))

    (and (= "Escape" (.-key event))
         @help-visible?)
    (reset! help-visible? false)

    (contains? #{"Escape" "m" "M"} (.-key event))
    (show-menu!)

    (contains? #{"h" "H"} (.-key event))
    (swap! help-visible? not)

    (and (= :screen/playing (:app/screen @app-state))
         (contains? #{"x" "X"} (.-key event)))
    (swap! sliders-visible? not)

    (and (= :screen/menu (:app/screen @app-state))
         (match-option-for-key (.-key event)))
    (start-match! (match-option-for-key (.-key event)))

    (and (= :screen/menu (:app/screen @app-state))
         (contains? #{"ArrowUp" "ArrowDown"} (.-key event)))
    (do
      (.preventDefault event)
      (move-menu-cursor! (if (= "ArrowUp" (.-key event))
                           -1
                           1)))

    (and (= :screen/menu (:app/screen @app-state))
         (= "Enter" (.-key event)))
    (start-match! (selected-match-option))

    (and (= :screen/playing (:app/screen @app-state))
         (contains? #{"r" "R"} (.-key event)))
    (new-game!)

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
  (let [{:app/keys [screen] :as app} @app-state]
    (cond
      (= :screen/menu screen)
      (case button
        :button/up (move-menu-cursor! -1)
        :button/down (move-menu-cursor! 1)
        (when (select-buttons button)
          (start-match! (selected-match-option))))

      (and (= :screen/playing screen)
           (match-over? app)
           (explosions-finished? @effects))
      (case button
        :button/a (new-game!)
        :button/b (show-menu!)
        nil))))

(def tick-duration (/ 1000 ticks-per-second))

;; Caps catch-up after the tab was hidden or the page stalled.
(def max-ticks-per-frame 5)

;; Frame timestamps jitter slightly; without slack a 60 Hz display
;; alternates between 0 and 2 ticks per frame.
(def tick-timing-slack 1)

(defonce frame-clock
  (atom {:clock/last-time nil
         :clock/unsimulated-time 0}))

(defn advance-frame-clock [{:clock/keys [last-time unsimulated-time]} timestamp]
  (let [elapsed (if last-time
                  (- timestamp last-time)
                  0)
        available (min (+ unsimulated-time elapsed)
                       (* max-ticks-per-frame tick-duration))
        tick-count (int (Math/floor (/ (+ available tick-timing-slack) tick-duration)))]
    {:clock/last-time timestamp
     :clock/unsimulated-time (- available (* tick-count tick-duration))
     :clock/tick-count tick-count}))

(defn step-player-and-get-broken-cells! [mode player-id]
  (let [before (player-state player-id)
        after (game/step before (current-action mode player-id))
        broken-cells (set/difference (:state/bricks-alive before)
                                     (:state/bricks-alive after))]
    (swap! app-state assoc-in [:app/players player-id] after)
    (swap! effects update player-id advance-effects broken-cells)
    broken-cells))

(defn mutation-target [mode player-id]
  (case mode
    :mode/one-player player-id
    :mode/two-player (opponent player-id)))

(defn step-match! []
  (let [{:app/keys [mode cursed?]} @app-state]
    (doseq [player-id (player-ids mode)]
      (let [broken-cells (step-player-and-get-broken-cells! mode player-id)]
        (when cursed?
          (dotimes [_ (count broken-cells)]
            (curse-player! (mutation-target mode player-id))))))))

(defn match-running? [{:app/keys [screen] :as app}]
  (and (= :screen/playing screen)
       (not (match-over? app))))

(defn advance-effects-only! []
  (swap! effects update-vals (fn [player-effects]
                               (advance-effects player-effects #{}))))

(defn tick! [timestamp]
  (record-frame! timestamp)
  (let [{:clock/keys [tick-count]} (swap! frame-clock advance-frame-clock timestamp)]
    (dotimes [_ tick-count]
      (let [{:app/keys [screen] :as app} @app-state]
        (cond
          (match-running? app)
          (step-match!)

          (= :screen/playing screen)
          (advance-effects-only!)))))
  (js/requestAnimationFrame tick!))

(defonce root
  (rdc/create-root (js/document.getElementById "app")))

(defn ^:dev/after-load render! []
  (rdc/render root [game-view]))

(defonce started
  (do
    (when-not rcade/in-cabinet?
      (js/window.addEventListener "keydown" on-key-down!)
      (js/window.addEventListener "keyup" on-key-up!))
    (rcade/connect! on-rcade-press!)
    (js/requestAnimationFrame tick!)
    true))

(defn init! []
  (render!))
