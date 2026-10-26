(ns brickbreaker.ui
  (:require
    [cljs.reader :as reader]
    [clojure.set :as set]
    [reagent.core :as r]
    [reagent.dom.client :as rdc]
    [brickbreaker.constants :as constants]
    [brickbreaker.game :as game]
    [brickbreaker.physics :as physics]))

(def world-height 100)

(defn window-world-size []
  {:constant/world-width (* world-height (/ js/window.innerWidth js/window.innerHeight))
   :constant/world-height world-height})

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

(defonce game-state
  (r/atom (let [stored-values (load-slider-values)]
            (game/new-game {:seed (rand-int 1000000)
                            :tuning (values-in-namespace stored-values "tuning")
                            :constants (merge (window-world-size)
                                              (values-in-namespace stored-values "constant"))}))))

(defonce effects
  (r/atom {:effects/frame 0
           :effects/explosions []}))

(def explosion-duration 45)
(def fragment-columns 3)
(def fragment-rows 2)
(def fragment-gravity 0.03)

(defonce held-keys
  (atom #{}))

(def key->direction
  {"ArrowLeft" :action/left
   "ArrowRight" :action/right
   "ArrowUp" :action/up
   "ArrowDown" :action/down
   "a" :action/left
   "d" :action/right
   "w" :action/up
   "s" :action/down
   "A" :action/left
   "D" :action/right
   "W" :action/up
   "S" :action/down})

(defonce sliders-visible?
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

(defn current-action []
  (into #{} (keep key->direction) @held-keys))

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


(def tuning-sliders
  [{:slider/key :tuning/paddle-acceleration
    :slider/label "Paddle acceleration"
    :slider/min 0.05
    :slider/max 1
    :slider/step 0.01}
   {:slider/key :tuning/paddle-friction
    :slider/label "Paddle friction (velocity kept per tick)"
    :slider/min 0.5
    :slider/max 1
    :slider/step 0.01}
   {:slider/key :tuning/paddle-max-speed
    :slider/label "Paddle max speed"
    :slider/min 0.5
    :slider/max 100
    :slider/step 0.5
    :slider/mutable? false}
   {:slider/key :tuning/paddle-angular-damping
    :slider/label "Paddle angular damping (spin kept per tick)"
    :slider/min 0.5
    :slider/max 1
    :slider/step 0.01}
   {:slider/key :tuning/ball-initial-speed
    :slider/label "Ball initial speed"
    :slider/min 0.05
    :slider/max 4
    :slider/step 0.05
    :slider/mutable? false}
   {:slider/key :tuning/ball-max-speed
    :slider/label "Ball max speed"
    :slider/min 0.5
    :slider/max 100
    :slider/step 0.5
    :slider/mutable? false}
   {:slider/key :tuning/gravity
    :slider/label "Gravity"
    :slider/min -0.03
    :slider/max 0.03
    :slider/step 0.0005}
   {:slider/key :tuning/gravity-angle
    :slider/label "Gravity angle (degrees, 90 is down)"
    :slider/min 0
    :slider/max 360
    :slider/step 1
    :slider/gravity-mode :gravity/linear}
   {:slider/key :tuning/gravity-center-x
    :slider/label "Gravity center x (fraction of width)"
    :slider/min 0
    :slider/max 1
    :slider/step 0.01
    :slider/gravity-mode :gravity/radial}
   {:slider/key :tuning/gravity-center-y
    :slider/label "Gravity center y (fraction of height)"
    :slider/min 0
    :slider/max 1
    :slider/step 0.01
    :slider/gravity-mode :gravity/radial}
   {:slider/key :tuning/ball-drag
    :slider/label "Ball drag"
    :slider/min 0
    :slider/max 0.01
    :slider/step 0.0001}
   {:slider/key :tuning/ball-mass
    :slider/label "Ball mass"
    :slider/min 0.1
    :slider/max 10
    :slider/step 0.1}
   {:slider/key :tuning/paddle-mass
    :slider/label "Paddle mass"
    :slider/min 1
    :slider/max 200
    :slider/step 1}
   {:slider/key :tuning/restitution
    :slider/label "Restitution (bounciness)"
    :slider/min 0
    :slider/max 1
    :slider/step 0.05}
   {:slider/key :constant/world-width
    :slider/label "World width"
    :slider/min 50
    :slider/max 400
    :slider/step 1
    :slider/needs-restart? true}
   {:slider/key :constant/world-height
    :slider/label "World height"
    :slider/min 50
    :slider/max 400
    :slider/step 1
    :slider/needs-restart? true}
   {:slider/key :constant/paddle-width
    :slider/label "Paddle width"
    :slider/min 2
    :slider/max 50
    :slider/step 0.5}
   {:slider/key :constant/paddle-height
    :slider/label "Paddle height"
    :slider/min 0.5
    :slider/max 10
    :slider/step 0.5}
   {:slider/key :constant/ball-radius
    :slider/label "Ball radius"
    :slider/min 0.25
    :slider/max 5
    :slider/step 0.25}
   {:slider/key :constant/brick-columns
    :slider/label "Brick columns"
    :slider/min 1
    :slider/max 20
    :slider/step 1
    :slider/needs-restart? true}
   {:slider/key :constant/brick-rows
    :slider/label "Brick rows"
    :slider/min 1
    :slider/max 20
    :slider/step 1
    :slider/needs-restart? true}
   {:slider/key :constant/brick-height-ratio
    :slider/label "Brick height ratio"
    :slider/min 0.01
    :slider/max 0.1
    :slider/step 0.005
    :slider/needs-restart? true}
   {:slider/key :constant/brick-fill-ratio
    :slider/label "Brick fill ratio"
    :slider/min 0
    :slider/max 1
    :slider/step 0.05
    :slider/needs-restart? true}
   {:slider/key :constant/bricks-top
    :slider/label "Bricks top"
    :slider/min 0
    :slider/max 50
    :slider/step 1
    :slider/mutable? false}])

(defn slider-path [key]
  [(if (= "constant" (namespace key))
     :state/constants
     :state/tuning)
   key])

(defn new-game! []
  (reset! game-state (game/new-game {:seed (rand-int 1000000)
                                     :tuning (:state/tuning @game-state)
                                     :constants (:state/constants @game-state)})))

(defn set-slider-value! [key text]
  (let [value (js/parseFloat text)]
    (when (js/isFinite value)
      (swap! game-state assoc-in (slider-path key) value)
      (save-slider-value! key value))))

(defn slider-active? [state slider]
  (let [gravity-mode (:slider/gravity-mode slider)]
    (or (nil? gravity-mode)
        (= gravity-mode (get-in state [:state/tuning :tuning/gravity-mode])))))

(defn set-gravity-mode! [gravity-mode]
  (swap! game-state assoc-in [:state/tuning :tuning/gravity-mode] gravity-mode)
  (save-slider-value! :tuning/gravity-mode gravity-mode))

(defn default-slider-value [key]
  (get (merge constants/default-constants
              (window-world-size)
              constants/default-tuning)
       key))

(defn reset-sliders! []
  (clear-slider-values!)
  (swap! game-state
         (fn [state]
           (reduce (fn [state {:slider/keys [key]}]
                     (assoc-in state (slider-path key) (default-slider-value key)))
                   (assoc-in state
                             [:state/tuning :tuning/gravity-mode]
                             (:tuning/gravity-mode constants/default-tuning))
                   tuning-sliders))))

(def bricks-mutate-constants-storage-key "brickbreaker/bricks-mutate-constants")

(defn load-bricks-mutate-constants? []
  (try
    (= "true" (js/localStorage.getItem bricks-mutate-constants-storage-key))
    (catch :default _
      false)))

(defn save-bricks-mutate-constants! [enabled?]
  (try
    (js/localStorage.setItem bricks-mutate-constants-storage-key (str enabled?))
    (catch :default _
      nil)))

(defonce bricks-mutate-constants?
  (r/atom (load-bricks-mutate-constants?)))

(defn toggle-bricks-mutate-constants! []
  (save-bricks-mutate-constants! (swap! bricks-mutate-constants? not)))

(defonce toasts
  (r/atom []))

(defonce toast-counter
  (atom 0))

(def toast-duration 3000)

(defn show-toast! [text]
  (let [id (swap! toast-counter inc)]
    (swap! toasts conj {:toast/id id
                        :toast/text text})
    (js/setTimeout (fn []
                     (swap! toasts (fn [current-toasts]
                                     (filterv (fn [toast]
                                                (not= id (:toast/id toast)))
                                              current-toasts))))
                   toast-duration)))

(defn format-value [value]
  (str (js/Number (.toPrecision value 3))))

(def mutation-factors [0.5 2])

(defn mutable-slider? [slider]
  (and (not (:slider/needs-restart? slider))
       (get slider :slider/mutable? true)))

(defn mutate-random-slider! []
  (let [slider (->> tuning-sliders
                    (filter mutable-slider?)
                    (filter (fn [slider]
                              (slider-active? @game-state slider)))
                    rand-nth)
        path (slider-path (:slider/key slider))
        old-value (get-in @game-state path)
        new-value (-> (* old-value (rand-nth mutation-factors))
                      (max (:slider/min slider))
                      (min (:slider/max slider)))]
    (swap! game-state assoc-in path new-value)
    (show-toast! (str (:slider/label slider) ": "
                      (format-value old-value) " → " (format-value new-value)))))

(defn toasts-view []
  [:div {:style {:position "absolute"
                 :top 8
                 :right 8
                 :display "flex"
                 :flex-direction "column"
                 :gap 6
                 :align-items "flex-end"
                 :pointer-events "none"}}
   (for [{:toast/keys [id text]} @toasts]
     ^{:key id}
     [:div {:style {:padding "6px 12px"
                    :background "rgba(0, 0, 0, 0.75)"
                    :border-radius 6}}
      text])])

(defn tuning-slider [{:slider/keys [key label min max step needs-restart?]}]
  (let [value (get-in @game-state (slider-path key))]
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
                 :top 40
                 :left 8
                 :max-height "calc(100vh - 56px)"
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
    [:input {:type "checkbox"
             :checked @bricks-mutate-constants?
             :on-change (fn [event]
                          (.blur (.-currentTarget event))
                          (toggle-bricks-mutate-constants!))}]
    " Bricks mutate constants"]
   [:label {:style {:grid-column "1 / -1"}}
    "Gravity mode "
    [:select {:value (name (get-in @game-state [:state/tuning :tuning/gravity-mode]))
              :on-change (fn [event]
                           (.blur (.-currentTarget event))
                           (set-gravity-mode! (keyword "gravity" (.. event -target -value))))}
     [:option {:value "linear"} "Linear"]
     [:option {:value "radial"} "Radial"]]]
   (for [slider tuning-sliders
         :when (slider-active? @game-state slider)]
     ^{:key (:slider/key slider)}
     [tuning-slider slider])])

(defn replay-button []
  [:button {:on-click (fn [event]
                        (.blur (.-currentTarget event))
                        (new-game!))}
   "Replay"])

(defn sliders-toggle-button []
  [:button {:on-click (fn [event]
                        (.blur (.-currentTarget event))
                        (swap! sliders-visible? not))}
   (if @sliders-visible?
     "Hide sliders"
     "Show sliders")])

(def ticks-per-second 60)

(defn game-over-view [{:state/keys [game-tick]}]
  [:div {:style {:position "absolute"
                 :top "50%"
                 :left "50%"
                 :transform "translate(-50%, -50%)"
                 :display "flex"
                 :flex-direction "column"
                 :gap 12
                 :align-items "center"
                 :padding 16
                 :background "rgba(0, 0, 0, 0.75)"
                 :border-radius 6}}
   [:div "Time: " (.toFixed (/ game-tick ticks-per-second) 1) " s"]
   [replay-button]])

(defn hud-view [state]
  [:<>
   [:div {:style {:position "absolute"
                  :top 8
                  :left 8}}
    [sliders-toggle-button]]
   (when (game/game-over? state)
     [game-over-view state])])

(defn game-view []
  (let [{:state/keys [paddle-location paddle-angle ball-location constants tuning] :as state} @game-state
        {:constant/keys [world-width world-height paddle-width paddle-height ball-radius]} constants]
    [:div {:style {:position "relative"
                   :width "100vw"
                   :height "100vh"
                   :font-family "sans-serif"
                   :color "#eee"
                   :background "#111"}}
     [:svg {:view-box (str "0 0 " world-width " " world-height)
            :style {:width "100%"
                    :height "100%"
                    :display "block"}}
      [:rect {:width world-width
              :height world-height
              :fill "#222"}]
      [bricks-view state]
      [explosions-view @effects constants]
      (when (= :gravity/radial (:tuning/gravity-mode tuning))
        (let [[center-x center-y] (physics/gravity-center {:tuning tuning
                                                           :constants constants})]
          [:circle {:cx center-x
                    :cy center-y
                    :r 1.5
                    :fill "none"
                    :stroke "rgba(255, 255, 255, 0.3)"
                    :stroke-width 0.3
                    :pointer-events "none"}]))
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
     [hud-view state]
     [toasts-view]
     (when @sliders-visible?
       [tuning-panel])]))


(defn typing-in-number-field? [event]
  (= "number" (.. event -target -type)))

(defn on-key-down! [event]
  (cond
    (typing-in-number-field? event)
    (when (= "Enter" (.-key event))
      (.blur (.-target event)))

    (contains? #{"r" "R"} (.-key event))
    (new-game!)

    :else
    (do
      (when (key->direction (.-key event))
        (.preventDefault event))
      (swap! held-keys conj (.-key event)))))

(defn on-key-up! [event]
  (swap! held-keys disj (.-key event)))

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

(defn step-game! []
  (let [before @game-state
        after (swap! game-state game/step (current-action))
        broken-cells (set/difference (:state/bricks-alive before)
                                     (:state/bricks-alive after))]
    (swap! effects advance-effects broken-cells)
    (when @bricks-mutate-constants?
      (dotimes [_ (count broken-cells)]
        (mutate-random-slider!)))))

(defn tick! [timestamp]
  (record-frame! timestamp)
  (let [{:clock/keys [tick-count]} (swap! frame-clock advance-frame-clock timestamp)]
    (dotimes [_ tick-count]
      (step-game!)))
  (js/requestAnimationFrame tick!))

(defonce root
  (rdc/create-root (js/document.getElementById "app")))

(defn ^:dev/after-load render! []
  (rdc/render root [game-view]))

(defonce started
  (do
    (js/window.addEventListener "keydown" on-key-down!)
    (js/window.addEventListener "keyup" on-key-up!)
    (js/requestAnimationFrame tick!)
    true))

(defn init! []
  (render!))
