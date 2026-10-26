(ns brickbreaker.ui.board
  (:require
    [brickbreaker.physics :as physics]
    [brickbreaker.ui.effects :as effects]
    [brickbreaker.ui.state :as state]
    [brickbreaker.ui.toasts :as toasts]))

(defn explosions-view [{:effects/keys [frame explosions]} constants]
  [:g {:pointer-events "none"}
   (for [{:explosion/keys [cell start-frame]} explosions
         :let [age (- frame start-frame)
               [_column row] cell]
         [index {:fragment/keys [x y velocity-x velocity-y spin width height]}] (map-indexed vector (effects/fragments constants cell))]
     ^{:key (str cell "-" start-frame "-" index)}
     [:rect {:x (- (/ width 2))
             :y (- (/ height 2))
             :width width
             :height height
             :fill (effects/brick-color row)
             :opacity (- 1 (/ age effects/explosion-duration))
             :transform (str "translate("
                             (+ x (* velocity-x age))
                             ","
                             (+ y (* velocity-y age) (* 0.5 effects/fragment-gravity age age))
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
               :fill (effects/brick-color row)
               :stroke "#111"
               :stroke-width 0.3}])]))

(defn player-view [player-id]
  (let [{:state/keys [paddle-location paddle-angle ball-location constants] :as player-state} (state/player-state player-id)
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
      [gravity-field-view player-state]
      [bricks-view player-state]
      [explosions-view (get @effects/effects player-id) constants]
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
     [toasts/toasts-view player-id]]))
