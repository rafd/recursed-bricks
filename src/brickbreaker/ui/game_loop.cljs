(ns brickbreaker.ui.game-loop
  (:require
    [clojure.set :as set]
    [reagent.core :as r]
    [brickbreaker.game :as game]
    [brickbreaker.ui.curses :as curses]
    [brickbreaker.ui.effects :as effects]
    [brickbreaker.ui.input :as input]
    [brickbreaker.ui.match :as match]
    [brickbreaker.ui.state :as state]))

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

(def ticks-per-second 60)

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
  (let [before (state/player-state player-id)
        after (game/step before (input/current-action mode player-id))
        broken-cells (set/difference (:state/bricks-alive before)
                                     (:state/bricks-alive after))]
    (swap! state/app-state assoc-in [:app/players player-id] after)
    (swap! effects/effects update player-id effects/advance-effects broken-cells)
    broken-cells))

(defn step-match! []
  (let [{:app/keys [mode cursed?]} @state/app-state]
    (doseq [player-id (state/player-ids mode)]
      (let [broken-cells (step-player-and-get-broken-cells! mode player-id)]
        (when cursed?
          (dotimes [_ (count broken-cells)]
            (curses/curse-player! (curses/mutation-target mode player-id))))))))

(defn tick! [timestamp]
  (record-frame! timestamp)
  (let [{:clock/keys [tick-count]} (swap! frame-clock advance-frame-clock timestamp)]
    (dotimes [_ tick-count]
      (let [{:app/keys [screen] :as app} @state/app-state]
        (cond
          (match/match-running? app)
          (step-match!)

          (= :screen/playing screen)
          (effects/advance-effects-only!)))))
  (js/requestAnimationFrame tick!))
