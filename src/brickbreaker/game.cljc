(ns brickbreaker.game
  (:require
    [brickbreaker.constants :as constants]
    [brickbreaker.physics :as physics]
    [brickbreaker.random :as random]))

(def directions
  #{:action/left :action/right :action/up :action/down})

(def ball-initial-direction
  (physics/normalize [0.7 -1]))

(def ball-spawn-distance 5)

(defn spawn-ball [{:state/keys [paddle-location tuning constants] :as state}]
  (let [{:constant/keys [ball-radius world-width]} constants
        {:location/keys [x y]} paddle-location
        above-y (- y ball-spawn-distance)]
    (assoc state
           :state/ball-location {:location/x (-> x
                                                 (max ball-radius)
                                                 (min (- world-width ball-radius)))
                                 :location/y (if (>= above-y ball-radius)
                                               above-y
                                               (+ y ball-spawn-distance))}
           :state/ball-velocity (-> ball-initial-direction
                                    (physics/scale (:tuning/ball-initial-speed tuning))
                                    physics/vector->velocity))))

(defn spawn-paddle [{:state/keys [constants] :as state}]
  (let [{:constant/keys [world-width world-height]} constants]
    (assoc state
           :state/paddle-location {:location/x (/ world-width 2)
                                   :location/y (- world-height 5)}
           :state/paddle-velocity {:velocity/x 0
                                   :velocity/y 0}
           :state/paddle-angle 0
           :state/paddle-angular-velocity 0)))

(defn with-brick-size [{:constant/keys [world-width world-height brick-columns brick-height-ratio] :as constants}]
  (assoc constants
         :constant/brick-width (/ world-width brick-columns)
         :constant/brick-height (* world-height brick-height-ratio)))

(defn new-game
  [{:keys [seed tuning constants]
    :or {seed 42}}]
  (let [constants (with-brick-size (merge constants/default-constants constants))
        {:constant/keys [brick-columns brick-rows brick-fill-ratio]} constants]
    (-> {:state/game-tick 0
         :state/game-status :status/playing
         :state/bricks-broken 0
         :state/tuning (merge constants/default-tuning tuning)
         :state/constants constants
         :state/bricks-alive (random/random-bricks {:seed seed
                                                    :columns brick-columns
                                                    :rows brick-rows
                                                    :fill-percent (* 100 brick-fill-ratio)})}
        spawn-paddle
        spawn-ball)))

(defn game-over? [{:state/keys [game-status]}]
  (not= :status/playing game-status))

(defn update-status [{:state/keys [bricks-alive] :as state}]
  (if (empty? bricks-alive)
    (assoc state :state/game-status :status/won)
    state))

;; NaN locations also fail this check.
(defn in-bounds? [{:state/keys [constants]} {:location/keys [x y]}]
  (and (<= 0 x (:constant/world-width constants))
       (<= 0 y (:constant/world-height constants))))

(defn respawn-escaped-objects [{:state/keys [paddle-location ball-location] :as state}]
  (cond-> state
    (not (in-bounds? state paddle-location)) spawn-paddle
    (not (in-bounds? state ball-location)) spawn-ball))

(defn step [state action]
  (if (game-over? state)
    state
    (-> state
        (physics/move-paddle action)
        physics/move-ball
        physics/collide-ball-with-paddle
        respawn-escaped-objects
        (update :state/game-tick inc)
        update-status)))

(defn score [{:state/keys [bricks-broken]}]
  bricks-broken)

#_(step (new-game {:seed 1}) #{:action/left})
