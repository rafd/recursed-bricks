(ns brickbreaker.game
  (:require
    [brickbreaker.constants :as constants]
    [brickbreaker.physics :as physics]
    [brickbreaker.random :as random]))

(def directions
  #{:action/left :action/right :action/up :action/down})

(def ball-initial-direction
  (physics/normalize [0.7 -1]))

(defn spawn-ball [{:state/keys [paddle-location tuning] :as state}]
  (assoc state
         :state/ball-location {:location/x (:location/x paddle-location)
                               :location/y (- (:location/y paddle-location) 5)}
         :state/ball-velocity (-> ball-initial-direction
                                  (physics/scale (:tuning/ball-initial-speed tuning))
                                  physics/vector->velocity)))

(defn with-brick-size [{:constant/keys [world-width world-height brick-columns brick-height-ratio] :as constants}]
  (assoc constants
         :constant/brick-width (/ world-width brick-columns)
         :constant/brick-height (* world-height brick-height-ratio)))

(defn new-game
  [{:keys [seed tuning constants]
    :or {seed 42}}]
  (let [constants (with-brick-size (merge constants/default-constants constants))
        {:constant/keys [brick-columns brick-rows brick-fill-ratio world-width world-height]} constants]
    (spawn-ball
      {:state/game-tick 0
       :state/game-status :status/playing
       :state/bricks-broken 0
       :state/tuning (merge constants/default-tuning tuning)
       :state/constants constants
       :state/paddle-location {:location/x (/ world-width 2)
                               :location/y (- world-height 5)}
       :state/paddle-velocity {:velocity/x 0
                               :velocity/y 0}
       :state/paddle-angle 0
       :state/paddle-angular-velocity 0
       :state/bricks-alive (random/random-bricks {:seed seed
                                                  :columns brick-columns
                                                  :rows brick-rows
                                                  :fill-percent (* 100 brick-fill-ratio)})})))

(defn game-over? [{:state/keys [game-status]}]
  (not= :status/playing game-status))

(defn update-status [{:state/keys [bricks-alive] :as state}]
  (if (empty? bricks-alive)
    (assoc state :state/game-status :status/won)
    state))

(defn step [state action]
  (if (game-over? state)
    state
    (-> state
        (physics/move-paddle action)
        physics/move-ball
        physics/collide-ball-with-paddle
        (update :state/game-tick inc)
        update-status)))

(defn score [{:state/keys [bricks-broken]}]
  bricks-broken)

#_(step (new-game {:seed 1}) #{:action/left})
