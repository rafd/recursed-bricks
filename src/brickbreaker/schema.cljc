(ns brickbreaker.schema
  (:require
    [malli.core :as m]))

(def Location
  [:map
   [:location/x number?]
   [:location/y number?]])

(def Velocity
  [:map
   [:velocity/x number?]
   [:velocity/y number?]])

(def Brick
  [:tuple nat-int? nat-int?])

(def Action
  [:set [:enum :action/left :action/right :action/up :action/down]])

(def Tuning
  [:map
   [:tuning/paddle-acceleration pos?]
   [:tuning/paddle-friction pos?]
   [:tuning/paddle-max-speed pos?]
   [:tuning/paddle-angular-damping pos?]
   [:tuning/ball-initial-speed pos?]
   [:tuning/ball-max-speed pos?]
   [:tuning/gravity number?]
   [:tuning/gravity-mode [:enum :gravity/linear :gravity/radial]]
   [:tuning/gravity-angle number?]
   [:tuning/gravity-center-x number?]
   [:tuning/gravity-center-y number?]
   [:tuning/ball-drag number?]
   [:tuning/ball-mass pos?]
   [:tuning/paddle-mass pos?]
   [:tuning/restitution [:and number? [:>= 0] [:<= 1]]]])

(def Constants
  [:map
   [:constant/world-width pos?]
   [:constant/world-height pos?]
   [:constant/paddle-width pos?]
   [:constant/paddle-height pos?]
   [:constant/ball-radius pos?]
   [:constant/brick-columns pos-int?]
   [:constant/brick-rows pos-int?]
   [:constant/brick-width pos?]
   [:constant/brick-height pos?]
   [:constant/brick-height-ratio pos?]
   [:constant/brick-fill-ratio [:and number? [:>= 0] [:<= 1]]]
   [:constant/bricks-top number?]])

(def State
  [:map
   [:state/game-tick nat-int?]
   [:state/game-status [:enum :status/playing :status/won]]
   [:state/bricks-broken nat-int?]
   [:state/tuning Tuning]
   [:state/constants Constants]
   [:state/paddle-location Location]
   [:state/paddle-velocity Velocity]
   [:state/paddle-angle number?]
   [:state/paddle-angular-velocity number?]
   [:state/ball-location Location]
   [:state/ball-velocity Velocity]
   [:state/bricks-alive [:set Brick]]])

(def valid-state?
  (m/validator State))

(def explain-state
  (m/explainer State))
