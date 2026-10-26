(ns brickbreaker.constants)

;; Min paddle acceleration stays above max gravity, so the paddle can always fight it.
(def tuning-sliders
  [{:slider/key :tuning/paddle-acceleration
    :slider/label "Paddle acceleration"
    :slider/default 0.08
    :slider/min 0.04
    :slider/max 0.25
    :slider/step 0.01}
   {:slider/key :tuning/paddle-friction
    :slider/label "Paddle friction (velocity kept per tick)"
    :slider/default 0.96
    :slider/min 0.85
    :slider/max 0.99
    :slider/step 0.005
    :slider/curse-step 0.02}
   {:slider/key :tuning/paddle-max-speed
    :slider/label "Paddle max speed"
    :slider/default 20
    :slider/min 0.5
    :slider/max 100
    :slider/step 0.5
    :slider/mutable? false}
   {:slider/key :tuning/paddle-angular-damping
    :slider/label "Paddle angular damping (spin kept per tick)"
    :slider/default 0.95
    :slider/min 0.8
    :slider/max 0.99
    :slider/step 0.005
    :slider/curse-step 0.03}
   {:slider/key :tuning/ball-initial-speed
    :slider/label "Ball initial speed"
    :slider/default 0.1
    :slider/min 0.05
    :slider/max 4
    :slider/step 0.05
    :slider/mutable? false}
   {:slider/key :tuning/ball-max-speed
    :slider/label "Ball max speed"
    :slider/default 20
    :slider/min 0.5
    :slider/max 100
    :slider/step 0.5
    :slider/mutable? false}
   {:slider/key :tuning/gravity
    :slider/label "Gravity"
    :slider/default 0.01
    :slider/min -0.025
    :slider/max 0.025
    :slider/step 0.0005
    :slider/curse-step 0.004}
   {:slider/key :tuning/gravity-angle
    :slider/label "Gravity angle (degrees, 90 is down)"
    :slider/default 90
    :slider/min 0
    :slider/max 360
    :slider/step 1
    :slider/curse-step 30
    :slider/wraps? true
    :slider/gravity-mode :gravity/linear}
   {:slider/key :tuning/gravity-center-x
    :slider/label "Gravity center x (fraction of width)"
    :slider/default 0.5
    :slider/min 0
    :slider/max 1
    :slider/step 0.01
    :slider/curse-step 0.25
    :slider/gravity-mode :gravity/radial}
   {:slider/key :tuning/gravity-center-y
    :slider/label "Gravity center y (fraction of height)"
    :slider/default 0.5
    :slider/min 0
    :slider/max 1
    :slider/step 0.01
    :slider/curse-step 0.25
    :slider/gravity-mode :gravity/radial}
   {:slider/key :tuning/ball-drag
    :slider/label "Ball drag"
    :slider/default 0.0008
    :slider/min 0
    :slider/max 0.004
    :slider/step 0.0001
    :slider/curse-step 0.0004}
   {:slider/key :tuning/ball-mass
    :slider/label "Ball mass"
    :slider/default 1
    :slider/min 0.25
    :slider/max 4
    :slider/step 0.05}
   {:slider/key :tuning/paddle-mass
    :slider/label "Paddle mass"
    :slider/default 10
    :slider/min 3
    :slider/max 40
    :slider/step 1}
   {:slider/key :tuning/restitution
    :slider/label "Restitution (bounciness)"
    :slider/default 0.95
    :slider/min 0.5
    :slider/max 1.1
    :slider/step 0.05
    :slider/curse-step 0.1}
   {:slider/key :constant/world-width
    :slider/label "World width"
    :slider/default 100
    :slider/min 50
    :slider/max 400
    :slider/step 1
    :slider/needs-restart? true}
   {:slider/key :constant/world-height
    :slider/label "World height"
    :slider/default 100
    :slider/min 50
    :slider/max 400
    :slider/step 1
    :slider/needs-restart? true}
   {:slider/key :constant/paddle-width
    :slider/label "Paddle width"
    :slider/default 16
    :slider/min 4
    :slider/max 60
    :slider/step 0.5}
   {:slider/key :constant/paddle-height
    :slider/label "Paddle height"
    :slider/default 2
    :slider/min 1.5
    :slider/max 15
    :slider/step 0.5
    :slider/curse-step 2}
   {:slider/key :constant/ball-radius
    :slider/label "Ball radius"
    :slider/default 1
    :slider/min 0.5
    :slider/max 5
    :slider/step 0.1
    :slider/curse-step 0.5}
   {:slider/key :constant/brick-columns
    :slider/label "Brick columns"
    :slider/default 10
    :slider/min 1
    :slider/max 20
    :slider/step 1
    :slider/needs-restart? true}
   {:slider/key :constant/brick-rows
    :slider/label "Brick rows"
    :slider/default 10
    :slider/min 1
    :slider/max 20
    :slider/step 1
    :slider/needs-restart? true}
   {:slider/key :constant/brick-height-ratio
    :slider/label "Brick height ratio"
    :slider/default 0.04
    :slider/min 0.01
    :slider/max 0.1
    :slider/step 0.005
    :slider/needs-restart? true}
   {:slider/key :constant/brick-fill-ratio
    :slider/label "Brick fill ratio"
    :slider/default 1
    :slider/min 0
    :slider/max 1
    :slider/step 0.05
    :slider/needs-restart? true}
   {:slider/key :constant/bricks-top
    :slider/label "Bricks top"
    :slider/default 10
    :slider/min 0
    :slider/max 50
    :slider/step 1
    :slider/mutable? false}])

(defn slider-defaults [namespace-name]
  (into {}
        (keep (fn [{:slider/keys [key default]}]
                (when (= namespace-name (namespace key))
                  [key default])))
        tuning-sliders))

(def default-constants
  (slider-defaults "constant"))

(def default-tuning
  (assoc (slider-defaults "tuning")
         :tuning/gravity-mode :gravity/linear))
