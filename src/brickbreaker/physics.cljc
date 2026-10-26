(ns brickbreaker.physics)

(defn clamp [value low high]
  (max low (min high value)))

(defn sign [value]
  (if (neg? value)
    -1
    1))

(defn absolute [value]
  (Math/abs (double value)))

(defn add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn subtract [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])

(defn scale [[x y] factor]
  [(* x factor) (* y factor)])

(defn dot [[ax ay] [bx by]]
  (+ (* ax bx) (* ay by)))

(defn cross [[ax ay] [bx by]]
  (- (* ax by) (* ay bx)))

(defn magnitude [vector]
  (Math/sqrt (dot vector vector)))

(defn normalize [vector]
  (scale vector (/ 1 (magnitude vector))))

(defn rotate [[x y] angle]
  (let [cosine (Math/cos angle)
        sine (Math/sin angle)]
    [(- (* x cosine) (* y sine))
     (+ (* x sine) (* y cosine))]))

(defn limit-magnitude [vector maximum]
  (let [length (magnitude vector)]
    (if (> length maximum)
      (scale vector (/ maximum length))
      vector)))

(defn location->vector [{:location/keys [x y]}]
  [x y])

(defn vector->location [[x y]]
  {:location/x x
   :location/y y})

(defn velocity->vector [{:velocity/keys [x y]}]
  [x y])

(defn vector->velocity [[x y]]
  {:velocity/x x
   :velocity/y y})

(defn degrees->radians [degrees]
  (* degrees (/ Math/PI 180)))

(defn gravity-center [{:keys [tuning constants]}]
  [(* (:tuning/gravity-center-x tuning) (:constant/world-width constants))
   (* (:tuning/gravity-center-y tuning) (:constant/world-height constants))])

(defn gravity-vector [{:keys [tuning constants location]}]
  (let [{:tuning/keys [gravity gravity-mode gravity-angle]} tuning]
    (case gravity-mode
      :gravity/linear
      (let [radians (degrees->radians gravity-angle)]
        (scale [(Math/cos radians) (Math/sin radians)] gravity))

      :gravity/radial
      (let [offset (subtract (gravity-center {:tuning tuning
                                              :constants constants})
                             (location->vector location))]
        (if (< (magnitude offset) 1e-9)
          [0 0]
          (scale (normalize offset) gravity))))))

(defn apply-gravity [{:state/keys [tuning constants] :as state} {:keys [location-key velocity-key]}]
  (let [[gravity-x gravity-y] (gravity-vector {:tuning tuning
                                               :constants constants
                                               :location (get state location-key)})]
    (-> state
        (update-in [velocity-key :velocity/x] + gravity-x)
        (update-in [velocity-key :velocity/y] + gravity-y))))

(defn axis-direction [action negative positive]
  (cond
    (and (contains? action negative) (not (contains? action positive))) -1
    (and (contains? action positive) (not (contains? action negative))) 1
    :else 0))

(defn accelerate-paddle [{:state/keys [tuning] :as state} action]
  (let [{:tuning/keys [paddle-acceleration paddle-friction paddle-max-speed]} tuning
        accelerate (fn [velocity direction]
                     (clamp (* paddle-friction (+ velocity (* direction paddle-acceleration)))
                            (- paddle-max-speed)
                            paddle-max-speed))]
    (-> state
        (apply-gravity {:location-key :state/paddle-location
                        :velocity-key :state/paddle-velocity})
        (update-in [:state/paddle-velocity :velocity/x] accelerate (axis-direction action :action/left :action/right))
        (update-in [:state/paddle-velocity :velocity/y] accelerate (axis-direction action :action/up :action/down)))))

(defn paddle-corners [{:state/keys [paddle-location paddle-angle constants]}]
  (let [{:constant/keys [paddle-width paddle-height]} constants
        half-width (/ paddle-width 2)
        half-height (/ paddle-height 2)
        center (location->vector paddle-location)]
    (for [corner [[(- half-width) (- half-height)]
                  [half-width (- half-height)]
                  [half-width half-height]
                  [(- half-width) half-height]]]
      (add center (rotate corner paddle-angle)))))

(defn brick-corners [constants [column row]]
  (let [{:constant/keys [brick-width brick-height bricks-top]} constants
        left (* column brick-width)
        top (+ bricks-top (* row brick-height))
        right (+ left brick-width)
        bottom (+ top brick-height)]
    [[left top] [right top] [right bottom] [left bottom]]))

(defn paddle-moment-of-inertia [{:state/keys [tuning constants]}]
  (let [{:tuning/keys [paddle-mass]} tuning
        {:constant/keys [paddle-width paddle-height]} constants]
    (/ (* paddle-mass (+ (* paddle-width paddle-width) (* paddle-height paddle-height)))
       12)))

(defn wall-contacts [constants corners]
  (let [{:constant/keys [world-width world-height]} constants]
    (for [[x y :as corner] corners
          [depth normal] [[(- x) [1 0]]
                          [(- x world-width) [-1 0]]
                          [(- y) [0 1]]
                          [(- y world-height) [0 -1]]]
          :when (pos? depth)]
      {:contact/point corner
       :contact/normal normal
       :contact/depth depth})))

(defn paddle-corner-in-brick-contacts [constants corners cell]
  (let [[[left top] _ [right bottom] _] (brick-corners constants cell)]
    (for [[x y :as corner] corners
          :when (and
                  (< left x right)
                  (< top y bottom))]
      (let [[depth normal] (apply min-key first [[(- x left) [-1 0]]
                                                 [(- right x) [1 0]]
                                                 [(- y top) [0 -1]]
                                                 [(- bottom y) [0 1]]])]
        {:contact/point corner
         :contact/normal normal
         :contact/depth depth}))))

(defn brick-corner-in-paddle-contacts [{:state/keys [paddle-location paddle-angle constants]} cell]
  (let [{:constant/keys [paddle-width paddle-height]} constants
        half-width (/ paddle-width 2)
        half-height (/ paddle-height 2)]
    (for [corner (brick-corners constants cell)
          :let [[local-x local-y] (-> corner
                                      (subtract (location->vector paddle-location))
                                      (rotate (- paddle-angle)))
                gap-x (- half-width (absolute local-x))
                gap-y (- half-height (absolute local-y))]
          :when (and
                  (pos? gap-x)
                  (pos? gap-y))]
      (let [[depth local-normal] (if (< gap-x gap-y)
                                   [gap-x [(- (sign local-x)) 0]]
                                   [gap-y [0 (- (sign local-y))]])]
        {:contact/point corner
         :contact/normal (rotate local-normal paddle-angle)
         :contact/depth depth}))))

(defn bricks-near-paddle [{:state/keys [bricks-alive constants]} corners]
  (let [xs (map first corners)
        ys (map second corners)]
    (filter (fn [cell]
              (let [[[left top] _ [right bottom] _] (brick-corners constants cell)]
                (and
                  (< left (apply max xs))
                  (< (apply min xs) right)
                  (< top (apply max ys))
                  (< (apply min ys) bottom))))
            bricks-alive)))

(defn paddle-obstacle-contacts
  "Contacts with normals pointing out of the obstacle, toward the paddle."
  [{:state/keys [constants] :as state}]
  (let [corners (paddle-corners state)]
    (concat
      (wall-contacts constants corners)
      (mapcat (fn [cell]
                (concat
                  (paddle-corner-in-brick-contacts constants corners cell)
                  (brick-corner-in-paddle-contacts state cell)))
              (bricks-near-paddle state corners)))))

(defn merge-contacts
  "Merges contacts that share a normal into one at their average point, so a flat face resting on a surface gets no false torque."
  [contacts]
  (for [group (vals (group-by (fn [contact]
                                (mapv (fn [component] (Math/round (* 1000.0 component)))
                                      (:contact/normal contact)))
                              contacts))]
    {:contact/point (scale (reduce add (map :contact/point group)) (/ 1 (count group)))
     :contact/normal (:contact/normal (first group))
     :contact/depth (apply max (map :contact/depth group))}))

(def resting-contact-speed 0.02)

(defn resolve-paddle-contact
  [{:state/keys [paddle-location paddle-velocity paddle-angular-velocity tuning] :as state}
   {:contact/keys [point normal depth]}]
  (let [{:tuning/keys [paddle-mass restitution]} tuning
        moment-of-inertia (paddle-moment-of-inertia state)
        center (location->vector paddle-location)
        [offset-x offset-y :as offset] (subtract point center)
        velocity (velocity->vector paddle-velocity)
        point-velocity (add velocity
                            [(- (* paddle-angular-velocity offset-y))
                             (* paddle-angular-velocity offset-x)])
        approach (dot point-velocity normal)
        pushed-out (assoc state :state/paddle-location (vector->location (add center (scale normal depth))))]
    (if (neg? approach)
      (let [offset-cross-normal (cross offset normal)
            ;; slow contacts do not bounce, so a paddle resting on a surface settles instead of jittering
            effective-restitution (if (< (- approach) resting-contact-speed)
                                    0
                                    restitution)
            impulse-magnitude (/ (* -1 (+ 1 effective-restitution) approach)
                                 (+ (/ 1 paddle-mass)
                                    (/ (* offset-cross-normal offset-cross-normal) moment-of-inertia)))
            impulse (scale normal impulse-magnitude)]
        (assoc pushed-out
               :state/paddle-velocity (vector->velocity (add velocity (scale impulse (/ 1 paddle-mass))))
               :state/paddle-angular-velocity (+ paddle-angular-velocity
                                                 (/ (cross offset impulse) moment-of-inertia))))
      pushed-out)))

(defn collide-paddle-with-obstacles [state]
  (reduce resolve-paddle-contact state (merge-contacts (paddle-obstacle-contacts state))))

(defn advance-paddle [{:state/keys [paddle-velocity paddle-angular-velocity] :as state} fraction]
  (-> state
      (update :state/paddle-location (fn [location]
                                       (-> (location->vector location)
                                           (add (scale (velocity->vector paddle-velocity) fraction))
                                           vector->location)))
      (update :state/paddle-angle + (* paddle-angular-velocity fraction))))

(defn move-paddle-body
  "Moves in steps of at most half a unit, so the paddle cannot tunnel through a brick or wall."
  [{:state/keys [paddle-velocity paddle-angular-velocity constants] :as state}]
  (let [{:constant/keys [paddle-width paddle-height]} constants
        half-diagonal (/ (magnitude [paddle-width paddle-height]) 2)
        travel (+ (magnitude (velocity->vector paddle-velocity))
                  (* (absolute paddle-angular-velocity) half-diagonal))
        step-count (max 1 (int (Math/ceil (/ travel 0.5))))
        fraction (/ 1.0 step-count)]
    (nth (iterate (fn [current]
                    (-> current
                        (advance-paddle fraction)
                        collide-paddle-with-obstacles))
                  state)
         step-count)))

(defn damp-paddle-spin [{:state/keys [tuning] :as state}]
  (update state :state/paddle-angular-velocity * (:tuning/paddle-angular-damping tuning)))

(defn move-paddle [state action]
  (-> state
      (accelerate-paddle action)
      damp-paddle-spin
      move-paddle-body))

(defn brick-at
  "Returns [column row] of the alive brick containing the point, or nil."
  [{:state/keys [bricks-alive constants]} {:location/keys [x y]}]
  (let [{:constant/keys [brick-width brick-height bricks-top]} constants
        cell [(int (Math/floor (/ x brick-width)))
              (int (Math/floor (/ (- y bricks-top) brick-height)))]]
    (when (contains? bricks-alive cell)
      cell)))

(defn break-brick [state cell]
  (-> state
      (update :state/bricks-alive disj cell)
      (update :state/bricks-broken inc)))

(defn move-ball-x [{:state/keys [ball-location ball-velocity constants] :as state}]
  (let [{:constant/keys [ball-radius world-width]} constants
        {:location/keys [x y]} ball-location
        velocity-x (:velocity/x ball-velocity)
        next-x (+ x velocity-x)
        cell (brick-at state {:location/x (+ next-x (* (sign velocity-x) ball-radius))
                              :location/y y})]
    (cond
      cell
      (-> state
          (break-brick cell)
          (update-in [:state/ball-velocity :velocity/x] -))

      (< (- next-x ball-radius) 0)
      (-> state
          (assoc-in [:state/ball-location :location/x] ball-radius)
          (assoc-in [:state/ball-velocity :velocity/x] (absolute velocity-x)))

      (> (+ next-x ball-radius) world-width)
      (-> state
          (assoc-in [:state/ball-location :location/x] (- world-width ball-radius))
          (assoc-in [:state/ball-velocity :velocity/x] (- (absolute velocity-x))))

      :else
      (assoc-in state [:state/ball-location :location/x] next-x))))

(defn move-ball-y [{:state/keys [ball-location ball-velocity constants] :as state}]
  (let [{:constant/keys [ball-radius world-height]} constants
        {:location/keys [x y]} ball-location
        velocity-y (:velocity/y ball-velocity)
        next-y (+ y velocity-y)
        cell (brick-at state {:location/x x
                              :location/y (+ next-y (* (sign velocity-y) ball-radius))})]
    (cond
      cell
      (-> state
          (break-brick cell)
          (update-in [:state/ball-velocity :velocity/y] -))

      (< (- next-y ball-radius) 0)
      (-> state
          (assoc-in [:state/ball-location :location/y] ball-radius)
          (assoc-in [:state/ball-velocity :velocity/y] (absolute velocity-y)))

      (> (+ next-y ball-radius) world-height)
      (-> state
          (assoc-in [:state/ball-location :location/y] (- world-height ball-radius))
          (assoc-in [:state/ball-velocity :velocity/y] (- (absolute velocity-y))))

      :else
      (assoc-in state [:state/ball-location :location/y] next-y))))

(defn apply-ball-gravity [state]
  (apply-gravity state {:location-key :state/ball-location
                        :velocity-key :state/ball-velocity}))

(defn apply-ball-drag [{:state/keys [ball-velocity tuning] :as state}]
  (let [velocity (velocity->vector ball-velocity)
        drag-factor (max 0 (- 1 (* (:tuning/ball-drag tuning) (magnitude velocity))))]
    (assoc state :state/ball-velocity (vector->velocity (scale velocity drag-factor)))))

(defn move-ball [state]
  (-> state
      apply-ball-gravity
      apply-ball-drag
      move-ball-x
      move-ball-y))

(defn paddle-contact
  "Returns the ball-paddle contact (world frame, relative to paddle center), or nil."
  [{:state/keys [ball-location paddle-location paddle-angle constants]}]
  (let [{:constant/keys [ball-radius paddle-width paddle-height]} constants
        half-width (/ paddle-width 2)
        half-height (/ paddle-height 2)
        [local-x local-y] (-> (location->vector ball-location)
                              (subtract (location->vector paddle-location))
                              (rotate (- paddle-angle)))
        closest [(clamp local-x (- half-width) half-width)
                 (clamp local-y (- half-height) half-height)]
        separation (subtract [local-x local-y] closest)
        distance (magnitude separation)]
    (when (< distance ball-radius)
      (let [[surface normal] (cond
                               (pos? distance)
                               [closest (scale separation (/ 1 distance))]

                               (< (- half-width (absolute local-x)) (- half-height (absolute local-y)))
                               [[(* (sign local-x) half-width) local-y] [(sign local-x) 0]]

                               :else
                               [[local-x (* (sign local-y) half-height)] [0 (sign local-y)]])]
        {:contact/offset (rotate surface paddle-angle)
         :contact/normal (rotate normal paddle-angle)}))))

(defn bounce-off-paddle
  [{:state/keys [ball-velocity paddle-location paddle-velocity paddle-angular-velocity tuning constants] :as state}
   {:contact/keys [offset normal]}]
  (let [{:constant/keys [ball-radius]} constants
        {:tuning/keys [ball-max-speed ball-mass paddle-mass restitution]} tuning
        moment-of-inertia (paddle-moment-of-inertia state)
        [offset-x offset-y] offset
        paddle-velocity-vector (velocity->vector paddle-velocity)
        contact-velocity (add paddle-velocity-vector
                              [(- (* paddle-angular-velocity offset-y))
                               (* paddle-angular-velocity offset-x)])
        ball-velocity-vector (velocity->vector ball-velocity)
        approach (dot (subtract ball-velocity-vector contact-velocity) normal)
        ball-location (-> (location->vector paddle-location)
                          (add offset)
                          (add (scale normal ball-radius))
                          vector->location)]
    (if (neg? approach)
      (let [offset-cross-normal (cross offset normal)
            impulse-magnitude (/ (* -1 (+ 1 restitution) approach)
                                 (+ (/ 1 ball-mass)
                                    (/ 1 paddle-mass)
                                    (/ (* offset-cross-normal offset-cross-normal) moment-of-inertia)))
            impulse (scale normal impulse-magnitude)]
        (assoc state
               :state/ball-location ball-location
               :state/ball-velocity (-> ball-velocity-vector
                                        (add (scale impulse (/ 1 ball-mass)))
                                        (limit-magnitude ball-max-speed)
                                        vector->velocity)
               :state/paddle-velocity (-> paddle-velocity-vector
                                          (subtract (scale impulse (/ 1 paddle-mass)))
                                          vector->velocity)
               :state/paddle-angular-velocity (- paddle-angular-velocity
                                                 (/ (cross offset impulse) moment-of-inertia))))
      (assoc state :state/ball-location ball-location))))

(defn collide-ball-with-paddle [state]
  (if-let [contact (paddle-contact state)]
    (bounce-off-paddle state contact)
    state))
