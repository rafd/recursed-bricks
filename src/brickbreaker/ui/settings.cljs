(ns brickbreaker.ui.settings
  (:require
    [cljs.reader :as reader]
    [brickbreaker.constants :as constants]
    [brickbreaker.ui.state :as state]))

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

(defn set-slider-value! [key text]
  (let [parsed (js/parseFloat text)
        value (clamp-to-slider (key->slider key) parsed)]
    (when (js/isFinite parsed)
      (state/update-all-players! (fn [player-state]
                                   (assoc-in player-state (slider-path key) value)))
      (save-slider-value! key value))))

(defn slider-active? [player-state slider]
  (let [gravity-mode (:slider/gravity-mode slider)]
    (or (nil? gravity-mode)
        (= gravity-mode (get-in player-state [:state/tuning :tuning/gravity-mode])))))

(defn set-gravity-mode! [gravity-mode]
  (state/update-all-players! (fn [player-state]
                               (assoc-in player-state [:state/tuning :tuning/gravity-mode] gravity-mode)))
  (save-slider-value! :tuning/gravity-mode gravity-mode))

(defn default-slider-value [key]
  (get (merge constants/default-constants
              (state/window-world-size (:app/mode @state/app-state))
              constants/default-tuning)
       key))

(defn reset-sliders! []
  (clear-slider-values!)
  (state/update-all-players!
    (fn [player-state]
      (reduce (fn [player-state {:slider/keys [key]}]
                (assoc-in player-state (slider-path key) (default-slider-value key)))
              (assoc-in player-state
                        [:state/tuning :tuning/gravity-mode]
                        (:tuning/gravity-mode constants/default-tuning))
              constants/tuning-sliders))))
