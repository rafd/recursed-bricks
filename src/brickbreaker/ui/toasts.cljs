(ns brickbreaker.ui.toasts
  (:require
    [reagent.core :as r]
    [brickbreaker.rcade :as rcade]))

(defonce toasts
  (r/atom []))

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
