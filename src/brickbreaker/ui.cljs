(ns brickbreaker.ui
  (:require
    [reagent.dom.client :as rdc]
    [brickbreaker.rcade :as rcade]
    [brickbreaker.ui.game-loop :as game-loop]
    [brickbreaker.ui.input :as input]
    [brickbreaker.ui.screens :as screens]))

(defonce root
  (rdc/create-root (js/document.getElementById "app")))

(defn ^:dev/after-load render! []
  (rdc/render root [screens/game-view]))

(defonce started
  (do
    (when-not rcade/in-cabinet?
      (js/window.addEventListener "keydown" input/on-key-down!)
      (js/window.addEventListener "keyup" input/on-key-up!))
    (rcade/connect! input/on-rcade-press!)
    (js/requestAnimationFrame game-loop/tick!)
    true))

(defn init! []
  (render!))
