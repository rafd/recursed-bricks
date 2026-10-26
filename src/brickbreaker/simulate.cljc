(ns brickbreaker.simulate
  (:require
    [brickbreaker.game :as game]))

(defn play-game
  "Returns the vector of states, from start state to game over (or max-ticks)."
  [{:keys [state decide max-ticks]}]
  (loop [states [state]]
    (let [current (peek states)]
      (if (or
            (game/game-over? current)
            (>= (:state/game-tick current) max-ticks))
        states
        (recur (conj states (game/step current (decide current))))))))
