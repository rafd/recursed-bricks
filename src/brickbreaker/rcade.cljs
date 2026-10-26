(ns brickbreaker.rcade)

;; Same handshake as PluginChannel.acquire in @rcade/sdk.
;; @rcade/plugin-input-classic uses import.meta (Closure cannot compile it)
;; and @rcade/sdk needs an undeclared zod dependency.

(def in-cabinet?
  (not= js/window js/window.parent))

(def button-name->button
  {"A" :button/a
   "B" :button/b
   "UP" :button/up
   "DOWN" :button/down
   "LEFT" :button/left
   "RIGHT" :button/right
   "ONE_PLAYER" :button/one-player
   "TWO_PLAYER" :button/two-player})

(def button->action
  {:button/left :action/left
   :button/right :action/right
   :button/up :action/up
   :button/down :action/down})

(defonce held-buttons
  (atom #{}))

(defn held-actions [player-number]
  (into #{}
        (keep (fn [[held-player button]]
                (when (= player-number held-player)
                  (button->action button))))
        @held-buttons))

(defn handle-message! [handler ^js message]
  (let [button (button-name->button (.-button message))
        player (.-player message)]
    (when button
      (if (.-pressed message)
        (do
          (swap! held-buttons conj [player button])
          (handler {:press/player player
                    :press/button button}))
        (swap! held-buttons disj [player button])))))

(def input-channel
  {:name "@rcade/input-classic"
   :version "^1.0.0"})

(defn acquire-port! [{:keys [name version]} on-port]
  (let [nonce (str (random-uuid))
        listener (fn listener [^js event]
                   (let [^js data (.-data event)]
                     (when (and (= "plugin_channel" (some-> data .-type))
                                (= nonce (.-nonce data)))
                       (js/window.removeEventListener "message" listener)
                       (if (.-error data)
                         (js/console.error "RCade plugin channel error" (.-error data))
                         (on-port (aget (.-ports event) 0))))))]
    (js/window.addEventListener "message" listener)
    (js/window.parent.postMessage (clj->js {:type "acquire_plugin_channel"
                                            :nonce nonce
                                            :channel {:name name
                                                      :version version}})
                                  "*")))

(defn connect! [handler]
  (when in-cabinet?
    (acquire-port! input-channel
                   (fn [^js port]
                     (set! (.-onmessage port)
                           (fn [^js event]
                             (handle-message! handler (.-data event))))))))
