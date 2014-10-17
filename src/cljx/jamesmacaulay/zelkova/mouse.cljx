#+clj
(ns jamesmacaulay.zelkova.mouse)

#+cljs
(ns jamesmacaulay.zelkova.mouse
  (:require [jamesmacaulay.zelkova.signal :as z]
            [goog.events :as events]
            [cljs.core.async :as async :refer [>! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

#+cljs
(defn- listen [el type]
  (let [out (async/chan)]
    (events/listen el type
                   (fn [e] (async/put! out e)))
    out))

#+cljs
(defn- position-channel
  [graph opts]
  (let [dom-events (listen js/document "mousemove")
        xform (comp (map (fn [e] [(.-offsetX e) (.-offsetY e)]))
                    (map (partial z/->Event ::position)))]
    (async/pipe dom-events (async/chan 1 xform))))

#+cljs
(def position (assoc (z/input [0 0] ::position)
                :event-sources {::position position-channel}))

