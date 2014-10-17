#+clj
(ns jamesmacaulay.zelkova.mouse)

#+cljs
(ns jamesmacaulay.zelkova.mouse
  (:require [jamesmacaulay.zelkova.signal :as z]
            [goog.events :as events]
            [cljs.core.async :as async :refer [>! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

#+cljs
(defn- listen
  [el type & args]
  (let [out (apply async/chan 1 args)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

#+cljs
(defn position-channel
  [graph opts]
  (listen js/document
          "mousemove"
          (comp (map (fn [e] [(.-offsetX e) (.-offsetY e)]))
                (map (partial z/->Event ::position)))))

#+cljs
(def position (assoc (z/input [0 0] ::position)
                :event-sources {::position position-channel}))

#+cljs
(def x (z/lift first position))

#+cljs
(def y (z/lift second position))

#+cljs
(defn clicks-channel
  [graph opts]
  (listen js/document
          "click"
          (comp (map (constantly :click))
                (map (partial z/->Event ::clicks)))))

#+cljs
(def clicks (assoc (z/input :click ::clicks)
              :event-sources {::clicks clicks-channel}))
