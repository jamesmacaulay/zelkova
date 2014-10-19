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
          (map (fn [e] [(.-pageX (.-event_ e)) (.-pageY (.-event_ e))]))))

#+cljs
(def position (z/input [0 0] ::position position-channel))

#+cljs
(def x (z/lift first position))

#+cljs
(def y (z/lift second position))

#+cljs
(defn clicks-channel
  [graph opts]
  (listen js/document
          "click"
          (map (constantly :click))))

#+cljs
(def clicks (z/input :click ::clicks clicks-channel))

#+cljs
(defn down?-channel
  [graph opts]
  (let [down-events (listen js/document "mousedown" (map (constantly true)))
        up-events (listen js/document "mouseup" (map (constantly false)))]
    (async/merge [down-events up-events])))

#+cljs
(def down? (z/input false ::down? down?-channel))
