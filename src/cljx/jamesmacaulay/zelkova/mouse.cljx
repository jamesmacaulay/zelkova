(ns jamesmacaulay.zelkova.mouse
  "This namespace provides keyboard-related signals."
  #+cljs
  (:require [jamesmacaulay.zelkova.signal :as z]
            [goog.events :as events]
            [cljs.core.async :as async :refer [>! <!]])
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

#+cljs
(defn- listen
  [el type & args]
  (let [out (apply async/chan 1 args)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

#+cljs
(defn- position-channel
  [graph opts]
  (listen js/document
          "mousemove"
          (map (fn [e] [(.-pageX (.-event_ e)) (.-pageY (.-event_ e))]))))

#+cljs
(def ^{:doc "A signal of mouse positions as `[x y]` vectors. Initial value is `[0 0]`."}
  position
  (z/input [0 0] ::position position-channel))

#+cljs
(def ^{:doc "A signal of mouse x-coordinates. Initial value is 0."}
  x
  (z/map first position))

#+cljs
(def ^{:doc "A signal of mouse y-coordinates. Initial value is 0."}
  y
  (z/map second position))

#+cljs
(defn- clicks-channel
  [graph opts]
  (listen js/document
          "click"
          (map (constantly :click))))

#+cljs
(def ^{:doc "A signal which is updated every time the mouse is clicked. The
value of the signal is always the keyword `:click`."}
  clicks
  (z/input :click ::clicks clicks-channel))

#+cljs
(defn- down?-channel
  [graph opts]
  (let [down-events (listen js/document "mousedown" (map (constantly true)))
        up-events (listen js/document "mouseup" (map (constantly false)))]
    (async/merge [down-events up-events])))

#+cljs
(def ^{:doc "A signal of boolean values, true when the primary mouse button is
depressed."}
  down?
  (z/input false ::down? down?-channel))
