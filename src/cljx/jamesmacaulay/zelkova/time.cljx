#+clj
(ns jamesmacaulay.zelkova.time
  (:refer-clojure :exclude [second]))

#+cljs
(ns jamesmacaulay.zelkova.time
  (:refer-clojure :exclude [second])
  (:require [jamesmacaulay.zelkova.signal :as z]
            [goog.events :as events]
            [cljs.core.async :as async :refer [>! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def millisecond 1)
(def second 1000)
(def minute (* 60 second))
(def hour (* 60 minute))

(defn in-milliseconds [n] (/ n millisecond))
(defn in-seconds [n] (/ n second))
(defn in-minutes [n] (/ n minute))
(defn in-hours [n] (/ n hour))

#+cljs
(defn- listen
  [el type & args]
  (let [out (apply async/chan 1 args)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

#+cljs
(defn fps-channel-fn
  [n]
  (fn [graph opts]
    (let [ms-per-frame (/ 1000 n)
          out (async/chan 1 (map (partial z/->Event [::fps n])))]
      (go-loop [t (js/Date.)]
        (let [now (js/Date.)
              diff (- now t)]
          (<! (async/timeout (- ms-per-frame diff))))
        (let [now (js/Date.)
              diff (- now t)]
          (>! out diff)
          (recur now)))
      out)))

#+cljs
(defn fps
  [n]
  (assoc (z/input 0 [::fps n])
    :event-sources {[::fps n] (fps-channel-fn n)}))

