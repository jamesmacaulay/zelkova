#+clj
(ns jamesmacaulay.zelkova.time
  (:refer-clojure :exclude [second])
  (:require [jamesmacaulay.zelkova.signal :as z]
            [cljs.core.async :as async :refer [>! <! go go-loop]]))

#+cljs
(ns jamesmacaulay.zelkova.time
  (:refer-clojure :exclude [second])
  (:require [jamesmacaulay.zelkova.signal :as z]
            [cljs.core.async :as async :refer [>! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

#+clj
(defn now [] (System/currentTimeMillis))
#+cljs
(defn now [] (.valueOf (js/Date.)))

(def millisecond 1)
(def second 1000)
(def minute (* 60 second))
(def hour (* 60 minute))

(defn in-milliseconds [n] (/ n millisecond))
(defn in-seconds [n] (/ n second))
(defn in-minutes [n] (/ n minute))
(defn in-hours [n] (/ n hour))

(defn fps-channel-fn
  [n]
  (fn [graph opts]
    (let [ms-per-frame (/ 1000 n)
          out (async/chan 1 (map (partial z/->Event [::fps n])))]
      (go-loop [t (now)
                error 0]
        (<! (async/timeout (- ms-per-frame error)))
        (let [new-t (now)
              diff (- new-t t)]
          (>! out diff)
          (recur new-t (+ error (- new-t t ms-per-frame)))))
      out)))

(defn fps
  [n]
  (assoc (z/input 0 [::fps n])
    :event-sources {[::fps n] (fps-channel-fn n)}))

(defn every-channel-fn
  [ms]
  (fn [graph opts]
    (let [out (async/chan 1 (map (partial z/->Event [::every ms])))]
      (go-loop [t (now)
                error 0]
        (<! (async/timeout (- ms error)))
        (let [new-t (now)]
          (>! out new-t)
          (recur new-t (+ error (- new-t t ms)))))
      out)))

(defn every
  [ms]
  (assoc (z/input (now) [::every ms])
    :event-sources {[::every ms] (every-channel-fn ms)}))
