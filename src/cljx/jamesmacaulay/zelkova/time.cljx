#+clj
(ns jamesmacaulay.zelkova.time
  (:refer-clojure :exclude [second])
  (:require [jamesmacaulay.zelkova.platform.time :as t]
            [jamesmacaulay.zelkova.signal :as z]
            [clojure.core.async :as async :refer [>! <! go go-loop]]))

#+cljs
(ns jamesmacaulay.zelkova.time
  (:refer-clojure :exclude [second])
  (:require [jamesmacaulay.zelkova.platform.time :as t]
            [jamesmacaulay.zelkova.signal :as z]
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

(defn fps-channel-fn
  [n]
  (fn [graph opts]
    (let [ms-per-frame (/ 1000 n)
          out (async/chan)]
      (go-loop [t (t/now)
                error 0]
        (<! (async/timeout (- ms-per-frame error)))
        (let [new-t (t/now)
              diff (- new-t t)]
          (>! out diff)
          (recur new-t (+ error (- new-t t ms-per-frame)))))
      out)))

(defn fps
  [n]
  (z/input 0 [::fps n] (fps-channel-fn n)))

(defn every-channel-fn
  [ms]
  (fn [graph opts]
    (let [out (async/chan)]
      (go-loop [t (t/now)
                error 0]
        (<! (async/timeout (- ms error)))
        (let [new-t (t/now)]
          (>! out new-t)
          (recur new-t (+ error (- new-t t ms)))))
      out)))

(defn every
  [ms]
  (z/input (t/now) [::every ms] (every-channel-fn ms)))
