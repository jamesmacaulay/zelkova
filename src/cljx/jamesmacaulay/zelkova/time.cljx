(ns jamesmacaulay.zelkova.time
  "Functions for working with time."
  (:refer-clojure :exclude [second delay])
  #+clj
  (:require [jamesmacaulay.zelkova.impl.time :as t]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [clojure.core.async :as async :refer [>! <! go go-loop]])
  #+cljs
  (:require [jamesmacaulay.zelkova.impl.time :as t]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [cljs.core.async :as async :refer [>! <!]])
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def millisecond 1)
(def second 1000)
(def minute (* 60 second))
(def hour (* 60 minute))

(defn in-milliseconds [ms] ms)
(defn in-seconds [ms] (/ ms second))
(defn in-minutes [ms] (/ ms minute))
(defn in-hours [ms] (/ ms hour))

(defn- fps-channel-fn
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
  "Takes desired number of frames per second (fps). The resulting signal gives
  a sequence of time deltas as quickly as possible until it reaches the desired
  FPS. A time delta is the time between the last frame and the current frame."
  [n]
  (z/input 0 [::fps n] (fps-channel-fn n)))

(defn- every-channel-fn
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
  "Takes a time interval `ms`. The resulting signal is the current time, updated
  every `ms` milliseconds."
  [ms]
  (z/input (t/now) [::every ms] (every-channel-fn ms)))

(def ^:private timestamp-vector
  (juxt (comp impl/timestamp impl/origin-event)
        impl/value))

(defn timestamp
  "Add a timestamp to any signal. Returns a signal of `[timestamp value]`
  vectors. Timestamps are tied to the origin events of a signal value, so
  `(timestamp mouse/x)` and `(timestamp mouse/y)` will always have the same
  timestamp because they rely on the same underlying event (`mouse/position`)."
  [sig]
  (let [sig-init (:init-fn sig)]
    (impl/make-signal {:init-fn (fn [live-graph opts]
                                  [(t/now) (sig-init live-graph opts)])
                       :sources [sig]
                       :msg-fn (fn [_ [msg]]
                                 (when (impl/fresh? msg)
                                   (impl/fresh (timestamp-vector msg))))})))

#+cljs
(defn delay
  "Delay a signal by `ms` milliseconds."
  [ms sig]
  (z/splice (fn [to from]
              (let [waiting (async/chan (+ 1000 ms))
                    fire! #(async/take! waiting (partial async/put! to))]
                (go-loop []
                  (let [v (<! from)]
                    (if (nil? v)
                      (async/close! to)
                      (do
                        (>! waiting v)
                        (js/setTimeout fire! ms)
                        (recur)))))))
            (:init-fn sig)
            sig))

#+cljs
(defn since
  "Returns a signal of boolean values: true when `sig` has updated in the past
  `ms` milliseconds, false otherwise."
  [ms sig]
  (let [start (z/map (constantly 1) sig)
        stop (z/map (constantly -1) (delay ms sig))
        delaydiff (z/foldp + 0 (z/merge start stop))]
    (z/map (complement zero?) delaydiff)))

#+cljs
(defn debounce
  "Returns a signal which relays the latest value from `sig` only after `ms`
  milliseconds have passed since `sig` last updated. Useful when you want to
  wait until a user stops typing before doing something with the text, for
  example."
  [ms sig]
  (let [timeouts (->> sig (since ms) (z/keep-if not false))]
    (->> sig (z/sample-on timeouts))))
