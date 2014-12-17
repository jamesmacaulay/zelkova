#+clj
(ns jamesmacaulay.zelkova.time
  (:refer-clojure :exclude [second delay])
  (:require [jamesmacaulay.zelkova.platform.time :as t]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [clojure.core.async :as async :refer [>! <! go go-loop]]))

#+cljs
(ns jamesmacaulay.zelkova.time
  (:refer-clojure :exclude [second delay])
  (:require [jamesmacaulay.zelkova.platform.time :as t]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
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

(def ^:private timestamp-vector
  (juxt (comp impl/timestamp impl/origin-event)
        impl/value))

(defn timestamp
  [sig]
  (impl/make-signal {:init [0 (impl/init sig)]
                     :sources [sig]
                     :msg-fn (fn [_ [msg]]
                               (when (impl/fresh? msg)
                                 (impl/fresh (timestamp-vector msg))))}))

#+cljs
(defn delay
  [ms sig]
  (z/splice (fn [to from]
              (let [waiting (async/chan 1000)
                    fire! #(async/take! waiting (partial async/put! to))]
                (go-loop []
                  (let [v (<! from)]
                    (if (nil? v)
                      (async/close! to)
                      (do
                        (>! waiting v)
                        (js/setTimeout fire! ms)
                        (recur)))))))
            (impl/init sig)
            sig))

