#+clj
(ns jamesmacaulay.async-tools.core
  (:refer-clojure :exclude [concat])
  (:require [clojure.core.async :as async :refer [go go-loop >! <! chan]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :as channels]))

#+cljs
(ns jamesmacaulay.async-tools.core
  (:refer-clojure :exclude [concat])
  (:require [cljs.core.async :as async :refer [>! <! chan]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as channels])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn concat
  [& chs]
  (let [out (chan)]
    (go-loop [remaining chs]
      (let [ch (first remaining)]
        (if (nil? ch)
          (async/close! out)
          (let [v (<! ch)]
            (if (nil? v)
              (recur (next remaining))
              (do
                (>! out v)
                (recur remaining)))))))
    out))

(defn do-effects
  [f! ch]
  (go-loop []
    (let [v (<! ch)]
      (when-not (nil? v)
        (f! v)
        (recur))))
  ch)

(defn log-mult
  [mult]
  (do-effects println (async/tap mult (chan))))

(defn log-channel
  [ch]
  (async/map (fn [x] (println x) x) [ch]))

(deftype ConstantReadPort [boxed-value]
  impl/ReadPort
  (take! [_ _] boxed-value))

(defn constant
  [x]
  (ConstantReadPort. (channels/box x)))

(defn readport?
  [x]
  (satisfies? impl/ReadPort x))

(defn cast-as-readport
  [x]
  (if (readport? x) x (constant x)))
