#+clj
(ns jamesmacaulay.zelkova.window
  (:refer-clojure :exclude [meta])
  (:require [jamesmacaulay.zelkova.signal :as z]
            [clojure.core.async :as async]))

#+cljs
(ns jamesmacaulay.zelkova.window
  (:refer-clojure :exclude [meta])
  (:require [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.async-tools.core :as tools]
            [goog.events :as events]
            [cljs.core.async :as async :refer [>! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

#+cljs
(defn- listen
  [el type & args]
  (let [out (apply async/chan 1 args)]
    (events/listen el type (fn [e] (async/put! out e)))
    out))

(defn- get-size
  [opts]
  #+cljs
  (let [node (or (:dom-node opts) (.-body js/document))
        w (.-clientWidth node)
        h (.-clientHeight node)]
    [w h])
  #+clj
  [500 500])

(defn dimensions-channel
  [graph opts]
  #+cljs
  (tools/concat (async/to-chan [(get-size opts)])
                (listen js/window "resize" (map #(get-size opts))))
  #+clj
  (async/chan))

(def dimensions
  (z/input [0 0] ::dimensions dimensions-channel))

(def width (z/lift first dimensions))

(def height (z/lift second dimensions))
