(ns jamesmacaulay.zelkova.window
  "This namespace provides window-related signals."
  (:refer-clojure :exclude [meta])
  #+clj
  (:require [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [clojure.core.async :as async])
  #+cljs
  (:require [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [jamesmacaulay.async-tools.core :as tools]
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

(defn- get-size
  [opts]
  #+cljs
  (let [node (or (:dom-node opts) (.-body js/document))
        w (.-clientWidth node)
        h (.-clientHeight node)]
    [w h])
  #+clj
  [500 500])

(defn- dimensions-channel
  [graph opts]
  #+cljs
  (listen js/window "resize" (map #(get-size opts)))
  #+clj
  (async/chan))

(def ^{:doc "A signal of `[x y]` vectors representing the current dimensions of
the window."}
  dimensions
  (impl/make-signal {:init-fn (fn [_ opts] (get-size opts))
                     :relayed-event-topic ::dimensions
                     :event-sources {::dimensions dimensions-channel}}))

(def ^{:doc "A signal of the current width of the window."}
  width
  (z/map first dimensions))

(def ^{:doc "A signal of the current height of the window."}
  height
  (z/map second dimensions))
