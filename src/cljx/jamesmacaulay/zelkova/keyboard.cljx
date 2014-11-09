#+clj
(ns jamesmacaulay.zelkova.keyboard
  (:refer-clojure :exclude [meta])
  (:require [jamesmacaulay.zelkova.signal :as z]
            [clojure.core.async :as async]))

#+cljs
(ns jamesmacaulay.zelkova.keyboard
  (:refer-clojure :exclude [meta])
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

(defn keydown-channel
  [graph opts]
  #+cljs
  (listen js/document "keydown")
  #+clj
  (async/chan))

(defn keyup-channel
  [graph opts]
  #+cljs
  (listen js/document "keyup")
  #+clj
  (async/chan))

(defn blur-channel
  [graph opts]
  #+cljs
  (listen js/window "blur")
  #+clj
  (async/chan))

(def down-events
  (z/input 0 ::down-events keydown-channel))

(def up-events
  (z/input 0 ::up-events keyup-channel))

(def blur-events
  (z/input 0 ::blur-events blur-channel))

(def ^:private empty-state {:alt-key false :meta-key false :key-codes #{}})

(defmulti event-action (fn [state event] (.-type event)))

(defmethod event-action "keydown"
  [state event]
  (-> state
      (update-in [:key-codes] conj (.-keyCode event))
      (assoc :alt (.-altKey event)
             :meta (.-metaKey event))))

(defmethod event-action "keyup"
  [state event]
  (-> state
      (update-in [:key-codes] disj (.-keyCode event))
      (assoc :alt (.-altKey event)
             :meta (.-metaKey event))))

(defmethod event-action "blur"
  [state event]
  empty-state)

(def ^:private key-merge
  (->> (z/merge down-events up-events blur-events)
       (z/reducep event-action empty-state)))

(defn- key-signal
  [f]
  (z/drop-repeats (z/map f key-merge)))

(def keys-down (key-signal :key-codes))

(defn directions
  [up down left right]
  (key-signal (fn [{:keys [key-codes]}]
                {:x (+ (if (key-codes right) 1 0)
                       (if (key-codes left) -1 0))
                 :y (+ (if (key-codes up) 1 0)
                       (if (key-codes down) -1 0))})))

(def arrows (directions 38 40 37 39))

(def wasd (directions 87 83 65 68))

(defn down?
  [code]
  (key-signal (fn [{:keys [key-codes]}]
                (boolean (key-codes code)))))

(def alt (key-signal :alt))

(def meta (key-signal :meta))

(def ctrl (down? 17))

(def shift (down? 16))

(def space (down? 32))

(def enter (down? 13))

(def last-pressed
  (z/map #(.-keyCode %) down-events))
