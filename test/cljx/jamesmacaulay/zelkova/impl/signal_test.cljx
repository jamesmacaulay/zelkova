#+clj
(ns jamesmacaulay.zelkova.impl.signal-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [clojure.core.async :as async :refer [go go-loop chan to-chan <! >!]]
            [clojure.core.async.impl.protocols :as async-impl]
            [jamesmacaulay.async-tools.test :refer (deftest-async)]
            [clojure.test :refer (deftest is are testing)])
  (:import [java.util.Date]))

#+cljs
(ns jamesmacaulay.zelkova.impl.signal-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [cljs.core.async :as async :refer [chan to-chan <! >!]]
            [cljs.core.async.impl.protocols :as async-impl]
            [jamesmacaulay.async-tools.test :refer-macros (deftest-async)]
            [cemerick.cljs.test :refer-macros (deftest is are testing)])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(deftest-async test-msg-fn-takes-event-and-previous-message-and-sequence-of-source-messages-and-returns-a-message
  (go
    (let [in1 (z/input 0 :in1)
          in2 (z/input 0 :in2)
          sig (impl/make-signal {:init-fn (constantly :test-init)
                                 :sources [in1 in2]
                                 :msg-fn  (fn [& args] (impl/fresh args))})
          live-graph (z/spawn sig)
          output (async/tap live-graph
                            (chan 1 impl/fresh-values))
          event1 (impl/->Event :in1 1 101)
          event2 (impl/->Event :in2 2 102)
          expected1 [event1
                     (impl/fresh :test-init)
                     [(impl/fresh 1) (impl/cached 0)]]
          expected2 [event2
                     (impl/fresh expected1)
                     [(impl/cached 1) (impl/fresh 2)]]]
      (async/onto-chan live-graph [event1 event2])
      (is (= [expected1 expected2]
             (<! (async/into [] output)))))))

;
;(defn handle-event
;  [graph-signal graph-state event-topic event-value event-timestamp]
;  (let [event-inputs (-> graph-signal (impl/inputs-by-topic) (get event-topic))
;        sorted-sigs (impl/topsort graph-signal)
;        signal->index (zipmap sorted-sigs (range))
;        kids-map (impl/kids-map graph-signal)
;        signals->sorted-index-set #(into (sorted-set) (map signal->index) %)
;        kid-indexes-map (zipmap (keys kids-map)
;                                (map signals->sorted-index-set (vals kids-map)))]
;    (loop [fresh-values (zipmap event-inputs (repeat event-value))
;           dirty-indexes (into (sorted-set) (mapcat kid-indexes-map event-inputs))]
;      (let [signal->message (fn [sig]
;                              (if (contains? fresh-values sig)
;                                (impl/fresh (get fresh-values sig))
;                                (impl/cached (get graph-state sig))))]
;        (if (empty? dirty-indexes)
;          (merge graph-state fresh-values)
;          (let [index (first dirty-indexes)
;                sig (get sorted-sigs index)
;                msg-fn (:msg-fn sig)
;                sig-value (impl/value (msg-fn (get graph-state sig)
;                                              (mapv signal->message
;                                                    (:sources sig))))]
;            (recur (assoc fresh-values sig sig-value)
;                   (disj dirty-indexes index))))))))
;
;(deftest test-handle-event
;  (let [in1 (z/input 0 :in1)
;        in2 (z/input 0 :in2)
;        graph-signal (z/map vector in1 in2)
;        sorted-sigs (impl/topsort graph-signal)
;        graph-state (zipmap sorted-sigs
;                            (for [sig sorted-sigs]
;                              ((:init-fn sig) nil {})))
;        graph-state' (handle-event graph-signal graph-state :in1 1 99)]
;    (is (= {in1 1 in2 0 graph-signal [1 0]}
;           graph-state'))))
;
;(defn graph-init-state
;  [signal live-graph opts]
;  (let [sorted-sigs (impl/topsort signal)]
;    (-> (zipmap sorted-sigs
;                (for [sig sorted-sigs]
;                  ((:init-fn sig) live-graph opts)))
;        (with-meta {:graph-signal signal}))))
;
;(defn- wrap-msg-fn
;  [msg-fn]
;  (fn [prev ]))
;
;(defn exposed-state-transducer
;  ([signal graph-atom]
;   (exposed-state-transducer signal graph-atom impl/topic impl/value impl/timestamp))
;  ([signal graph-atom topic-fn value-fn timestamp-fn]
;   (let [kid-indexes-map (impl/kid-indexes-map signal)
;         inputs-by-topic (impl/inputs-by-topic signal)]
;     (fn [rf]
;       (fn
;         ([] (rf))
;         ([result] (rf result))
;         ([result event]
;          (let [topic (topic-fn event)
;                value (value-fn event)
;                timestamp (timestamp-fn event)
;                event-inputs (get inputs-by-topic topic)
;                init-messages (zipmap event-inputs (repeat (impl/fresh value event)))
;                init-dirty-indexes (into (sorted-set)
;                                         (mapcat kid-indexes-map)
;                                         event-inputs)]
;            (loop [fresh-values init-fresh-values
;                   dirty-indexes init-dirty-indexes]
;              (let [signal->message (fn [sig]
;                                      (if (contains? fresh-values sig)
;                                        (impl/fresh (get fresh-values sig))
;                                        (impl/cached (get graph-state sig))))])))))))))
;

;(defn atom-state-transducer
;  [signal graph-state-atom topic-fn value-fn]
;  (let [state ()
;        rf (fn [result [timestamp topic value :as event]]
;             (let [state @graph-state-atom]))]
;    (fn
;      ([] (rf))
;      ([result] (rf result)))))

;
;(deftest atom-state-transducer-test
;  (let [in1 (z/input 0 :in1)
;        in2 (z/input 0 :in2)
;        combined (z/map vector in1 in2)
;        state-atom (atom {})
;        atom-state-transducer (atom-state-transducer )]))

