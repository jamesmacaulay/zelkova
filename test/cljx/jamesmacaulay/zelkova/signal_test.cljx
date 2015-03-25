#+clj
(ns jamesmacaulay.zelkova.signal-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [clojure.core.async :as async :refer [go go-loop chan to-chan <! >!]]
            [clojure.core.async.impl.protocols :as async-impl]
            [jamesmacaulay.async-tools.test :refer (deftest-async)]
            [clojure.test :refer (deftest is are testing)])
  (:import [java.util.Date]))

#+cljs
(ns jamesmacaulay.zelkova.signal-test
  (:require [jamesmacaulay.async-tools.core :as tools]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.impl.signal :as impl]
            [cljs.core.async :as async :refer [chan to-chan <! >!]]
            [cljs.core.async.impl.protocols :as async-impl]
            [jamesmacaulay.async-tools.test :refer-macros (deftest-async)]
            [cemerick.cljs.test :refer-macros (deftest is are testing)])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn event-constructor
  [topic]
  (partial impl/make-event topic))

(deftest test-signal-sources
  (let [input (z/input 0)
        foldp (z/foldp + 0 input)
        mapped (z/map vector input foldp)
        async (z/async mapped)]
    (are [sig sources] (= (impl/signal-deps sig) sources)
         input #{}
         foldp #{input}
         mapped #{input foldp}
         async #{mapped})))

(deftest test-memoized-graph-calculations
  (let [input (z/input 0 :some-topic)
        foldp (z/foldp + 0 input)
        mapped (z/map vector input foldp)
        async (z/async mapped)]
    (are [sig parents-map] (= (impl/parents-map sig) parents-map)
         input {input #{}}
         foldp {input #{}
                foldp #{input}}
         mapped {input #{}
                 foldp #{input}
                 mapped #{input foldp}}
         async {input #{}
                foldp #{input}
                mapped #{input foldp}
                async #{mapped}})
    (are [sig kids-map] (= (impl/kids-map sig) kids-map)
         input {input #{}}
         foldp {input #{foldp}
                foldp #{}}
         mapped {input #{foldp mapped}
                 foldp #{mapped}
                 mapped #{}}
         async {input #{foldp mapped}
                foldp #{mapped}
                mapped #{async}
                async #{}})
    (are [sig sorted-sigs] (= (impl/topsort sig) sorted-sigs)
         input [input]
         foldp [input foldp]
         mapped [input foldp mapped]
         async [input foldp mapped async])
    (are [sig inputs-by-topic] (= (impl/inputs-by-topic sig) inputs-by-topic)
         input {:some-topic [input]}
         foldp {:some-topic [input]}
         mapped {:some-topic [input]}
         async {:some-topic [input]
                (:relayed-event-topic async) [async]})))

(deftest-async test-wiring-things-up
  (go
    (let [number (event-constructor :numbers)
          letter (event-constructor :letters)
          numbers-input (z/input 0 :numbers)
          letters-input (z/input :a :letters)
          pairs (z/map vector numbers-input letters-input)
          live-graph (z/spawn pairs)
          output (async/tap live-graph
                            (chan 1 impl/fresh-values))]
      (async/onto-chan live-graph
                       [(number 1)
                        (letter :b)
                        (number 2)
                        (letter :c)])
      (is (= [[1 :a] [1 :b] [2 :b] [2 :c]]
             (<! (async/into [] output)))))))

(deftest-async test-io
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          graph (z/spawn in)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 0 (impl/init graph)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [1 2 3]
             (<! (async/into [] out)))))))

(deftest-async test-map
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          incremented (z/map inc in)
          graph (z/spawn incremented)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 1 (impl/init graph)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [2 3 4]
             (<! (async/into [] out)))))
    (let [[a b c] (map event-constructor [:a :b :c])
          ins (map (partial z/input 0) [:a :b :c])
          summed (apply z/map + ins)
          graph (z/spawn summed)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 0 (impl/init graph)))
      (async/onto-chan graph [(a 1) (b 2) (c 3) (a 10)])
      (is (= [1 3 6 15]
             (<! (async/into [] out)))))
    (let [zero-arity-+-map (z/spawn (z/map +))
          zero-arity-vector-map (z/spawn (z/map vector))]
      (is (= 0 (impl/init zero-arity-+-map)))
      (is (= [] (impl/init zero-arity-vector-map))))))

(deftest-async test-foldp
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          sum (z/foldp + 0 in)
          graph (z/spawn sum)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 0 (impl/init graph)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [1 3 6]
             (<! (async/into [] out)))))))

(deftest-async test-regular-signals-are-synchronous
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          decremented (z/map dec in)
          incremented (z/map inc in)
          combined (z/map (fn [a b] {:decremented a
                                            :incremented b})
                                 decremented
                                 incremented)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (async/onto-chan graph (map number [2 10]))
      (is (= [{:decremented 1
               :incremented 3}
              {:decremented 9
               :incremented 11}]
             (<! (async/into [] out)))))))

(deftest-async test-constant
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          foo (z/constant :foo)
          combined (z/map vector in foo)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [0 :foo] (impl/init graph)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [[1 :foo] [2 :foo] [3 :foo]]
             (<! (async/into [] out)))))))

(deftest-async test-merge
  (go
    (let [a (event-constructor :a)
          b (event-constructor :b)
          a-in (z/input 10 :a)
          b-in (z/input 20 :b)
          b-dec (z/map dec b-in)
          b-inc (z/map inc b-in)
          merged (z/merge a-in b-dec b-in b-inc)
          graph (z/spawn merged)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 10 (impl/init graph)))
      (async/onto-chan graph [(a 20) (b 30) (a 40) (b 50)])
      (is (= [20 29 40 49]
             (<! (async/into [] out)))))))

(deftest-async test-combine
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          inc'd (z/map inc in)
          combined (z/combine [in inc'd])
          graph (z/spawn combined)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [0 1] (impl/init graph)))
      (async/onto-chan graph (map number [1 2]))
      (is (= [[1 2] [2 3]]
             (<! (async/into [] out)))))
    (let [empty-combined (z/spawn (z/combine []))]
      (is (= [] (impl/init empty-combined))))))


(deftest-async test-sample-on
  (go
    (let [pos (event-constructor :mouse-position)
          click ((event-constructor :mouse-clicks) :click)
          fake-mouse-position (z/input [0 0] :mouse-position)
          fake-mouse-clicks (z/input :click :mouse-clicks)
          sampled (z/sample-on fake-mouse-clicks fake-mouse-position)
          graph (z/spawn sampled)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [0 0] (impl/init graph)))
      (async/onto-chan graph
                       [(pos [10 10])
                        click
                        (pos [20 20])
                        (pos [30 30])
                        click
                        (pos [40 40])
                        (pos [50 50])
                        click])
      (is (= [[10 10] [30 30] [50 50]]
             (<! (async/into [] out)))))))

(deftest-async test-transducep
  (go
    (let [number (event-constructor :numbers)
          in (z/input 10 :numbers)
          odd-increments (z/transducep (comp (map inc)
                                                   (filter odd?))
                                             conj
                                             in)
          graph (z/spawn odd-increments)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [] (impl/init graph)))
      (async/onto-chan graph (map number [20 21 22 23]))
      (is (= [[21]
              [21 23]]
             (<! (async/into [] out)))))))

(deftest-async test-count
  (go
    (let [in1-event (event-constructor :in1)
          in2-event (event-constructor :in2)
          in1 (z/input 1 :in1)
          in2 (z/input 1 :in2)
          count1 (z/count in1)
          combined (z/map vector count1 in1 in2)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [0 1 1] (impl/init graph)))
      (async/onto-chan graph [(in1-event 2)
                              (in1-event 3)
                              (in2-event 2)
                              (in1-event 4)])
      (is (= [[1 2 1]
              [2 3 1]
              [2 3 2]
              [3 4 2]]
             (<! (async/into [] out)))))))

(deftest-async test-count-if
  (go
    (let [in1-event (event-constructor :in1)
          in2-event (event-constructor :in2)
          in1 (z/input 1 :in1)
          in2 (z/input 1 :in2)
          count1-odd (z/count-if odd? in1)
          combined (z/map vector count1-odd in1 in2)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [0 1 1] (impl/init graph)))
      (async/onto-chan graph [(in1-event 2)
                              (in1-event 3)
                              (in2-event 2)
                              (in1-event 4)
                              (in1-event 5)])
      (is (= [[0 2 1]
              [1 3 1]
              [1 3 2]
              [1 4 2]
              [2 5 2]]
             (<! (async/into [] out)))))))

(deftest-async test-keep-if
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          oddnums (z/keep-if odd? -1 in)
          count-odd (z/count oddnums)
          evennums (z/keep-if even? -2 in)
          count-even (z/count evennums)
          combined (z/map vector oddnums count-odd evennums count-even)
          graph (z/spawn combined)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [-1 0 0 0] (impl/init graph)))
      (async/onto-chan graph (map number [1 2 3]))
      (is (= [[1 1 0 0]
              [1 1 2 1]
              [3 2 2 1]]
             (<! (async/into [] out)))))))

(deftest-async test-keep-when
  (go
    (let [number (event-constructor :numbers)
          letter (event-constructor :letters)
          numbers-in (z/input 0 :numbers)
          letters-in (z/input :a :letters)
          odd-kept-letters (z/keep-when (z/map odd? numbers-in) :false-init letters-in)
          graph (z/spawn odd-kept-letters)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= :false-init (impl/init graph)))
      (is (= :a (impl/init (z/spawn (z/keep-when (z/map even? numbers-in) :z letters-in)))))
      (async/onto-chan graph [(letter :b)
                              (number 1)
                              (letter :c)
                              (number 2)
                              (letter :d)
                              (letter :e)
                              (number 3)
                              (letter :f)])
      (is (= [:c :f] (<! (async/into [] out)))))))

(deftest-async test-drop-repeats
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          no-repeats (z/drop-repeats in)
          graph (z/spawn no-repeats)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 0 (impl/init graph)))
      (async/onto-chan graph (map number [1 1 2 1 2 2 2 1 1]))
      (is (= [1 2 1 2 1] (<! (async/into [] out)))))))

(deftest-async test-world-building-with-value-source-channel-fn
  (go
    (let [value-source (async/to-chan [[10 10]
                                       [20 20]
                                       [30 30]])
          mouse-position (z/input [0 0] :mouse-position (constantly value-source))
          graph (z/spawn mouse-position)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [[10 10] [20 20] [30 30]]
             (<! (async/into [] out)))))))

(deftest-async test-input-with-value-source-mult
  (go
    (let [value-source (async/chan)
          numbers (z/input 0 :numbers (async/mult value-source))
          graph (z/spawn numbers)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (async/onto-chan value-source [1 2 3])
      (is (= [1 2 3]
             (<! (async/into [] out)))))))

(deftest-async test-input-with-value-source-channel
  (go
    (let [value-source (async/chan)
          numbers (z/input 0 :numbers value-source)
          graph (z/spawn numbers)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (async/onto-chan value-source [1 2 3])
      (is (= [1 2 3]
             (<! (async/into [] out)))))))

(deftest-async test-async-makes-signals-asynchronous
  (go
    (let [number (event-constructor :numbers)
          in (z/input 0 :numbers)
          decremented (z/map dec in)
          incremented (z/map inc in)
          async-incremented (z/async incremented)
          combined (z/combine [decremented async-incremented])
          graph (z/spawn combined)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= [-1 1] (impl/init graph)))
      (>! graph (number 1))
      (is (= [0 1] (<! out)))
      (is (= [0 2] (<! out)))
      (>! graph (number 2))
      (is (= [1 2] (<! out)))
      (is (= [1 3] (<! out)))
      (async/close! graph)
      (is (= nil (<! out))))))

(deftest test-template
  (let [tmpl (z/template {:a (z/input 1 :a)
                          :b (z/input 2 :b)})]
    (is (= {:a 1 :b 2}
           (impl/init (z/spawn tmpl))))))

(deftest-async test-pipeline-works-with-transducers
  (go
    (let [ch (async/chan)
          graph (->> ch
                     (z/input [0] :number-vectors)
                     (z/pipeline (comp cat
                                       (map inc)
                                       (filter odd?))
                                 99)
                     (z/spawn))
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 1 (impl/init graph)))
      (async/onto-chan ch [[1 2 3]
                           [4 5 6 7]
                           [8 9]
                           [10]])
      (is (= [3 5 7 9 11]
             (<! (async/into [] out)))))))

(deftest test-pipeline-uses-last-message-in-batch-for-init
  (is (= 4 (->> (z/input [1 2 3 4] :number-vectors)
                (z/pipeline cat 0)
                (z/spawn)
                (impl/init)))))

(deftest test-pipeline-falls-back-to-base-value-for-init
  (is (= 99 (->> (z/input 0 :numbers)
                 (z/pipeline (filter odd?) 99)
                 (z/spawn)
                 (impl/init)))))

(deftest-async test-pipeline-resets-transducer-state-on-every-item-making-stateful-transducers-pretty-much-useless
  (go
    (let [ch (async/chan)
          graph (->> ch
                     (z/input 0 :numbers)
                     (z/pipeline (drop 1) 99)
                     (z/spawn))
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 99 (impl/init graph)))
      (async/onto-chan ch [1 2 3 4])
      (is (= []
             (<! (async/into [] out)))))
    (let [ch (async/chan)
          graph (->> ch
                     (z/input 0 :numbers)
                     (z/pipeline (take 1) 99)
                     (z/spawn))
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= 0 (impl/init graph)))
      (async/onto-chan ch [1 2 3 4])
      (is (= [1 2 3 4]
             (<! (async/into [] out)))))))

(deftest-async test-events-are-timestamped-and-messages-have-references-to-their-origin-events
  (go
    (let [number-event (impl/make-event :numbers 1)
          in (z/input 0 :numbers)
          graph (z/spawn in)
          out-messages (async/tap graph (chan))]
      (async/onto-chan graph [number-event])
      (let [[msg] (<! out-messages)]
        (is (= number-event
               (-> msg
                   (impl/origin-event)
                   (impl/record-timestamp nil))))))))

(deftest-async test-splice
  (go
    (let [ch (async/chan)
          graph (->> ch
                     (z/input 0 :numbers)
                     (z/splice (fn [to from]
                                 (async/pipeline-async 1
                                                       to
                                                       (fn [v ch]
                                                         (go (>! ch v) (async/close! ch)))
                                                       from))
                               (constantly -1))
                     (z/spawn))
          out (async/tap graph (chan 1 impl/fresh-values))]
      (is (= -1 (impl/init graph)))
      (>! ch 1)
      (is (= 1 (<! out)))
      (>! ch 2)
      (is (= 2 (<! out)))
      (async/close! ch)
      (is (= nil (<! out))))
    (let [graph (->> (z/input :a)
                     (z/splice (fn [to from]
                                 (async/pipeline-async 1
                                                       to
                                                       (fn [v ch]
                                                         (go (>! ch v) (async/close! ch)))
                                                       from)))
                     (z/spawn))]
      (is (= :a (impl/init graph))))))

(deftest-async test-activate-when
  (go
    (let [go-to-path (event-constructor :path)
          click (impl/make-event :mouse-clicks :click)
          path (z/input "/" :path)
          mouse-clicks (z/input :click :mouse-clicks)
          foo-path? (z/map #{"/foo"} path)
          clicks-count (z/count mouse-clicks)
          clicks-count-activated-on-foo-path (z/activate-when foo-path? clicks-count)
          combined (z/template {:path path
                                :count clicks-count
                                :count-on-foo clicks-count-activated-on-foo-path})
          graph (z/spawn combined)
          out (async/tap graph (chan 1 impl/fresh-values))]
      (async/onto-chan graph [click
                              (go-to-path "/foo")
                              click
                              click
                              (go-to-path "/bar")
                              click
                              click])
      (is (= [{:path "/" :count 1 :count-on-foo 0}
              {:path "/foo" :count 1 :count-on-foo 0}
              {:path "/foo" :count 2 :count-on-foo 1}
              {:path "/foo" :count 3 :count-on-foo 2}
              {:path "/bar" :count 3 :count-on-foo 2}
              {:path "/bar" :count 4 :count-on-foo 2}
              {:path "/bar" :count 5 :count-on-foo 2}]
             (<! (async/into [] out)))))))