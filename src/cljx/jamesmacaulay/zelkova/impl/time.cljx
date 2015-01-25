(ns jamesmacaulay.zelkova.impl.time
  "Implementation details for `jamesmacaulay.zelkova.time`.")

#+clj
(defn now [] (System/currentTimeMillis))
#+cljs
(defn now [] (.valueOf (js/Date.)))
