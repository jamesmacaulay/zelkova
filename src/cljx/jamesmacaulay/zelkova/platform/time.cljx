(ns jamesmacaulay.zelkova.platform.time)

#+clj
(defn now [] (System/currentTimeMillis))
#+cljs
(defn now [] (.valueOf (js/Date.)))
