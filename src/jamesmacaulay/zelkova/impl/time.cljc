(ns jamesmacaulay.zelkova.impl.time
  "Implementation details for `jamesmacaulay.zelkova.time`.")

(defn now []
  #?(:clj (System/currentTimeMillis)
    :cljs (.valueOf (js/Date.))))
