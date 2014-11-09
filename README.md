# Zelkova

[![Build Status](https://travis-ci.org/jamesmacaulay/zelkova.svg)](https://travis-ci.org/jamesmacaulay/zelkova)

[Elm](http://elm-lang.org/)-style FRP for Clojure and ClojureScript.

[![Clojars Project](http://clojars.org/jamesmacaulay/zelkova/latest-version.svg)](http://clojars.org/jamesmacaulay/zelkova)

## What does it look like?

```clojure
(ns my-app.ui
  (:require [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.keyboard :as keyboard]
            [jamesmacaulay.zelkova.mouse :as mouse]))

(def saved-points-signal
  (let [shift-clicks (z/keep-when keyboard/shift mouse/clicks)]
    (->> mouse/position
         (z/sample-on shift-clicks)
         (z/reducep conj #{}))))

(def saved-points-atom
  (z/pipe-to-atom saved-points-signal))
```

## Demos

* [Mario](http://jamesmacaulay.github.io/zelkova/examples/mario/) ([source](https://github.com/jamesmacaulay/zelkova/blob/gh-pages/examples/mario/src/mario/core.cljs))
* [Drag and Drop](http://jamesmacaulay.github.io/zelkova/examples/drag-and-drop/) ([source](https://github.com/jamesmacaulay/zelkova/blob/gh-pages/examples/drag-and-drop/src/drag_and_drop/core.cljs))

## License

Copyright © 2014

Distributed under the MIT License.
