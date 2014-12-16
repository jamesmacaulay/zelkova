(ns timelord.core
    (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [jamesmacaulay.zelkova.signal :as z]
              [jamesmacaulay.zelkova.mouse :as mouse]
              [jamesmacaulay.zelkova.keyboard :as keyboard]
              [jamesmacaulay.zelkova.time :as time])
    (:import goog.History))

;; -------------------------
;; State
(defonce app-state (atom {:text "Hello, this is: "}))


;; -------------------------
;; Views

(defn main-page []
  [:div [:h2 (:text @app-state) "Page 1"]])

;; -------------------------
;; Initialize app
(defn init! []
  (reagent/render-component [main-page] (.getElementById js/document "app")))

