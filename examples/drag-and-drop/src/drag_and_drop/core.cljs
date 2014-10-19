(ns drag-and-drop.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [>! <!]]
            [jamesmacaulay.zelkova.signal :as z :refer [lift sample-on foldp reducep]]
            [jamesmacaulay.zelkova.mouse :as mouse]
            [jamesmacaulay.zelkova.time :as time]
            [jamesmacaulay.async-tools.core :as tools])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def init-state {:placed-boxes []
                 :drag nil})

(defn pos->hue
  [[x y]]
  (mod (+ (/ x 2) (/ y 2)) 360))

(defn build-box
  [[x1 y1] [x2 y2]]
  (let [[top bottom] (sort [y1 y2])
        [left right] (sort [x1 x2])
        centre [(/ (+ left right) 2)
                (/ (+ top bottom) 2)]]
    {:top top
     :left left
     :width (- right left)
     :height (- bottom top)
     :hue (pos->hue centre)}))

(defn in-box?
  [[x y]
   {:keys [top left width height]}]
  (and (< left x (+ left width))
       (< top y (+ top height))))

(defn all-boxes
  [state]
  (-> (:placed-boxes state)
      (concat (-> state :drag :moving-boxes))
      (conj (-> state :drag :resizing-box))))

(defn clicked-boxes
  [pos state]
  (->> (all-boxes state)
       (into #{} (filter (partial in-box? pos)))))

(defn remove-boxes
  [clicked-boxes state]
  (-> state
      (update-in [:placed-boxes] (partial remove clicked-boxes))
      (update-in [:drag :moving-boxes] (partial remove clicked-boxes))
      (update-in [:drag :resizing-box] (fn [box] (if (clicked-boxes box) nil box)))))

(defn click
  [pos state]
  (remove-boxes (clicked-boxes pos state) state))

(defn topleft-pos
  [{:keys [left top]}]
  [left top])

(defn start-drag
  [pos state]
  (let [drag-target? (partial in-box? pos)
        drag-offset (comp (partial map - pos) topleft-pos)
        state-with-drag (reduce (fn [state box]
                                  (if (drag-target? box)
                                    (update-in state [:drag :moving-boxes] conj (assoc box :drag-offset (drag-offset box)))
                                    (update-in state [:placed-boxes] conj box)))
                                (assoc state
                                  :placed-boxes []
                                  :drag {:moving-boxes []
                                         :resizing-box nil
                                         :start-pos pos})
                                (:placed-boxes state))]
    (if (-> state-with-drag :drag :moving-boxes seq)
      state-with-drag
      (assoc-in state-with-drag
                [:drag :resizing-box]
                (build-box pos pos)))))

(defn drag
  [pos state]
  (let [drag-to-pos (fn [box]
                      (let [[left top] (map - pos (:drag-offset box))]
                        (assoc box :left left :top top)))]
    (-> state
        (update-in [:drag :moving-boxes] (partial mapv drag-to-pos))
        (update-in [:drag :resizing-box] (fn [box]
                                           (when box
                                             (build-box pos (-> state :drag :start-pos))))))))
(defn drop-moving-boxes
  [state]
  (-> state
      (update-in [:placed-boxes]
                 (fn [boxes]
                   (let [moved-boxes (map #(dissoc % :drag-offset)
                                          (-> state :drag :moving-boxes))]
                     (into [] (concat boxes moved-boxes)))))))

(defn drop-resizing-box
  [state]
  (-> state
      (update-in [:placed-boxes]
                 (fn [boxes]
                   (if-let [resizing-box (-> state :drag :resizing-box)]
                     (conj boxes resizing-box)
                     boxes)))))

(defn stop-drag
  [state]
  (-> state
      drop-moving-boxes
      drop-resizing-box
      (assoc :drag nil)))

(defrecord NoOp [] IFn (-invoke [_ state] state))
(defrecord Click [pos] IFn (-invoke [_ state] (click pos state)))
(defrecord StartDrag [pos] IFn (-invoke [_ state] (start-drag pos state)))
(defrecord Drag [pos] IFn (-invoke [_ state] (drag pos state)))
(defrecord StopDrag [] IFn (-invoke [_ state] (stop-drag state)))

(def state-signal
  (let [dragging-positions (z/keep-when mouse/down?
                                        [0 0]
                                        mouse/position)
        dragging? (z/drop-repeats (z/merge (z/keep-if not false mouse/down?)
                                           (z/lift (constantly true)
                                                   dragging-positions)))
        dragstarts (z/keep-if identity true dragging?)
        dragstops (z/keep-if not false dragging?)
        click-positions (->> mouse/position (z/sample-on mouse/clicks) (z/drop-when dragging? [0 0]))
        actions (z/log (z/merge (z/constant (->NoOp))
                                (z/lift ->StartDrag (z/sample-on dragstarts mouse/position))
                                (z/lift (constantly (->StopDrag)) dragstops)
                                (z/lift ->Drag dragging-positions)
                                (z/lift ->Click click-positions)))]
    (z/foldp (fn [action state]
               (assoc (action state)
                 :action (pr-str action)))
             init-state
             actions)))

(def app-state (z/pipe-to-atom state-signal))

(defn box-color
  [opacity box]
  (str "hsla(" (:hue box) ",50%,50%," opacity ")"))

(defn render-box
  [opacity box]
  (when box
    (dom/div #js {:style #js {:backgroundColor (box-color opacity box)
                              :position "absolute"
                              :top (:top box)
                              :left (:left box)
                              :width (:width box)
                              :height (:height box)}}
             nil)))

(def render-solid-box (partial render-box 1))

(def render-ghost-box (partial render-box 0.5))

(defn render-state
  [state]
  (let [placed-boxes (:placed-boxes state)
        moving-boxes (-> state :drag :moving-boxes)
        resizing-box (-> state :drag :resizing-box)]
    (dom/div #js {:style #js {:-webkit-touch-callout "none"
                              :-webkit-user-select "none"
                              :-khtml-user-select "none"
                              :-moz-user-select "none"
                              :-ms-user-select "none"
                              :user-select "none"}}
      (apply dom/div #js {:class "placed-boxes"}
        (map render-solid-box (:placed-boxes state)))
      (apply dom/div #js {:class "moving-boxes"}
        (map render-ghost-box moving-boxes))
      (render-ghost-box resizing-box)
      (dom/div #js {:style #js {:position "relative"}}
        (dom/h1 nil "Drag and drop")
        (dom/p nil
               (dom/a #js {:href "https://github.com/jamesmacaulay/zelkova/blob/gh-pages/examples/drag-and-drop/src/drag_and_drop/core.cljs"}
                      "View source")
               ".")
        (dom/p nil
               "Drag to create boxes, drag to move them around, and click to remove them.")
        (dom/pre nil (.stringify js/JSON (clj->js state) nil 2))))))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (render-state app))))
  app-state
  {:target (. js/document (getElementById "app"))})
