(ns ^:figwheel-always drag-and-drop.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.mouse :as mouse]))

(enable-console-print!)

(def init-state {:boxes []
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
     :hue (pos->hue centre)
     :resizing? true}))

(defn in-box?
  [[x y]
   {:keys [top left width height]}]
  (and (< left x (+ left width))
       (< top y (+ top height))))

(defn moving?
  [box]
  (contains? box :drag-offset))

(defn resizing?
  [box]
  (:resizing? box))

(defn click
  [pos state]
  (let [without-clicked (remove (partial in-box? pos))]
    (update-in state [:boxes] (partial into [] without-clicked))))

(defn topleft-pos
  [{:keys [left top]}]
  [left top])

(defn start-dragging-box-from-pos
  [pos box]
  (let [offset (->> box (topleft-pos) (map - pos))]
    (assoc box :drag-offset offset)))

(defn start-drag
  [pos state]
  (let [drag-target? (partial in-box? pos)
        {targets true non-targets false} (->> state :boxes (group-by drag-target?))
        boxes' (into [] (concat non-targets
                                (map (partial start-dragging-box-from-pos pos) targets)
                                (when (empty? targets) [(build-box pos pos)])))]
    (assoc state
      :boxes boxes'
      :drag {:start-pos pos})))

(defn drag
  [pos state]
  (let [box-category #(cond (moving? %) :moving
                            (resizing? %) :resizing
                            :else :placed)
        {:keys [placed moving resizing]} (group-by box-category (:boxes state))
        drag-to-pos (fn [box]
                      (let [[left top] (map - pos (:drag-offset box))]
                        (assoc box :left left :top top)))
        boxes' (into [] (concat placed
                                (map drag-to-pos moving)
                                (map (fn [_] (build-box pos (-> state :drag :start-pos)))
                                     resizing)))]
    (assoc state :boxes boxes')))

(defn drop-boxes
  [state]
  (let [drop-box (fn [box] (dissoc box :drag-offset :resizing?))]
    (update-in state [:boxes] (partial into [] (map drop-box)))))

(defn stop-drag
  [state]
  (-> state (drop-boxes) (assoc :drag nil)))

(defrecord NoOp [] IFn (-invoke [_ state] state))
(defrecord Click [pos] IFn (-invoke [_ state] (click pos state)))
(defrecord StartDrag [pos] IFn (-invoke [_ state] (start-drag pos state)))
(defrecord Drag [pos] IFn (-invoke [_ state] (drag pos state)))
(defrecord StopDrag [] IFn (-invoke [_ state] (stop-drag state)))

(def state-signal
  (let [dragging-positions (z/keep-when mouse/down?
                                        [0 0]
                                        mouse/position)
        dragging? (->> (z/constant true)
                       (z/sample-on dragging-positions)
                       (z/merge (z/keep-if not false mouse/down?))
                       (z/drop-repeats))
        dragstarts (z/keep-if identity true dragging?)
        dragstops (z/keep-if not false dragging?)
        click-positions (->> mouse/position (z/sample-on mouse/clicks) (z/drop-when dragging? [0 0]))
        actions (z/merge (z/constant (->NoOp))
                         (z/map ->StartDrag (z/sample-on dragstarts mouse/position))
                         (z/map (constantly (->StopDrag)) dragstops)
                         (z/map ->Drag dragging-positions)
                         (z/map ->Click click-positions))]
    (z/foldp (fn [action state]
               (assoc (action state)
                 :last-action (pr-str action)))
             init-state
             actions)))

(def app-state (z/pipe-to-atom state-signal))

(defn box-color
  [box]
  (let [opacity (if (or (moving? box) (resizing? box)) 0.5 1)]
    (str "hsla(" (:hue box) ",50%,50%," opacity ")")))

(defn render-box
  [box]
  (when box
    (dom/div #js {:style #js {:backgroundColor (box-color box)
                              :position "absolute"
                              :top (:top box)
                              :left (:left box)
                              :width (:width box)
                              :height (:height box)}}
             nil)))

(defn render-state
  [state]
  (dom/div #js {:style #js {:-webkit-touch-callout "none"
                            :-webkit-user-select "none"
                            :-khtml-user-select "none"
                            :-moz-user-select "none"
                            :-ms-user-select "none"
                            :user-select "none"}}
           (apply dom/div nil
                  (map render-box (:boxes state)))
           (dom/div #js {:style #js {:position "relative"}}
                    (dom/h1 nil "Drag and drop")
                    (dom/p nil
                           (dom/a #js {:href "https://github.com/jamesmacaulay/zelkova/blob/gh-pages/examples/drag-and-drop/src/drag_and_drop/core.cljs"}
                                  "View source")
                           ".")
                    (dom/p nil
                           "Drag to create boxes, drag to move them around, and click to remove them.")
                    (dom/pre nil (.stringify js/JSON (clj->js state) nil 2)))))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (render-state app))))
  app-state
  {:target (. js/document (getElementById "app"))})

