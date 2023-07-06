(ns dreef.script
  (:require
   [potok.core :as ptk]))


(defn create-script-evt [{:keys [id]}]
  (ptk/reify ::create-script
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:script id] {:id id :text ""}))))


(defn save-script-state-evt [{:keys [script-id state]}]
  (ptk/reify ::save-script-state
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:script script-id] {:id script-id :state state}))))
