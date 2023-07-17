(ns dreef.script
  (:require
   [dreef.utils :as utils]
   [potok.core :as ptk]))


(defn update-script-evt
  [{script-id    :id
    script-state :state
    script-doc   :doc}]
  (ptk/reify ::update-script
    ptk/UpdateEvent
    (update [_ state]
      (let [{current-status :status script-text :text} (get-in state [:script script-id])]
        (update-in state [:script script-id] utils/assoc-some
                   :state script-state
                   :status (cond (= current-status :new) :new
                                 (= script-text script-doc) :saved
                                 :otherwise :changed)
                   :text script-text
                   :doc script-doc)))))


(defn save-script-evt
  [{script-id   :id
    script-text :text
    name        :name
    path        :path}]
  (ptk/reify ::save-script
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:script script-id] assoc
                 :status :saved
                 :text script-text
                 :name name
                 :path path
                 :doc ""))))


(defn create-script-evt
  [{script-id :id}]
  (ptk/reify ::create-script
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:script script-id]
                {:id     script-id
                 :state  nil
                 :status :new
                 :text   ""
                 :doc    ""}))))
