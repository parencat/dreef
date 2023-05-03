(ns dreef.core
  (:require
   [rumext.v2 :as mf]
   [applied-science.js-interop :as j]
   ["ui-box" :default box]
   [dreef.layout :refer [layout-manager]]
   [dreef.styles :refer [colors]]))


(mf/defc app []
  [:> box {:background-color (:polar0 colors)
           :color            (:snow2 colors)
           :height           "100vh"
           :overflow         "hidden"}
   [:& layout-manager]])


(defonce root
  (-> js/document
      (j/call :getElementById "root")
      (mf/create-root)))


(defn mount []
  (mf/render! root (mf/element app)))


(defn ^:export start []
  (mount))
