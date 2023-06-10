(ns dreef.editor-tool
  (:require
   [rumext.v2 :as mf]
   [dreef.tabs :refer [tabs]]))


(mf/defc editor [{:keys [view-id tabs-id]}]
  [:& tabs {:tabs-id tabs-id
            :view-id view-id}])
