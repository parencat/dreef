(ns dreef.editor-tool
  (:require
   [rumext.v2 :as mf]
   [dreef.tool :refer [tabs]]))


(mf/defc editor []
  [:& tabs {:tabs [{:title "MyAwesomeThing" :active true}
                   {:title "MyAwesomeThing1"}
                   {:title "MyAwesomeThing2"}
                   {:title "MyAwesomeThing3" :type :no-icon}
                   {:title "MyAwesomeThing4"}
                   {:title "MyAwesomeThing5" :type :no-icon}
                   {:title "MyAwesomeThing6"}
                   {:title "MyAwesomeThing7"}
                   {:title "MyAwesomeThing8"}]}])
