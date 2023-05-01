(ns dev
  (:refer-clojure :exclude [test])
  (:require
   [clojure.repl :refer :all]
   [fipp.edn :refer [pprint]]
   [clojure.tools.namespace.repl :refer [refresh]]
   [clojure.java.io :as io]
   [duct.core :as duct]
   [duct.core.repl :as duct-repl :refer [auto-reset]]
   [eftest.runner :as eftest]
   [integrant.core :as ig]
   [integrant.repl :refer [clear halt go init prep reset]]
   [integrant.repl.state :refer [config system]]))


(def profiles
  [:duct.profile/dev :duct.profile/local])


(defn read-config []
  (duct/read-config (io/resource "dreef/config.edn")))


(defn test []
  (eftest/run-tests (eftest/find-tests "test")))


(duct/load-hierarchy)

(clojure.tools.namespace.repl/set-refresh-dirs "dev/src" "src" "test")

(integrant.repl/set-prep! #(duct/prep-config (read-config) profiles))


(comment

 (go)

 (reset)
 (auto-reset)

 (halt)

 nil)
