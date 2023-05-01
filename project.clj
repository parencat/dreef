(defproject dreef "0.1.0-SNAPSHOT"
  :description "new cool journey through tools and data"
  :min-lein-version "2.0.0"

  :dependencies
  [[org.clojure/clojure "1.11.1"]
   [duct/core "0.8.0"]
   [duct/module.logging "0.5.0"]]

  :plugins
  [[duct/lein-duct "0.12.3"]
   [lein-ancient "1.0.0-RC3"]]

  :middleware
  [lein-duct.plugin/middleware]

  :main ^:skip-aot dreef.main
  :source-paths ["src/service"]
  :resource-paths ["resources" "target/resources"]

  :profiles
  {:dev     {:source-paths   ["dev/src"]
             :resource-paths ["dev/resources"]
             :dependencies   [[integrant/repl "0.3.2"]
                              [hawk "0.2.11"]
                              [eftest "0.6.0"]]}

   :repl    {:repl-options {:init-ns dev}}

   :uberjar {:aot :all}})
