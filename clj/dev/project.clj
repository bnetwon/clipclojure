(defproject myscript "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  :repl-options {;:init (require 'clojure.tools.namespace.repl)
                 :init-ns swing-clip.clipboard}
  :plugins [[io.aviso/pretty "0.1.37"]]
  :middleware [io.aviso.lein-pretty/inject]
  :dependencies [ [org.clojure/clojure "1.10.1"]
                  [org.clojure/core.async "0.7.559"]
                  [org.clojure/tools.logging "0.5.0"]
                  ;[org.clojure/tools.namespace "0.3.1"]
                  [nrepl "0.6.0"]
                  [org.clojure/java.data "0.1.1"]
                  [org.clojure/clojure-contrib "1.2.0"]
                  [proto-repl "0.3.1"]
                  [seesaw "1.5.0"]
                  ;[org.jsoup/jsoup "1.12.1"]
                  [clj-soup/clojure-soup "0.1.3"]
                  [reply "0.4.4"]
                  ;[clj-inspector/clj-inspector "0.0.16"]
                  [spyscope "0.1.6"]
                  [org.clojure/core.rrb-vector "0.1.1"]
                  [io.aviso/pretty "0.1.37"]
                  [com.stuartsierra/component "0.4.0"]
                  [com.walmartlabs/test-reporting "0.1.0"]
                  [dk.ative/docjure "1.12.0"]
                  [criterium/criterium "0.4.5"]
                  [org.clojure/tools.reader "1.3.2"]
                  [cider/orchard "0.5.5"]
                  [org.clojure/java.jdbc "0.7.11"]
                  [org.xerial/sqlite-jdbc "3.30.1"]
                  [java-jdbc/dsl "0.1.3"]
                  [junegunn/inspector "0.2.0"]]

    ;:resource ["resources/*"]
    :resource-paths ["lib/jacob.jar" "lib/pf-all-7.2.0.jar" "lib/pf-joi-4.0.0.jar"
                     "lib/pf-swing-3.1.0.jar"]
    ;:resource-paths ["lib/ij.jar"]
  :main ^{:skip-aot true} swing-clip.clipboard)
