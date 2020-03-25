(defproject uap "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "1.0.567"]
                 [org.clojure/java.jdbc "0.7.11"]
                 [org.xerial/sqlite-jdbc "3.30.1"]
                   [org.clojure/java.data "0.2.0"]
                   [org.clojure/clojure-contrib "1.2.0"]
                   [proto-repl "0.3.1"]
                   [seesaw "1.4.5"]
                   [org.jsoup/jsoup "1.11.2"]
                   [clj-soup/clojure-soup "0.1.3"]
                   [reply "0.3.7"]
                   [org.clojure/tools.namespace "0.3.1"]
                   [org.apache.commons/commons-lang3 "3.9"]
                   [org.clojure/data.xml "0.0.8"]
                   [org.tcrawley/dynapath "1.1.0"]
                   [org.clojure/core.rrb-vector "0.0.12"]
                   [compliment "0.3.10"]
                   [mvxcvi/puget "1.2.0"]
                   [fipp "0.6.22"] [mvxcvi/arrangement "1.2.0"]
                   [criterium "0.4.5"]
                   [java-jdbc/dsl "0.1.3"]
                 ]
  :source-paths ["src" "dev"]
  :repl-options {:init-ns uap.core})
