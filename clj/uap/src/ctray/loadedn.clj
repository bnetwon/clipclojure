(ns ctray.state
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s :refer [fdef]]))

(require '[clojure.java.io :as io] '[clojure.edn :as edn])
(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (io/reader source)]
      (edn/read (java.io.PushbackReader. r)))

    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))
      
      (load-edn "resources/test.edn")