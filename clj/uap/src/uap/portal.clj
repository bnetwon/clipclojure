(ns uap.portal
  (:require [portal.api :as p]
            [clojure.core.protocols :refer [Datafiable]]))
;; for node and jvm
;;(require '[portal.api :as p])

;; for web
;; NOTE: you might need to enable popups for the portal ui to work in the
;; browser.
;;(require '[portal.web :as p])


(def portal (p/open)) ; Open a new inspector
(add-tap #'p/submit) 


;; (require '[clojure.core.protocols :refer [Datafiable]])
 (extend-protocol Datafiable
  java.io.File
  (datafy [^java.io.File this]
    {:name          (.getName this)
     :absolute-path (.getAbsolutePath this)
     :flags         (cond-> #{}
                      (.canRead this)     (conj :read)
                      (.canExecute this)  (conj :execute)
                      (.canWrite this)    (conj :write)
                      (.exists this)      (conj :exists)
                      (.isAbsolute this)  (conj :absolute)
                      (.isFile this)      (conj :file)
                      (.isDirectory this) (conj :directory)
                      (.isHidden this)    (conj :hidden))
     :size          (.length this)
     :last-modified (.lastModified this)
     :uri           (.toURI this)
     :files         (seq (.listFiles this))
     :parentname    (.getAbsolutePath (.getParentFile this))}))
;; or with an extension installed, do:
;;(def p (p/open {:launcher :vs-code}))  ; jvm / node only
;;(def p (p/open {:launcher :intellij})) ; jvm / node only
;;
;;(add-tap #'portal/submit) ; Add portal as a tap> target
;;
;;(tap> :hello) ; Start tapping out values
;;
;;(p/clear) ; Clear all values
;;
;;(tap> :world) ; Tap out more values
;;
;;(prn @p) ; bring selected value back into repl
;;
;;(remove-tap #'p/submit) ; Remove portal from tap> targetset
;;
;;(p/close) ; Close the inspector when done
;;
;;(p/docs) ;(p View docs locally via Portal - jvm / node only
