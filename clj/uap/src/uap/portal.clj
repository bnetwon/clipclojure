(na uap.portal
 (:require [portal.api :as p]))
;; for node and jvm
;;(require '[portal.api :as p])

;; for web
;; NOTE: you might need to enable popups for the portal ui to work in the
;; browser.
;;(require '[portal.web :as p])


(def p (p/open)) ; Open a new inspector

;; or with an extension installed, do:
;;(def p (p/open {:launcher :vs-code}))  ; jvm / node only
;;(def p (p/open {:launcher :intellij})) ; jvm / node only
;;
;;(add-tap #'p/submit) ; Add portal as a tap> target
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
;;(p/docs) ; View docs locally via Portal - jvm / node only