(ns swing-clip.sql-db 
 (:require [clojure.tools.namespace.repl :as tnr]
            [java-jdbc.sql]
            [clojure.java.jdbc :as java-jdbc.sql])
  )
(refer-clojure :exclude '[filter for group-by into partition-by set update])
(require '[honey.sql :as sql]
         ;; CAUTION: this overwrites several clojure.core fns:
         ;;
         ;; filter, for, group-by, into, partition-by, set, and update
         ;;
         ;; you should generally only refer in the specific
         ;; helpers that you want to use!
         '[honey.sql.helpers :refer :all :as h]
         ;; so we can still get at clojure.core functions:
         '[clojure.core :as c])

(def libdb
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "db/lib_db.db"})

(def cdb
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "db/sql_db.db"} )

(defn gettablename [^String pstr  ]
(let 
    [tstr  (-> (clojure.string/trim pstr)
               (clojure.string/upper-case ))]
  (clojure.java.jdbc/query cdb 
    (-> (select :tablenames.*)
        (from :tablenames)
        (where [:=  :tablenames.name_lo tstr ])
        (sql/format)))))

;; (defn select-cdb
;;   ([selsym]
;;     ; ^clojure.lang.Keyword sym]

;;      (java-jdbc.sql/select selsym)))
;;   ([selsym
;;     ^clojure.lang.Keyword sym
;;     whereopt]
;;    (clojure.java.jdbc/query cdb
;;      (java-jdbc.sql/select selsym sym (java-jdbc.sql/where whereopt))))) 
