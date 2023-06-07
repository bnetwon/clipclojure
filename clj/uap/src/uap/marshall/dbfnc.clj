(ns uap.marshall.dbfnc
  "Classpath into db"
  (:require [clojure.java.jdbc :refer :all])
  (:gen-class)
  )
;db initialize
(.mkdir (java.io.File. (System/getProperty "user.dir") "db"))

(defrecord TableObj [tblname columns indexes] )

;SAMPLE CREATE
; PRIMARY KEY("timestamp","url")
; unique(post_id, tag_id)
; ["FOREIGN KEY(tag_id) REFERENCES tags(tag_id)"]
;CREATE INDEX pn ON news (	url,	title);

(def pathrecord  [ [:pathseq     :AI       ]
                   [:path        :text     ]])
                                    
(def resrecord   [ [:pathseq     :numeric  ]
                   [:resourceseq :AI       ]
                   [:resource    :text     ]])

(def pathlist (TableObj. :pathlist 
                          pathrecord  
                          nil ))
(def reslist  (TableObj. :reslist 
                          resrecord  
                          nil ))


(def testdata
  { :url "http://example.com",
   :title "SQLite Example",
   :body "Example using SQLite with Clojure"
   })

(def dbspec
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "db/database.db"
   })

(defn create-db [^TableObj tableobj](
 let[tblname (:tblname tableobj)
     columns (:columns tableobj) ] (try (clojure.java.jdbc/db-do-commands dbspec
                       (clojure.java.jdbc/create-table-ddl tblname
                                         columns))
       (catch Exception e
         (println (.getMessage e))))) )



(defn print-result-set
  "prints the result set in tabular form"
  [result-set]
  (doseq [row result-set]
    (println row)))


(defn select
  "execute query and return lazy sequence"
  []
  (clojure.java.jdbc/query dbspec ["select * from news"]))


(defn -main
  "launch!"
  []
  (create-db)
  (clojure.java.jdbc/insert! dbspec :news testdata)
  ;; (print-result-set (output)
  )

;' (.mkdir (java.io.File. "/path/to/dir/to/create"))
;=> #clojure.reflect.Constructor{:name "do_reflect", 
;;                               :declaring-class "clojure.reflect.JavaReflector",
;;                               :parameter-types ["java.lang.Object"],
;;                               :exception-types [],
;;                               :flags #{:public}}

;; :bases            a set of names of the type's bases
;; :flags            a set of keywords naming the boolean attributes
;;                   of the type.
;; :members          a set of the type's members. Each member is a map
;;                   and can be a constructor, method, or field.
;;  Keys common to all members:
;; :name             name of the type 
;; :declaring-class  name of the declarer
;; :flags            keyword naming boolean attributes of the member
;;  Keys specific to constructors:
;; :parameter-types  vector of parameter type names
;; :exception-types  vector of exception type names
;;  Key specific to methods:
;; :parameter-types  vector of parameter type names
;; :exception-types  vector of exception type names
;; :return-type      return type name
;;  Keys specific to fields:
;; :type             type name
;;  Options:
;;    :ancestors     in addition to the keys described above, also
;;                  include an :ancestors key with the entire set of
;;                  ancestors, and add all ancestor members to
;;                  :members.
;;   :reflector     implementation to use. Defaults to JavaReflector,
;;                  AsmReflector is also an option.
;; (System/getProperty "user.dir") 
;; or
;; (-> (java.io.File. ".") .getAbsolutePath)
;;(sql/update! db :players {:name "Smith, Steve" :score 42 :active true} ["jerseyNum = ?" 99])
; As an alternative to update!, use execute!
;;(sql/execute! db ["UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?" "Smith, Steve" 42 true 99])

;;(j/query mysql-db
;;  ["select * from fruit where appearance = ?" "rosy"]
;;  {:row-fn :cost})
;;(query db
;;       ["select name,email from users where id = ?" 1]  ;;(a)
;;       :result-set-fn doall                             ;;(b)
;;       :row-fn identity                                 ;;(c)
;;       :identifiers clojure.string/lower-case           ;;(d)
;;       :as-arrays? false)                               ;;(e)
;;(drop-table-ddl :foo :entities clojure.string/upper-case)


;;CREATE TABLE IF NOT EXISTS products (
;;    id          INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
;;    name        TEXT    NOT NULL,
;;    description TEXT    NULL,
;;    price       INTEGER NOT NULL,
;;    discount    INTEGER NOT NULL DEFAULT 0,
;;    reg_date    TEXT    NOT NULL
;;);
;;SQLite Q
