(ns uap.sample
  (:require [clojure.tools.namespace.repl :as tnr]
            [clojure.repl]
            [clojure.reflect :as cr]
            ;[proto-repl.saved-values]
            [clojure.pprint :as pp]
            [seesaw.core]
            ;[uap.java]
            [uap.java.classpath]
            [uap.misc]
            [java-jdbc.sql]
            [clojure.java.jdbc :as java-jdbc.sql]
            [seesaw.table])
  (:import [java.awt Toolkit] [java.awt.datatransfer Clipboard DataFlavor StringSelection])
  ;(:use[user])
  )
(def labelStr "insert into aeonbo.CBM_LABEL_PRT (I_COMPANYCODE,I_STORECODE,STR_PRINTER_CODE,STR_PRINTTYPE,STR_PRINTERNAME,STR_PRINTERADDRESS,STR_HOUSEDIRECTORY,STR_FTPUSER,STR_FTPPASSWORD,STR_MAKEINF,I_MAKEDATETIME,STR_LASTUPDATEINF,I_LASTUPDATEDATETIME,STR_PRINTERADDRESS_STAND,I_PRIORITY) values ('6400','804010','$IP','30','$NAME','$IP','PRTDATA','HQMDBO','mdbo','VINX','to_number(to_char(localtimestamp, 'yyyymmddhh24missff2'))','VINX','to_number(to_char(localtimestamp, 'yyyymmddhh24missff2'))','$IP','2') ")

(declare pathlist-seq)
;(require '[clojure.java.data :as j])
(defn transrecode [tablevec recode]
   (select-keys recode (map #(% 0)(butlast (tablevec 1)))))

;(orchard.java.classpath/classpath-seq (.get (orchard.java.classpath/classpath) 0))
(def cdb
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "db/clojuredatabase.db"})

(defn select-cdb
  ([selsym]
    ; ^clojure.lang.Keyword sym]
   (clojure.java.jdbc/query cdb
     (java-jdbc.sql/select selsym)))
  ([selsym
    ^clojure.lang.Keyword sym
    whereopt]
   (clojure.java.jdbc/query cdb
     (java-jdbc.sql/select selsym sym (java-jdbc.sql/where whereopt)))))

(defn create-classpath-db
  "create classpath db and table"
  []
  (try (clojure.java.jdbc/db-do-commands cdb
                       (clojure.java.jdbc/create-table-ddl :pathlist
                                         [[:url :text]
                                          [:seq :serial :PK] ;[:seq :NUMERIC :PK]
                                          [:timestamp :datetime :default :current_timestamp]]))
       (catch Exception e
         (println (.getMessage e)))))

(defn db-act
  "create classpath db and table"
  ([dbact dkey]
   (try (clojure.java.jdbc/db-do-commands dbact
                        (dbact dkey))
        (catch Exception e
          (println (.getMessage e)))))
  ([dbact dkey opt]
   (try (clojure.java.jdbc/db-do-commands dbact
                        (dbact dkey opt))
        (catch Exception e
          (println (.getMessage e))))))
;(db-act clojure.java.jdbc/drop-table-ddl :pathlist)
(defn drop-table-db
  "create classpath db and table"
  [tablekey]
  (try (clojure.java.jdbc/db-do-commands cdb
                       (clojure.java.jdbc/drop-table-ddl tablekey))
       (catch Exception e
         (println (.getMessage e)))))
;
(defn db-select[table-keyword opt]
  (clojure.java.jdbc/query cdb
    (java-jdbc.sql/select * table-keyword (java-jdbc.sql/where opt))))


(defn clj-name
  [cljname ](
              if (instance?
                   clojure.lang.LazySeq
                   cljname)
             (doseq [chunk cljname](clj-name chunk))
             (
               let [ cljflg (clojure.string/ends-with? cljname ".clj")
                     cngstr (if cljflg (.substring cljname 0 (- (count cljname) 5)) cljname)]
               (do (Thread/sleep 100)
                 (some-> cljname (clojure.string/replace \/ \.)
                   (clojure.string/replace \_ \-)
                   (println))))))
;(reduce (fn[ li [a b] ] (assoc li b a)) {} {:a 1, :b 2})
; (->
;     ; (munge)
;     (read-string)
;     (require))
; (defn ttt[]
;      (->> (orchard.java.classpath/classpath)
;        (map orchard.java.classpath/classpath-seq)
;        (map clj-name)))

;(clojure.java.jdbc/query)

(def resourceseq [:resourceseq
                  [[:pathseq :NUMERIC]
                   [:resourceseq :serial :PK :AI]
                   [:urldetail             :TEXT]
                   [:urldir                :TEXT]
                   [:urlname               :TEXT]
                   ["PRIMARY KEY (resourceseq) "]]])
;

(def sourceseq [:sourceseq
                [[:pathseq :NUMERIC]
                 [:resourceseq :serial :PK :AI]
                 [:urldetail             :TEXT]
                 [:package               :TEXT]
                 [:classname             :TEXT]
                 ["PRIMARY KEY (resourceseq) "]]])
;["PRIMARY KEY (seq ,urldetail)"]]])

(def pathlist [:pathlist
                [[:url :text]
                 [:pathseq :AI :serial] ;[:seq :NUMERIC :PK]
                 [:timestamp :datetime :default :current_timestamp]
                 ["PRIMARY KEY (pathseq) "]]])

(def pathseq [:pathseq
              (resourceseq 1)])

(def javapath [:javapath
               (sourceseq 1)])

(def cljpath [:cljpath
               (sourceseq 1)])

(def javamethod [:javamethod
                  (concat (butlast (resourceseq 1))
                   [[:javaseq       :PK :NUMERIC]
                    [:name                 :text]
                    [:type                 :text]
                    [:returntype           :text]
                    [:declaringclass       :text]
                    [:parametertypes       :text]
                    [:exceptiontypes       :text]
                    [:flags                :text]
                    ["PRIMARY KEY (javaseq )"]])])

;(create-table-)
(def javafield [:javafield
                (concat (butlast (resourceseq 1))
                 [[:javaseq     :PK :NUMERIC]
                  [:name               :text]
                  [:type               :text]
                  [:returntype         :text]
                  [:declaringclass     :text]
                  [:parametertypes     :text]
                  [:exceptiontypes     :text]
                  [:flags              :text]
                  ["PRIMARY KEY (javaseq )"]])])



(defn map->dbname [mapvalue]
  (reduce (fn[res [k v] ](into res {(->
                                     k
                                     name
                                     (clojure.string/replace
                                      #"-" "") keyword)
                                    v}))
          {} mapvalue))

(defn create-table-db
  "create classpath db and table"
  [[name opt]]
  (try (clojure.java.jdbc/db-do-commands cdb
         (clojure.java.jdbc/create-table-ddl name opt))
    (catch Exception e
         (println (.getMessage e)))))

;(into {} (map list (orchard.java.classpath/classpath) (range)))
(defn class-s[]
      (for [[cpobj idx] (into {} (map list (orchard.java.classpath/classpath) (range)))]
          (clojure.java.jdbc/insert! cdb :pathlist {:url cpobj})))
;
(defn class-seq[]
      (doseq [class-path  (orchard.java.classpath/classpath)]
        (doseq [class-info (orchard.java.classpath/classpath-seq class-path)]
          (class-info))))

(defn select-out
  "execute query and return lazy sequence"
  ([^clojure.lang.Keyword sym]
   (let [s-sql (str "select * from " (name sym))]
    (clojure.java.jdbc/query cdb [s-sql]))))

;(create-index :cljpath 1 ["urlname"]);
;(create-index :javapath 1 ["urlname"])
(defn create-index
  "execute query and return lazy sequence"
  ([^clojure.lang.Keyword sym idx opts]
   (let [s-sql (str " CREATE INDEX "(str "px_" idx (name sym)) " ON " (name sym) " ( " (clojure.string/join "," opts) " ); ")]
    (do
     (prn s-sql)
     (try (clojure.java.jdbc/db-do-commands cdb
            s-sql)
          (catch Exception e
            (println (.getMessage e))))))))

(defn pathlist-seq
 "parameters is maps";:resourceseq
 ([seqs]
  (
    let [ seq-id (:pathseq seqs)
          url    (java.net.URL. (:url seqs))]
    (doseq [ chunks (orchard.java.classpath/classpath-seq url)]
     (let [urldetail chunks
           splited-chunks (clojure.string/split chunks #"/")
           aurlpath (butlast splited-chunks)
           package (clojure.string/join "/" aurlpath)
           purename (last splited-chunks)
           isclj (orchard.misc/clj-file? purename)
           isjava (orchard.misc/file-ext?  purename ".class")
           splited-urlname (clojure.string/split purename #"\.")
           purlurl  (clojure.string/join "." aurlpath)
           classname (clojure.string/join "." (butlast splited-urlname))
           retcode {:pathseq      seq-id
                    :urldetail    urldetail
                    :urldir       purlurl
                    :urlname      purename
                    :package      package
                    :classname    classname}]
       (merge seqs retcode)))));(.printStackTrace e))))))
 ([seqs
   range-atom]
  (
    let[seq-id (:pathseq seqs)
        url (java.net.URL. (:url seqs))]
    (doseq [ chunks (orchard.java.classpath/classpath-seq url)]
     (let [urldetail chunks
           splited-chunks (clojure.string/split chunks #"/")
           aurlpath (butlast splited-chunks)
           package (clojure.string/join "/" aurlpath)
           purename (last splited-chunks)
           resouceseq (swap! range-atom inc)
           isclj (orchard.misc/clj-file? purename)
           isjava (orchard.misc/file-ext?  purename ".class")
           splited-urlname (clojure.string/split purename #"\.")
           purlurl  (clojure.string/join "." aurlpath)
           classname (clojure.string/join "." (butlast splited-urlname))
           isexist false;(:seq (db-select :pathseq {:seq seq-id :urldetail urldetail}))
           istarget (> (count urldetail) 0)
           retcode {:pathseq      seq-id
                    :resourceseq  resouceseq
                    :urldetail    urldetail
                    :urldir       purlurl
                    :urlname      purename
                    :package      package
                    :classname    classname}]
       (if-not isexist
         (do
           ;(when (= seq-id 40 ) (prn urldetail))
           (when isclj (try (clojure.java.jdbc/insert! cdb :cljpath (transrecode cljpath retcode))
                         (catch Exception e (do (. e printStackTrace)))))
           (when isjava (try (clojure.java.jdbc/insert! cdb :javapath (transrecode javapath retcode))
                          (catch Exception e (do (. e printStackTrace)))))
           (when istarget (try
                            (clojure.java.jdbc/insert! cdb :pathseq (transrecode pathseq retcode))
                            (catch Exception e (do (. e printStackTrace))))));(.printStackTrace e))))))
        (prn "a")))))))


(defn pathlist-cntup
 "parameters is maps"
  ([seqs
    target-atom
    java-atom
    clj-atom]
   (
     let[seq-id (:pathseq seqs)
         url (java.net.URL. (:url seqs))]
     (doseq [ chunks (orchard.java.classpath/classpath-seq url)]
      (let [urldetail chunks
            splited-chunks  (clojure.string/split chunks #"/")
            aurlpath        (butlast splited-chunks)
            urlpath         (clojure.string/join "/" aurlpath)
            urlname         (last splited-chunks)
            isclj           (orchard.misc/clj-file? urlname)
            isjava          (orchard.misc/file-ext? urlname ".class")
            targetnum       (swap! target-atom inc)
            javanum         (if isjava (swap! java-atom inc) @java-atom)
            cljnum          (if isclj  (swap! clj-atom inc)  @clj-atom)
            splited-urlname (clojure.string/split urlname #"\.")
            purlurl         (clojure.string/join "." aurlpath)
            purename        (clojure.string/join "." (butlast splited-urlname))
            isexist         false
            istarget        (> (count urldetail) 0)
            recode          {:pathseq    seq-id
                             :urldetail  urldetail
                             :urldir     urlpath
                             :urlname    urlname}
            precode         {:pathseq    seq-id
                             :urldetail  urldetail
                             :urldir     purlurl
                             :urlname    purename}]
        (if-not isexist
          (do
            ;(when (= seq-id 40 ) (prn urldetail))
            (when isclj (try (clojure.java.jdbc/insert! cdb :cljpath (transrecode cljpath precode))
                          (catch Exception e (do (prn precode)))))
            (when isjava (try (clojure.java.jdbc/insert! cdb :javapath (transrecode javapath precode))
                           (catch Exception e (do (prn precode)))))
            (when istarget (try
                             (clojure.java.jdbc/insert! cdb :pathseq (transrecode pathseq recode))
                             (catch Exception e (do (prn recode))))));(.printStackTrace e))))))
         (prn recode)))))))
         ;isexist (:pathseq (db-select :pathseq {:pathseq seq-id :urldetail urldetail}))
;

(defn java-seq
 "parameters is maps"
  [seqs](
          let [seq-id     (:pathseq seqs)
               ;url        (java.net.URL. (:urldetail seqs))
               javastr    (clojure.string/join "." (-> [] (conj (:urldir seqs)) (conj (:urlname seqs))))
               javaclass  (read-string javastr)
               vecjavamap (->> javaclass clojure.reflect/reflect :members
                           (map #( (fn[x] ( into {}  x)) %))
                           vec)]
                          ;(#( % 0)))] ;(#(.get % 0)))]
                          ;map->dbname)]
          ( for [ refval vecjavamap
                  :let [dbval (map->dbname refval)]]
           ; (clojure.java.jdbc/insert!
           ;  cdb
           ;  :javaref
            (merge seqs dbval))))
;
; (defn java-seq
;  "parameters is maps"
;   [seqs](
;           let[seq-id     (:pathseq seqs)
;               url        (java.net.URL. (:url seqs))
;               javastr    (clojure.string/join "." (-> [] (conj (:urldir seqs)) (conj (:urlname seqs))))
;               javaclass  (read-string javastr)
;               vecjavamap (->> javaclass clojure.reflect/reflect :members
;                           (map #( (fn[x] ( into {}  x)) %))
;                           vec)]
;                           ;(#( % 0)))] ;(#(.get % 0)))]
;                           ;map->dbname)]
;           (doseq[refval vecjavamap
;                  :let [dbval (map->dbname refval)]]
;            (clojure.java.jdbc/insert!
;             cdb
;             :javaref
;             (merge seqs dbval)))))
;(test-java-seq (take 1 (select-out :javapath)))
(defn test-java-seq ;(take 1 (select-out :javapath))
 "parameters is maps"
  [seqs](
          let [seq-id     (:pathseq seqs)
              ;url        (java.net.URL. (:url seqs))
               javastr    (clojure.string/join "." (-> [] (conj (:urldir seqs)) (conj (:urlname seqs))))
               javaclass  (read-string javastr)
               vecjavamap (->> javaclass clojure.reflect/reflect :members
                           (map #( (fn[x] ( into {}  x)) %))
                           vec)]
                          ;(#( % 0)))] ;(#(.get % 0)))]
                          ;map->dbname)]
            (doseq [refval vecjavamap
                    :let [dbval (map->dbname refval)
                          isfield (some? (:type dbval))]]
              (prn isfield))))
          ;(clojure.java.jdbc/insert! cdb :javaref (merge seqs javamap))))
;

; (defn initpathlistatom[] (
;                           let[target-atom  (atom 0)
;                               java-atom    (atom 0)
;                               clj-atom     (atom 0)]
;                             (doseq [
;                                     recode (select-out :pathlist)]
;                               ( pathlist-cntup
;                                 recode
;                                 target-atom
;                                 java-atom
;                                 clj-atom))))

(defn insertjavarec [recode]
  (let [isfield (some? (:type recode))]
    (if isfield
      (clojure.java.jdbc/insert! cdb :javafield   (transrecode javafield  recode))
      (clojure.java.jdbc/insert! cdb :javamethod  (transrecode javamethod recode)))))


(defn insertpath[] (reduce (
                            fn[result [m v]](
                                              clojure.java.jdbc/insert! cdb :pathlist
                                              {:url (.toString m) :pathseq v}));
                        {} (map list (orchard.java.classpath/classpath) (drop 1 (range)))))
;
(defn insertjavaseq[] (reduce (
                               fn[result [m v]](
                                                 insertjavarec ( assoc m :javaseq v)))
                        {} (map list (flatten (map java-seq (select-out :javapath))) (drop 1 (range)))))

(defn initpathlist[] (let [range-atom (atom 0) ] (doseq [recode (select-out :pathlist)]
                                                  (pathlist-seq recode range-atom))))

(defn initjava[]  (doseq [recode (select-out :javapath)]( java-seq recode)))

(defn initdb[] (map create-table-db  [cljpath
                                      javapath
                                      javamethod
                                      javafield
                                      pathlist
                                      pathseq
                                      resourceseq]))
;
(defn dropdb[] (map #(drop-table-db (% 0))  [cljpath
                                             javafield
                                             javapath
                                             javamethod
                                             pathlist
                                             pathseq
                                             resourceseq]))
(defn refreshing[]
  (do (map #(try (drop-table-db (% 0))
             (catch Exception e
               (. e printStackTrace)))
        [cljpath
          javafield
          javapath
          javamethod
          pathlist
          pathseq
          resourceseq])
      (initdb)
      (insertpath)
      (initpathlist)
      (insertjavarec)))
;
;
;(defn inst[] (doseq [[recode rangename ] (map list (select-out :pathlist) (drop 1 (range)))]
;              (print rangename)))
;(select-cdb "count(*) as count" :pathlist)

;(.get (orchard.java.classpath/classpath-seq))
;        (.get (orchard.java.classpath/classpath) 15)) 1)
;(:bases (clojure.reflect/reflect (orchard.java.classpath/classpath-seq (.get (orchard.java.classpath/classpath) 0))))

; (-> (orchard.java.classpath/classpath)
;     (.get  12)
;     (orchard.java.classpath/classpath-seq)
;     (.get  6))
;(require 'clojure.string/)
; (-> (orchard.java.classpath/classpath)
;     (.get  11)
;     (orchard.java.classpath/classpath-seq)
;     (.get  6)
;     (clojure.string/replace \/ \.)
;     (clojure.string/replace \_ \-)
;     (clojure.string/replace ".clj" "")
;     (read-string)
;     (find-ns))



    ; (map clj-name))

    ;(clojure.reflect/reflect)
    ;(:members)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(java-seq (.get (select-cdb * :javapath {:seq 4}) 0))

;(type (read-string (.get (orchard.java.classpath/classpath-seq (.get (orchard.java.classpath/classpath) 0)) 0)))

;(:members (clojure.reflect/reflect (orchard.java.classpath/classpath-seq (.get (orchard.java.classpath/classpath) 0))))

;;boot-aware-classloader
;;classpath-namespaces

;(classpath-namespaces  (classpath))

; (defn classpath-namespaces
;   "Returns all namespaces defined in sources on the classpath or the specified
;   classpath URLs"
;   ([classpath-urls]
;    (->> (mapcat cp/classpath-seq classpath-urls)
;         (filter misc/clj-file?)
;         (map (comp read-namespace io/resource))
;         (filter identity)
;         (sort)))
;   ([]
;    (classpath-namespaces (cp/classpath))))
