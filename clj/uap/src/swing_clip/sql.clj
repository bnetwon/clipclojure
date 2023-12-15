(ns swing-clip.sql 
 (:use [swing-clip.clipboard] ))

(defn list->sqlin 
  ( [] (set-string (list->sqlin (getlist))))
  ([list] ( str (list 0 ) " in  (" (clojure.string/join "," (drop 1 list  )) ")"  )))

;where ( I_COMPANYCODE	I_INDICATENUMBER	I_PLUCODE	I_STORECODE =  '105	14038905	23000018	1140' ) ( I_COMPANYCODE	I_INDICATENUMBER	I_PLUCODE	I_STORECODE =  '105	14038905	23000001	1140' ) ( I_COMPANYCODE	I_INDICATENUMBER	I_PLUCODE	I_STORECODE =  '105	14038905	23000001	6660' ) ( I_COMPANYCODE	I_INDICATENUMBER	I_PLUCODE	I_STORECODE =  '105	14038905	23000018	6660' ) ( I_COMPANYCODE	I_INDICATENUMBER	I_PLUCODE	I_STORECODE =  '105	14038883	23000001	20100' ) 
;;(defn getsqlwh 
;;  ( [] (set-string (getsqlwh (getlist))))
;;  ([list] 
;;     (->> (map (fn[ listel ]
;;                             ( str "( " (list 0 ) " =  '" listel "' " ") "  ))  ( drop 1 list  ))
;;     (apply str)
;;     (str "where ")
;;       ))
;;  )
;;(defn tabledatawh[tsrt]
;; (let [ tabledata (->> tstr  (clojure.string/split-lines) (map tokens))
;;        head      (first tabledata)
;;        contents  (drop 1 tabledata) ]
;;    (apply  str "where "  ( ->>
;;      (for [c contents ] (
;;        let [cc (map #(str " = '" %1 "'") c)]
;;   
;;        (->> (zipmap head cc)  (interpose " and ") (flatten ) ( apply str)  )
;;         ))
;;      (map #(str "( " %1 " )"))  ( interpose " or \n" ) ))) )

;(defn list->sqlor
;  ( [] (set-string (list->sqlor (getlist))))
;  ([list] ( str (list 0 ) " in  (" (clojure.string/join " or " (drop 1 list  )) ")"  )))


(defn tabledatawh [tstr]
 (let [ tabledata (->> tstr  (clojure.string/split-lines) (map #(clojure.string/split %1 #"\t")))
        head      (first tabledata)
        contents  (drop 1 tabledata) ]
    (apply  str "where "  ( ->>
      (for [c contents ] (
        let [cc (map #(str " = '" %1 "'") c)]
   
        (->> (zipmap head cc)  (interpose " and ") (flatten ) ( apply str)  )
         ))
      (map #(str "( " %1 " )"))  ( interpose " or \n" ) ))) )



(defn tabledatain[tstr]
 (let [ tabledata (->> tstr  (clojure.string/split-lines) )
        sptab     (fn [tstr ] (clojure.string/split tstr #"\t"))
        head      (sptab (first tabledata))
        titleflg  (= (count head) 1)
        tablename (if titleflg (first head) "tablename" )
        tabledata (if titleflg (drop 1 tabledata) tabledata )
        head      (sptab (first tabledata)) 
        contents  (drop 1 tabledata) ]
    (apply str ""
           ( interpose "  \n" 
            (for [c contents ]
              ( apply str "insert into " tablename "  ( " 
               (->> head  (interpose " , ") (flatten ) ( apply str ))
               " ) "
               " values ( "
               (->> (sptab c) (#(concat %1 '("")  )) (take (count head) )  (map #(str "'" %1 "'" ) ) (interpose " , ") (flatten ) ( apply str ))
             " ) "
               )))
            )) )

(defn tabledataup[tstr]
 (let [ tabledata (->> tstr  (clojure.string/split-lines)  (map #(clojure.string/split %1 #"\t")))
        head      (first tabledata)
        titleflg  (= (count head) 1)
        tablename (if titleflg (first head) "tablename" )
        tabledata (if titleflg (drop 1 tabledata) tabledata )
        head      (first tabledata) 
        contents  (drop 1 tabledata) ]
    (apply str " "
           ( interpose "  \n" 
            (for [c contents ]
              ( apply str  " set  " 
               (->> (map list head c)   (map #(apply str (first %1) " = '" (last %1) "' " ) )  (interpose " , \n ") (flatten ) ( apply str ))
            
               )))
            )) )

(defn tabledatainsert([tstr]
 (let [ tabledata (->> tstr  (clojure.string/split-lines) )
        sptab     (fn [tstr ] (clojure.string/split tstr #"\t"))
        head      (sptab (first tabledata))
        titleflg  (= (count head) 1)
        tablename (if titleflg (first head) "tablename" )
        tabledata (if titleflg (drop 1 tabledata) tabledata )
        head      (sptab (first tabledata)) 
        contents  (drop 1 tabledata) ]
    (tabledatainsert contents tablename head)) )
 ([tstr tablename head]
 (let [ sptab     (fn [tstr ] (clojure.string/split tstr #"\t")) 
        contents  (first tstr)
        nextcont  (next tstr)] 
 ( if nextcont (lazy-seq ( cons
 (apply str ""
              ( apply str "insert into " tablename "  ( " 
               (->> head  (interpose " , ") (flatten ) ( apply str ))
               " ) "
               " values ( "
               (->> (sptab contents) (#(concat %1 '("")  )) (take (count head) )  (map #(str "'" %1 "'" ) ) (interpose " , ") (flatten ) ( apply str ))
             " ) "
               )
            ) (tabledatainsert nextcont tablename head) )))
            )))
(defn tabledatainsert
([tstr ](tabledatainsert tstr #"\t"))
([tstr sep]
 (let [ tabledata (->> tstr  (clojure.string/split-lines) )
        sptab     (fn [tstr ] (clojure.string/split tstr sep))
        head      (sptab (first tabledata))
        titleflg  (= (count head) 1)
        tablename (if titleflg (first head) "tablename" )
        tabledata (if titleflg (drop 1 tabledata) tabledata )
        head      (sptab (first tabledata)) 
        contents  (drop 1 tabledata) ]
    (tabledatainsert contents tablename head sep)) )
 ([tstr tablename head sep]
 (let [ sptab     (fn [tmpstr ] (clojure.string/split tmpstr sep)) 
        tstr      (if (list? tstr) tstr (list tstr) )
        contents  (first tstr)
        nextcont  (next tstr)
        curr      ( apply str "insert into " tablename "  ( " 
               (->> head  (interpose " , ") (flatten ) ( apply str ))
               " ) "
               " values ( "
               (->> (sptab contents) (#(concat %1 '("")  )) (take (count head) )  (map #(str "'" %1 "'" ) ) (interpose " , ") (flatten ) ( apply str ))
             " ) "
               )] 
 ( if nextcont (lazy-seq ( cons
 (apply str "" curr
              
            ) (tabledatainsert nextcont tablename head sep) )) curr )
            )))
            
(defn tsql-normalize [tstr] (clojure.string/replace tstr #"'NULL'" "NULL" ))
(defn clip2insert [] (set-string (tsql-normalize (tabledatain (get-string)))))
(defn clip2where [] (set-string (tsql-normalize (tabledatawh (get-string)))))
(defn clip2update [](set-string (tsql-normalize (tabledataup (get-string)))))
