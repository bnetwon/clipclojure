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
              ( apply str "insert into " tablename "  ( " 
               (->> head  (interpose " , ") (flatten ) ( apply str ))
               " ) "
               " values ( "
               (->> c (#(concat %1 '("")  )) (take (count head) )  (map #(str "'" %1 "'" ) ) (interpose " , ") (flatten ) ( apply str ))
               " ) "
               )))
            )) )


(defn tsql-normalize [tstr] (clojure.string/replace tstr #"'NULL'" "NULL" ))
(defn clip2insert [] (set-string (tsql-normalize (tabledatain (get-string)))))
(defn clip2where [] (set-string (tsql-normalize (tabledatawh (get-string)))))
