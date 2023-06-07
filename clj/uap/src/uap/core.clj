(ns uap.core)


(def labelStr "insert into aeonbo.CBM_LABEL_PRT (I_COMPANYCODE,I_STORECODE,STR_PRINTER_CODE,STR_PRINTTYPE,STR_PRINTERNAME,STR_PRINTERADDRESS,STR_HOUSEDIRECTORY,STR_FTPUSER,STR_FTPPASSWORD,STR_MAKEINF,I_MAKEDATETIME,STR_LASTUPDATEINF,I_LASTUPDATEDATETIME,STR_PRINTERADDRESS_STAND,I_PRIORITY) values ('6400','804010','$IP','30','$NAME','$IP','PRTDATA','HQMDBO','mdbo','VINX','to_number(to_char(localtimestamp, 'yyyymmddhh24missff2'))','VINX','to_number(to_char(localtimestamp, 'yyyymmddhh24missff2'))','$IP','2') ")

(def helpStr "swing-clip.clipboard,swing-clip.sql")
(def helpStrS "(use '[swing-clip.clipboard] '[swing-clip.sql])")

(defmacro def- [item value]
  `(def ^{:private true} ~item ~value)
)
(defn -main [& args]
  "メイン関数."
  (println args))
;
;(defn split-data 
; ([ tstr b ] 
;  (list (apply str (take b tstr))
;        (apply str (drop b tstr)) )
;  )
; ([ tmplist tmpstr b & colls ] 
;   ( if (seqable? tmplist)
;   ( let [encoding "SJIS"
;          bstr (. tmpstr getBytes  encoding)
;          restcoll (if (seq? (first colls)) (flatten colls) colls )
;          cc (count restcoll) ] 
;    (if (not (empty? restcoll))
;      (let [fstr (new String (byte-array (take b bstr)) encoding)  ]
;       (-> tmplist 
;         (conj fstr ) 
;         (into (split-data tmplist (new String  (byte-array (drop b bstr ))  encoding ) (first restcoll) (rest restcoll))))
;      )
;    ;ELSE
;      (into tmplist (split-data tmpstr b))
;    )
;   )
;   ;not list
;   (split-data []  tmplist tmpstr b colls )
; )))
; 
;;(defn split-bytes ([ str b] (
;; if (instance? String str)
;; (let [bstr (. str getBytes  "SJIS") ]( list))
;; (new String bstr 0 b "SJIS" )   (new String bstr b (- (count bstr) b )  "SJIS")
;;
;; ((take b str) (drop b str))
;;
;; ([ str b & colls] ())
;; let [cc (count colls)]
;; (if (> cc 1)
;;    (let [sbs (split-bytes   str b) ]( list))
;;   (take 1 sbs ) (do (split-bytes (drop 1 sbs) (take 1 colls) (drop 1 colls)))))))
;;
;;    (split-bytes  str b)))
;;;(defn split-bytes ([ stra b] (
;;; if (instance? String stra)
;;; (let [bstra (. stra getBytes  "SJIS") ]
;;; (list (new String bstra 0 b "SJIS" )   (new String bstra b (- (count bstra) b )  "SJIS")))
;;;
;;; ((take b stra) (drop b stra))))
;;;
;;; ([ stra b & colls] 
;;;  ( let [cc (count colls)]
;;;   (if (> cc 0)
;;;    (let [sbs (. stra getBytes  "SJIS") 
;;;          bbb (prn b)
;;;          sss (prn "aa")
;;;          ss  (prn  (new String sbs 0 b "SJIS" ))  ]
;;;     (list (new String sbs 0 b "SJIS" ) 
;;;       (split-bytes (new String  sbs b (- (count sbs) 1) "SJIS") (take 1 colls) (drop 1 colls))) )
;;;    (split-bytes  stra b)))))
;;;
;;;;(take 1 sbs )
;
;
;
;;(require 'uap.clip-sample)
;
;;(split-bytes "abcde" 1 1 1 1)
(use 'swing-clip.clipboard)
(use 'swing-clip.sql)
;;(use 'swing-clip.sqldb)
;(require 'inspector.core)
;
