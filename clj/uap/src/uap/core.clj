(ns uap.core)


(def labelStr "insert into aeonbo.CBM_LABEL_PRT (I_COMPANYCODE,I_STORECODE,STR_PRINTER_CODE,STR_PRINTTYPE,STR_PRINTERNAME,STR_PRINTERADDRESS,STR_HOUSEDIRECTORY,STR_FTPUSER,STR_FTPPASSWORD,STR_MAKEINF,I_MAKEDATETIME,STR_LASTUPDATEINF,I_LASTUPDATEDATETIME,STR_PRINTERADDRESS_STAND,I_PRIORITY) values ('6400','804010','$IP','30','$NAME','$IP','PRTDATA','HQMDBO','mdbo','VINX','to_number(to_char(localtimestamp, 'yyyymmddhh24missff2'))','VINX','to_number(to_char(localtimestamp, 'yyyymmddhh24missff2'))','$IP','2') ")

(def helpStr "swing-clip.clipboard,swing-clip.sql")
(def helpStrS "(use '[swing-clip.clipboard] '[swing-clip.sql])")

(defn -main [& args]
  "メイン関数."
  (println args))

;(defn split-bytes ([ str b] (
; if (instance? String str)
; (let [bstr (. str getBytes  "SJIS") ]( list))
; (new String bstr 0 b "SJIS" )   (new String bstr b (- (count bstr) b )  "SJIS")
;
; ((take b str) (drop b str))
;
; ([ str b & colls] ())
; let [cc (count colls)]
; (if (> cc 1)
;    (let [sbs (split-bytes   str b) ]( list))
;   (take 1 sbs ) (do (split-bytes (drop 1 sbs) (take 1 colls) (drop 1 colls)))))))
;
;    (split-bytes  str b)))



;(require 'uap.clip-sample)

;(split-bytes "abcde" 1 1 1 1)
