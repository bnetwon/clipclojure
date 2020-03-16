(ns clip-sample
  (:require [swing-clip.clipboard])
  (:import [java.awt Toolkit]
           [java.awt.datatransfer Clipboard DataFlavor ClipboardOwner Transferable StringSelection]))
(require 'clojure.tools.namespace.repl)
(def ^Clipboard clip swing-clip.clipboard/clip)
(def afn
  (atom
   (fn
     [^ClipboardOwner owner
      ^Transferable contents
      ^Clipboard c
      ^Transferable t]
    (if contents
     (cond  (. contents isDataFlavorSupported DataFlavor/javaFileListFlavor)
            (prn (. contents getTransferData DataFlavor/javaFileListFlavor))
            (. contents isDataFlavorSupported DataFlavor/stringFlavor)
            (prn (. contents getTransferData DataFlavor/stringFlavor))
            :else nil)))))
(defn resetafn[]
  (reset! afn
    (fn
      [^ClipboardOwner owner
       ^Transferable contents
       ^Clipboard c
       ^Transferable t]
     (if contents
      (cond  (. contents isDataFlavorSupported DataFlavor/javaFileListFlavor)
             (prn (. contents getTransferData DataFlavor/javaFileListFlavor))
             (. contents isDataFlavorSupported DataFlavor/stringFlavor)
             (prn (. contents getTransferData DataFlavor/stringFlavor))
             :else nil)))))
; (def flaver (reify java.awt.datatransfer.FlavorListener
;               (flavorsChanged [this f]
;                (@afn this f))))
(defn sss[x] (cond
               (= x 1)(prn "a")
               (= x 1)(prn "a")
               :else (prn "b")))
(def clipOwner (reify java.awt.datatransfer.ClipboardOwner
                 (^void  lostOwnership [this ^Clipboard c ^Transferable t]
                   (do (Thread/sleep 250)
                       (let [^Transferable contents (. c getContents this)]
                        (do
                         (@afn this contents c t)
                         (. c setContents contents this)))))))
;
(def nilOwner (reify java.awt.datatransfer.ClipboardOwner
                (^void  lostOwnership [this ^Clipboard c ^Transferable t]
                  nil)))

(defn ownerInit[](do (let [^Transferable contents (. clip getContents clipOwner)]
                       (. clip setContents contents clipOwner))))

(defn get-string []
  (when (.isDataFlavorAvailable clip DataFlavor/stringFlavor)
    (.getData clip DataFlavor/stringFlavor)))

(defn get-filelist []
  (when (.isDataFlavorAvailable clip DataFlavor/javaFileListFlavor)
    (.getData clip DataFlavor/javaFileListFlavor)))

(defn cnttt[stra] (if (= (count stra) 0) (println "") (do (def a (+ a 1)) (println (str a)))))
(defn cntttllist[stra cntnum]( let [
                                    nelement (first stra)
                                    nelementcnt (count nelement)
                                    calcnt (if (= nelementcnt 0 ) cntnum (+ cntnum 1))]
                              (if (= nelement nil) nil
                               (cons (if (= nelementcnt 0 ) nil calcnt)
                                (cntttllist (rest stra) calcnt)))))

(defn addalstr[strarr linesep] (if (= (count strarr) 0) ""
                                (str (if (first strarr) (first strarr) "" ) linesep
                                 (addalstr (rest strarr) linesep))))
(defn getlist []
    (clojure.string/split (get-string) #"\n"))

(defn makesomelist []
  (def somelist
    (clojure.string/split (get-string) #"\n")))

(defn set-string [s]
  (let [ss (StringSelection. (print-str s))]
    (.setContents clip ss ss)))
(defn change[ a b ](set-string (clojure.string/replace (get-string) a b)))
(defn changes[ a b ](set-string (clojure.string/replace (get-string) a b)))
(defn set-list[s] (set-string (clojure.string/join "\n" s)))
(defn ddd [strings] (partition 2 (map #(str %) (clojure.string/split strings #"(\t|\n)"))))

(defn stringtomap [strings] (into {} (map vec (partition 2 (map #(str %) (clojure.string/split strings #"(\t|\n)"))))))

(defn stringtomapbyreg [strings regx] (into {} (map vec (partition 2 (map #(str %) (clojure.string/split strings regx))))))

(defn rrten[ r rs ] (if (re-find r rs) rs))

(def regex-char-esc-smap
  (let [esc-chars "()*&^%$#!"]
    (zipmap esc-chars
            (map #(str "\\" %) esc-chars))))

(defn str-to-pattern
  [string]
  (->> string
       (replace regex-char-esc-smap)
       (reduce str)
       re-pattern))
;-table
;
; implements ClipboardOwner {}
;  Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
;
;  public Main() {}
;    // Implement Copy operation
;    StringSelection contents = new StringSelection("data");
;    clipboard.setContents(contents, this);
;    FlavorListener lis = new FlavorListener(){}
;      @Override
;      public void flavorsChanged(FlavorEvent e) {}
;       System.out.println(e);
;
;    ;
;    clipboard.addFlavorListener(lis);
;    clipboard.removeFlavorListener(lis);
;    // Implement Paste operation
;    Transferable content = clipboard.getContents(this);
;    String dstData;
;    try {}
;      dstData = (String) content.getTransferData(DataFlavor.stringFlavor);
;      System.out.println(dstData);
;     catch (Exception e) {}
;      e.printStackTrace();
;
;
;
;  public void lostOwnership(Clipboard clipboard, Transferable contents) {}
;    System.out.println("Clipboard contents replaced");
;
;
;  public static void main(String[] args) {}
;    Main test = new Main();
;
