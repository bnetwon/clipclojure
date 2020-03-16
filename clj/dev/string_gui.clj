(ns string-gui
  (:require [swing-clip.clipboard]
            [seesaw.selector :as selector])
  (:import [java.awt Toolkit]
           [java.awt.datatransfer Clipboard DataFlavor ClipboardOwner Transferable StringSelection])
  (:use  [seesaw core table][clojure.repl]))

(use 'seesaw.dev)
(defn center-frame [center]
  (frame :title "cframe" :width 500 :height 400 :on-close :dispose
     :content
    (border-panel :id :bp
      :center (scrollable center))))
      ; :south  (label :id :sel :text "Selection: "))))
(def bp (border-panel))
(def cf (center-frame gp))
(apply config! bp [:south (label :id :sel :text "Selection: ")])
(def gp(grid-panel
         :border "Properties"
         :columns 2
         :items ["Name" (text "Frank")
                 "Address" (text "123 Main St")]))

; (apply config! bp :id :bpname)
; (selector/id-of!* bp :bpname)
; (seesaw.core/value! bp {:sel "bbaa"})
; (seesaw.core/value bp)
; (seesaw.core/value! (select cf [:#sel]) "alal")
; ;(def cf (center-frame bp))
; (show! cf)
