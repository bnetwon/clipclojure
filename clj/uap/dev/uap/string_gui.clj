(ns uap.string-gui
  (:require [swing-clip.clipboard]
            [seesaw.selector :as selector])
  (:import [java.awt Toolkit]
           [java.awt.datatransfer Clipboard DataFlavor ClipboardOwner Transferable StringSelection])
  (:use  [seesaw core table][clojure.repl]))
(defn ins->map[obj](->> obj
     cr/reflect
     :members (map #( (fn[x] ( into {}  x)) %)) vec
        ))
(def classColumnName [ :name :return-type :declaring-class :parameter-types :exception-types :flags ])
(def members-c-n [{:key :name, :text "name", :class java.lang.String}
 {:key :return-type, :text "return-type", :class java.lang.Object}
 {:key :declaring-class, :text "declaring-class", :class java.lang.Class}
 {:key :parameter-types, :text "parameter-types", :class java.lang.Object}
 {:key :exception-types, :text "exception-types", :class java.lang.Class}
 {:key :flags, :text "flags", :class java.lang.Object} ])

(defn ct-model[obj ](seesaw.table/table-model
                 :columns members-c-n
                 :rows  obj
                         ))

(use 'seesaw.dev)
(defn center-frame [center]
  (frame :title "cframe" :width 500 :height 400 :on-close :dispose
     :content
    (border-panel :id :bp
      :center (scrollable center))))
      ; :south  (label :id :sel :text "Selection: "))))
(def bp (border-panel))
(def gp(grid-panel
         :border "Properties"
         :columns 2
         :items ["Name" (text "Frank")
                 "Address" (text "123 Main St")]))
(def cf (center-frame gp))
;(apply config! bp [:south (label :id :sel :text "Selection: ")])


; (apply config! bp :id :bpname)
; (selector/id-of!* bp :bpname)
; (seesaw.core/value! bp {:sel "bbaa"})
; (seesaw.core/value bp)
; (seesaw.core/value! (select cf [:#sel]) "alal")
; ;(def cf (center-frame bp))
; (show! cf)
;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; A simple example of (table) for basic tabular data

(defn make-table []
  (table :id :table
    :model [
      :columns [ { :key :name :text "Name" } 
                 { :key :town :text "Town" } 
                 { :key :interest :text "Interest" }]
      :rows [{ :name "Kupzog" :town "Cologne" :interest "programming" :id 1234}
             { :name "Hansson" :town "Ystadt" :interest "Hunting" :id 2234}
             { :name "Walter" :town "London" :interest "Rafting" :id 12345}]]))

; Note that the :id key isn't in the :columns spec, but it is still retained
; behind the scenes by the table model.

; The rest is boilerplate ...
(defn make-frame []
  (frame :title "JTable Example" :width 500 :height 400 :content
    (border-panel
      :center (scrollable (make-table))
      :south  (label :id :sel :text "Selection: "))))

(defmacro defexample
  "Does the boilerplate for an example.

  arg-vec is a binding vector of arguments for the example, usually command-line
  args. body is code which must return an instance of javax.swing.JFrame. If the
  frame's size has not been set at all, pack! is called. Then show! is called.

  Defines two functions:

    run   : takes an on-close keyword and trailing args and runs
            the example.
    -main : calls (run :exit & args). i.e. runs the example and exits when
            closed

  See the plethora of examples in this directory for usage examples.
  "
  [arg-vec & body]
  `(do
     (defn ~'run [on-close# & args#]
       (let [~arg-vec args#
             f# (invoke-now ~@body)]
         (config! f# :on-close on-close#)
         (when (= (java.awt.Dimension.) (.getSize f#))
           (pack! f#))
         (show! f#)))

     (defn ~'-main [& args#]
       (apply ~'run :exit args#))))

(defexample []
  (let [f (show! (make-frame))
        t (select f [:#table])]
    ; Listen for selection changes and show them in the label
    (listen t :selection 
      (fn [e] 
        (config! (select f [:#sel]) 
          :text (str "Selection: " 
                     ; (selection t) returns the selected row index
                     ; (value-at t row) returns the record at row
                     (value-at t (selection t))))))
    f))
(def testtable ( table 
  :model [
    :columns [{:key :name, :text "Name"} :likes] 
    :rows '[["Bobby" "Laura Palmer"]
           ["Agent Cooper" "Cherry Pie"]
           {:likes "Laura Palmer" :name "James"}
           {:name "Big Ed" :likes "Norma Jennings"}]]))
(defn ^javax.swing.table.DefaultTableCellRenderer default-table-cell-renderer
  [render-fn]
  (if (instance? javax.swing.table.DefaultTableCellRenderer render-fn)
    render-fn
    (proxy [javax.swing.table.DefaultTableCellRenderer] []
      (^java.awt.Component getTableCellRendererComponent [^javax.swing.JTable table, ^Object value,
                ^Boolean selected?, ^Boolean focus?, ^Integer row, ^Integer column]
                (let [^javax.swing.table.DefaultTableCellRenderer this this]
                (proxy-super getTableCellRendererComponent table value selected? focus? row column )
                ;(proxy-super getListCellRendererComponent component value index selected? focus?)
                (render-fn this { :this      this
                                  :component table
                                  :value     value
                                  :selected? selected?
                                  :focus?    focus?
                                  :row       row
                                  :column    column})
                (apply config! this [:background "#aaaaee"]));(todo this (.setBackgroundColor "#000033") ))
                ; int columnModelIndex = table.getColumnModel().getColumn(column).getModelIndex();
      ))))
(defn omap[obj]
  (let[ obj-map    (->> obj clojure.reflect/reflect :members
                        (map #( (fn[x] ( into {}  x)) %)) vec )]
  obj-map))
(defn tablemap[obj]
  (let[ obj-map    (->> obj clojure.reflect/reflect :members
                        (map #( (fn[x] ( into {}  x)) %)) vec )
        tablemodel (seesaw.table/table-model
                    :columns members-c-n
                    :rows  obj-map  )
        table      (table :model tablemodel)
                        ]
 (-> {} (assoc :obj-map    obj-map)
        (assoc :tablemodel tablemodel)
        (assoc :table      table)
 )))
(defn tableframemap[obj]
   (let[ tablemap   (tablemap obj )
         cframe     (center-frame (:table tablemap)) ]
   (-> tablemap (assoc :frame cframe ))
 ))

;(run :dispose)

