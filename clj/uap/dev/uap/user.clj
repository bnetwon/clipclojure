(ns uap.user
  (:require [clojure.tools.namespace.repl :as tnr]
            [clojure.repl]
            ;[proto-repl.saved-values]
            [clojure.reflect :as cr]
            [clojure.pprint :as pp]
            [seesaw.core]
            [seesaw.table]
            )
  (:import [java.awt Toolkit] [java.awt.datatransfer Clipboard DataFlavor StringSelection])
  (:use [seesaw core table][clojure.repl])
  )
(require '[clojure.data.csv :as csv] '[clojure.java.io :as io])
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
                         )
)
; (->> (ins->map "a") ct-model  (table :id :table :model) center-frame show! )
(defn lastsample[](->> String
     cr/reflect
     :members (into '() ) pp/print-table))

(defn pptrn( [ks rows](do (prn "ks" ks) (prn "rows" rows)))
           ( [rows]   (pptrn (keys (first rows)) rows)))
(def create-table-model (seesaw.table/table-model
                 :columns [:name
                           {:key :age :text "Age" :class java.lang.Integer}]
                 :rows [ ["Jim" 65]
                         {:age 75 :name "Doris"} ]
                         )
)
(def sampletablemodel (seesaw.table/table-model
                 :columns [:name
                           {:key :age :text "Age" :class java.lang.Integer}]
                 :rows [ ["Jim" 65]
                         {:age 75 :name "Doris"} ]
                         )
)
(def sampletable (seesaw.core/table :model
                      [:columns [:age :height]
                       :rows    [{:age 13 :height 45}
                                 {:age 45 :height 13}]]
                 )
)

(defn center-frame [center]
  (frame :title "JTable Example" :width 500 :height 400 :content
    (border-panel
      :center (scrollable center)
      :south  (label :id :sel :text "Selection: "))))
(defn showsampletable[]
(let[tta (table :id :table :model sampletablemodel)
    cf (center-frame tta)]
    (cf))
)


(defn start
  []
  ; (println "I'm starting now")
  (println "Start completed"))

(defn reset []
  (tnr/refresh :after 'user/start))

(println "proto-repl-demo dev/user.clj loaded.")

(def ^Clipboard clip (.getSystemClipboard (Toolkit/getDefaultToolkit)))

(defn get-string []
  (when (.isDataFlavorAvailable clip DataFlavor/stringFlavor)
    (.getData clip DataFlavor/stringFlavor)))

(defn get-filelist []
  (when (.isDataFlavorAvailable clip DataFlavor/javaFileListFlavor)
    (.getData clip DataFlavor/javaFileListFlavor)))

(defn set-string [s]
  (let [ss (StringSelection. (print-str s))]
    (.setContents clip ss ss)))

(defn rrten[ r rs ] (if (re-find r rs) rs ))

(defn default-list-cell-renderer
  [render-fn]
  (if (instance? javax.swing.ListCellRenderer render-fn)
    render-fn
    (proxy [javax.swing.DefaultListCellRenderer] []
      (getListCellRendererComponent [component value index selected? focus?]
        (let [^javax.swing.DefaultListCellRenderer this this]
          (proxy-super getListCellRendererComponent component value index selected? focus?)
          (render-fn this { :this      this
                            :component component
                            :value     value
                            :index     index
                            :selected? selected?
                            :focus?    focus? })
          this)))))
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

(defn makesomelist []
  (do (def somelist
        (clojure.string/split (get-string) #"\n"))
    somelist))

  
(defn delcomma [str](clojure.string/replace str "," ""))
(defn delbkt [str](clojure.string/replace str "\\)" ""))

; (defn- ^javax.swing.table.DefaultTableModel proxy-table-model
;   [column-names column-key-map column-classes]
;   (let [full-values (atom [])]
;     (proxy [javax.swing.table.DefaultTableModel] [(object-array column-names) 0]
;       (isCellEditable [row col] false)
;       (setRowCount [^Integer rows]
;         ; trick to force proxy-super macro to see correct type to avoid reflection.
;         (swap! full-values (fn [v]
;                              (if (< rows (count v))
;                                (subvec v rows)
;                                (vec (concat v (take (- (count v) rows) (constantly nil)))))))
;         (let [^javax.swing.table.DefaultTableModel this this]
;           (proxy-super setRowCount rows)))
;       (addRow [^objects values]
;         (swap! full-values conj (last values))
;         ; TODO reflection - I can't get rid of the reflection here without crashes
;         ; It has something to do with Object[] vs. Vector overrides.
;         (proxy-super addRow values))
;       (insertRow [row ^objects values]
;         (swap! full-values insert-at row (last values))
;         ; TODO reflection - I can't get rid of the reflection here without crashes
;         ; It has something to do with Object[] vs. Vector overrides.
;         (proxy-super insertRow row values))
;       (removeRow [row]
;         (swap! full-values remove-at row)
;         (let [^javax.swing.table.DefaultTableModel this this]
;           (proxy-super removeRow row)))
;       ; TODO this stuff is an awful hack and now that I'm wiser, I should fix it.
;       (getValueAt [row col]
;         (if (= -1 row col)
;           column-key-map
;           (if (= -1 col)
;             (get @full-values row)
;             (let [^javax.swing.table.DefaultTableModel this this]
;               (proxy-super getValueAt row col)))))
;       (setValueAt [value row col]
;         (if (= -1 col)
;           (swap! full-values assoc row value)
;           (let [^javax.swing.table.DefaultTableModel this this]
;             (proxy-super setValueAt value row col))))
;       (getColumnClass [^Integer c]
;         (proxy-super getColumnClass c)
;         (nth column-classes c)))))
;         (defn -main [& args]
;           "メイン関数."
;           (println args))
;
