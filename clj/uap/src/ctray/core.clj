(ns ctray.core
 (:require [ctray.state :refer [*runtime-state]] [ctray.cmd])
 (:import [java.awt AWTException]
          [java.awt Image]
          [java.awt MenuItem]
          [java.awt PopupMenu]
          [java.awt SystemTray]
          [java.awt TrayIcon]
          [java.awt.event ActionListener]
          [java.io File]
          [javax.imageio ImageIO]
          [java.util.prefs Preferences] )
    (:gen-class))

(defn tray-supported?
  []
  (. SystemTray isSupported))

(defn str->img [str](ImageIO/read (new File str)))

(defn make-tray ([]( do 
                 (let [tray (@*runtime-state :tray) ](if (nil?  (@*runtime-state :tray ) )
                                                           (swap! *runtime-state assoc :tray (SystemTray/getSystemTray))) ) ; if nil
                 (@*runtime-state :tray)
                   ))
                ([tray-icon](doto (@*runtime-state :tray) (.add tray-icon) )))


(defn make-tray-menu ([](make-tray-menu (str->img  "./resources/TrayIcon.png")))
                     ([image](let [
                                   popup-menu ( new PopupMenu)
                                   tray-icon ( new TrayIcon image  "trayIcon" popup-menu)
                                    ]
                                        (do (swap! *runtime-state assoc :popup-menu popup-menu)
                                            (swap! *runtime-state assoc :tray-icon  tray-icon )
                                              popup-menu
                                         )
                       )))
(defn make-menu-item ([str] ( new MenuItem str )
                     ))

(def exit-menu-item  (doto ( new MenuItem "EXIT" )(.addActionListener (reify ActionListener(actionPerformed [this e](do (prn e) (System/exit 0) ) ) ))
                     ))
(def dispose-menu-item  (doto ( new MenuItem "Dispose" )(.addActionListener (reify ActionListener(actionPerformed [this e](do (prn e) (. (@*runtime-state :tray) remove (@*runtime-state :tray-icon)) ) ) ))
                     ))

(defn cmd-menu-item ([] (cmd-menu-item {:title "cmd"}))
  ([maps] (if (nil? (@*runtime-state :cmd-menu))
            (do (swap! *runtime-state assoc :cmd-map (ctray.cmd/cmd-frame-atom-map))
                (swap! *runtime-state assoc :cmd ((@*runtime-state :cmd-map) :frame))
                (swap! *runtime-state assoc :cmd-menu (doto (new MenuItem (maps :title))
                                                        (.addActionListener (reify ActionListener (actionPerformed [this e]
                                                                                                    (do (prn e) (. (@*runtime-state :cmd) setVisible true)))))))))))
(defn add-cmd [] (.add (@*runtime-state :popup-menu) (@*runtime-state :cmd-menu)))
(defn init[] ( do
                 (swap! *runtime-state assoc
                      :prefs (.node (Preferences/userRoot) "ctray"))
                 (let [tray          (make-tray)
                       default-item  exit-menu-item
                       popup-icon    (make-tray-menu)
                        ]
                              (do (.add popup-icon default-item)
                                  (.add tray  (@*runtime-state :tray-icon))
                              )
                   )
                ))

(defn initdev[] ( do
                 (swap! *runtime-state assoc
                      :prefs (.node (Preferences/userRoot) "ctray"))
                 (let [tray          (make-tray)
                       default-item  dispose-menu-item
                       popup-icon    (make-tray-menu)
                        ]
                              (do (.add popup-icon default-item)
                                  (.insert tray  (@*runtime-state :tray-icon) 2)
                              )
                   )
                ))

(defn initConsole[titlename](let [  titlestr (if keyword? (name titlename) titlename )
                                    titlekey (if keyword?  titlename (keyword titlename))
                                    menu_iem ( new MenuItem titlestr )
                                 ]
                             (prn :exit)
                            )
)
;(require 'ctray.cmd)

(defn main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
