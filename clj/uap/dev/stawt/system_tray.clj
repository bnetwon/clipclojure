(ns stawt.system_tray 
 (:import [java.awt.AWTException   ]  
 [java.awt.Image                   ]
 [java.awt.MenuItem                ]
 [java.awt.PopupMenu               ]
 [java.awt.SystemTray              ]
 [java.awt.TrayIcon                ]
 [java.awt.event.ActionEvent       ]
 [java.awt.event.ActionListener    ]
 [java.awt.image.BufferedImage     ]
 [java.io.File                     ]
 [java.io.IOException              ]
 [java.util.ArrayList              ]
 [javax.imageio.ImageIO;           ]
 ))

    (declare  ^SystemTray tray )
    (declare  ^TrayIcon trayIcon)
    (declare  ^PopupMenu popup )
    (declare  ^atom  exit_act )

    (defn init
              (if (SystemTray/isSupported)
     (do ;get the SystemTray instance
        (def tray  SystemTray/getSystemTray)
        (def exit_act ( atom (reify ActionListener (actionPerformed [this e](prn e) ) ))
        ;// load an image
        (let [ rimage ( ImageIO/read(new File("./resource/TrayIcon.png") ))
               tk     (Toolkit/getDefaultToolkit) 
               act    (reify ActionListener (actionPerformed [this e](prn e) ))  ]
               ())
        BufferedImage rimage = null;
        try {
            rimage = ImageIO.read(new File("./resource/TrayIcon.png"));
        } catch (IOException e1) {
            e1.printStackTrace();
        }
        Image image = Scalr.resize(rimage, Method.QUALITY,
                16, 16);

//        Toolkit tk = Toolkit.getDefaultToolkit();
        // create a action listener to listen for default action executed on the tray icon
        ActionListener listener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                // execute default action of the application
                // ...
                System.exit(0);
            }
        };
        // create a popup menu
        popup = new PopupMenu();
        // create menu item for the default action
        MenuItem defaultItem = new MenuItem("EXIT");
        defaultItem.addActionListener(listener);
        popup.add(defaultItem);
        /// ... add other items
        // construct a TrayIcon
        trayIcon = new TrayIcon(image, "LeinRepl", popup);
        // set the TrayIcon properties
        trayIcon.addActionListener(listener);
        // ...
        // add the tray image
        try {
            tray.add(trayIcon);
        } catch (AWTException e) {
            System.err.println(e);
        }
        // ...
    } else {
        // disable tray option in your application or
        // perform other actions
//        ...
    }
    // ...
    // some time later
    // the application state has changed - update the image
    if (trayIcon != null) {
//        Image image = Toolkit.getDefaultToolkit().getImage("/resource/TrayIcon.jpg");
//        trayIcon.setImage(image);
//        Toolkit.getDefaultToolkit().getImage("../../resource/TrayIcon.jpg");
    }
    // ...

  }

    public void startServer(){
        String[] args = "lein.bat repl :headless :port 53935".split(" ");
        ArrayList<String> arr = new ArrayList<String>();
        for (String string : args) {
            arr.add(string);

        }
//        try {
//            ProcessCommandUtil.executeCommand(arr);
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
        MenuItem serverItem = new MenuItem("Started Port : " + "53935");
//        defaultItem.addActionListener(listener);
        popup.add(serverItem);
    }

    public static void main(String[] args) {
        SystemTrayApp sta = new SystemTrayApp();
        sta.init();
    }
}
