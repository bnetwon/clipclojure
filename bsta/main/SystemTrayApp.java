package bsta.main;
import java.awt.AWTException;
import java.awt.Image;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.imageio.ImageIO;

import org.imgscalr.Scalr;
import org.imgscalr.Scalr.Method;


public class SystemTrayApp {
    SystemTray tray = null;
    TrayIcon trayIcon = null;
    PopupMenu popup = null;

    public void init(){

    if (SystemTray.isSupported()) {
        // get the SystemTray instance
        tray = SystemTray.getSystemTray();
        // load an image
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
