package bsta.main;
import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Panel;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.imgscalr.Scalr;
import org.imgscalr.Scalr.Method;

@SuppressWarnings("serial")
public class ImageSample extends Frame {

    public ImageSample(){
        this.setSize(300, 200);
        addWindowListener(new WindowAdapter(){
           public void windowClosing(WindowEvent e){
               System.exit(0);
           }
        });
        MyPanel panel = new MyPanel();
        this.add(panel,BorderLayout.CENTER);
        this.setVisible(true);
    }

    public static void main(String[] args) {
        new ImageSample();
    }
}
@SuppressWarnings("serial")
class MyPanel extends Panel {
    Image image;

    public MyPanel(){
        try {
            BufferedImage rimage = ImageIO.read(new File("./resource/TrayIcon.jpg"));
            image = Scalr.resize(rimage, Method.QUALITY,
                    26, 26);
        } catch (IOException e) {
            e.printStackTrace();
        }
//        image = Toolkit.getDefaultToolkit().getImage("./resource/TrayIcon.jpg");
    }

    public void paint(Graphics g){
        if (image != null){
            g.drawImage(image, 0, 0, this);
        }
    }
}