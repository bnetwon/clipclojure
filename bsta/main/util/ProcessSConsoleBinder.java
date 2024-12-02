package bsta.main.util;

import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;

import bsta.main.swing.SConsolePanel;

public class ProcessSConsoleBinder {
    SConsolePanel scp = null;
    Hashtable<Class<?>, Object> ht = null;

    OutputStream os = null;
    InputStream is = null;
    InputStream erris = null;

    boolean watchFlg = true;

    Runnable pWatcher = new Runnable() {
        @Override
        public void run() {
            BufferedReader br = null;
            try {
                br = new BufferedReader(
                        new InputStreamReader(
                                is,
                        "SJIS"));
                while (watchFlg) {
                    final String line = br.readLine();
                    if (line == null) {
                        break;
                    }
                    System.out.println(line);
                    if(scp!=null){

                        scp.getTextArea().append(line);
                        scp.getTextArea().append("\r\n");
                        scp.getTextArea().setCaretPosition(
                                scp.getTextArea().getDocument().getLength());

                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                try {
                    if (br != null) {
                        br.close();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    };

    Runnable eWatcher = new Runnable() {
        @Override
        public void run() {
            BufferedReader br = null;
            try {
                br = new BufferedReader(
                         new InputStreamReader(
                             erris,
                             "SJIS"));
                while (watchFlg) {
                    final String line = br.readLine();
                    if (line == null) {
                        break;
                    }
                    if(scp!=null){
                                scp.getTextArea().append(line);
                                scp.getTextArea().append("\r\n");
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                try {
                    if (br != null) {
                        br.close();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    };

    @SuppressWarnings("serial")
    Action enterAction = new AbstractAction(){
        @Override
        public void actionPerformed(ActionEvent arg0) {
            if(scp!=null && os!=null){
                try {
                    os.write(scp.getTextField().getText().getBytes("UTF-8"));
                    os.write("\r\n".getBytes("UTF-8"));
                    os.flush();
                    scp.getTextField().setText("");
                } catch (UnsupportedEncodingException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    };

    public ProcessSConsoleBinder() {    }

    public ProcessSConsoleBinder(SConsolePanel scp,
            Hashtable<Class<?>, Object> ht) {
        this.scp = scp;
        this.ht = ht;
    }

    public void cupling(SConsolePanel scp , Process processWatched){
        is = processWatched.getInputStream();
        erris = processWatched.getErrorStream();
        os = processWatched.getOutputStream();
        this.scp=scp;
        scp.getEnterbtn().addActionListener(enterAction);
        new Thread(pWatcher).start();
        new Thread(eWatcher).start();

//        try {
//            System.out.println(processWatched.waitFor());
//            System.out.println("aaa");
//        } catch (InterruptedException e) {
//            e.printStackTrace();
//        }
    }

//    ht.put(Process.class, processWatched);
//    ht.put(ProcessBuilder.class, pbNoLock);

    public static void main(String[] args) {
        ProcessSConsoleBinder pscb = new ProcessSConsoleBinder();
        SConsolePanel scp = new SConsolePanel();
        JFrame frame ;
        frame = SConsolePanel.createAndShowGui(scp);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        ArrayList<String> arr = new ArrayList<String>();
        for (String string : args) {
            arr.add(string);

        }
        try {
            Hashtable<Class<?>, Object>  ct = ProcessCommandUtil.executeCommandPlain(arr);
            pscb.cupling(scp, (Process) ct.get(Process.class));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
