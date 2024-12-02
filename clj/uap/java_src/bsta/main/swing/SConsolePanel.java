// -*- mode:java; encoding:utf-8 -*-
// vim:set fileencoding=utf-8:
// https://ateraimemo.com/Swing/TextAreaOutputStream.html

package bsta.main.swing;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.WindowConstants;

@SuppressWarnings("serial")
public class SConsolePanel extends JPanel {
    JTextArea textArea = new JTextArea();
    JTextField textField = new JTextField("");
    JButton enterbtn = new JButton("Enter");

    public JTextArea getTextArea() {
        return textArea;
    }

    public void setTextArea(JTextArea textArea) {
        this.textArea = textArea;
    }

    public JTextField getTextField() {
        return textField;
    }

    public void setTextField(JTextField textField) {
        this.textField = textField;
    }

    public JButton getEnterbtn() {
        return enterbtn;
    }

    public void setEnterbtn(JButton enterbtn) {
        this.enterbtn = enterbtn;
    }

  public SConsolePanel() {
    super(new BorderLayout());

    // TEST: textArea.getDocument().addDocumentListener(new FIFODocumentListener(textArea));
    textArea.setEditable(false);


//    OutputStream os = new TextAreaOutputStream(textArea);

    // // TEST:
    // try {
    //   // System.setOut(new PrintStream(os, true, "UTF-8"));
    //   FileHandler fh = new FileHandler("test.log");
    //   fh.setEncoding("UTF-8");
    //   LOGGER.addHandler(fh);
    // } catch (IOException ex) {
    //   ex.printStackTrace();
    // }

    JButton button = new JButton("Clear");
    button.addActionListener(new ActionListener() {

        @Override
        public void actionPerformed(ActionEvent arg0) {
            // TODO 自動生成されたメソッド・スタブ
            textArea.setText("");
        }
    });


    Box box = Box.createHorizontalBox();
    box.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    box.add(Box.createHorizontalGlue());
    box.add(textField);
    box.add(Box.createHorizontalStrut(5));
    box.add(enterbtn);
    box.add(Box.createHorizontalStrut(5));
    box.add(button);

    add(new JScrollPane(textArea));
    add(box, BorderLayout.SOUTH);
    setPreferredSize(new Dimension(320, 240));
  }

  public static void main(String... args) {
    EventQueue.invokeLater(new Runnable() {
      @Override public void run() {
        createAndShowGui(new SConsolePanel());
      }
    });
  }

  public static JFrame createGui(SConsolePanel scp) {
      JFrame frame ;
      try {
          UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } catch (ClassNotFoundException e) {
          // TODO 自動生成された catch ブロック
          e.printStackTrace();
      } catch (InstantiationException e) {
          // TODO 自動生成された catch ブロック
          e.printStackTrace();
      } catch (IllegalAccessException e) {
          // TODO 自動生成された catch ブロック
          e.printStackTrace();
      } catch (UnsupportedLookAndFeelException e) {
          // TODO 自動生成された catch ブロック
          e.printStackTrace();
      }

      frame = new JFrame("TextAreaOutputStream");
      frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
      frame.getContentPane().add(scp);
      frame.pack();
      frame.setLocationRelativeTo(null);
      return frame;
  }

  public static JFrame createAndShowGui(SConsolePanel scp) {
      JFrame frame ;
      frame = createGui(scp);
      frame.setVisible(true);
    return frame;
  }
}


class TextAreaLogger {
}

/*@SuppressWarnings("serial")
class EnterAction extends AbstractAction {
  private static final Logger LOGGER = Logger.getLogger(TextAreaLogger.class.getName());
  private final JTextField textField;

  protected EnterAction(JTextField textField) {
    super("Enter");
    this.textField = textField;
  }

  @Override public void actionPerformed(ActionEvent e) {
//    LOGGER.info(String.format("%s%n  %s%n", Calendar.getInstance().toString(), textField.getText()));
  }
}*/

// class FIFODocumentListener implements DocumentListener {
//   private static final int MAX_LINES = 100;
//   private final JTextComponent textComponent;
//   protected FIFODocumentListener(JTextComponent textComponent) {
//     this.textComponent = textComponent;
//   }
//   @Override public void insertUpdate(DocumentEvent e) {
//     Document doc = e.getDocument();
//     Element root = doc.getDefaultRootElement();
//     if (root.getElementCount() <= MAX_LINES) {
//       return;
//     }
//     EventQueue.invokeLater(new Runnable() {
//       @Override public void run() {
//         removeLines(doc, root);
//       }
//     });
//     textComponent.setCaretPosition(doc.getLength());
//   }
//   private static void removeLines(Document doc, Element root) {
//     Element fl = root.getElement(0);
//     try {
//       doc.remove(0, fl.getEndOffset());
//     } catch (BadLocationException ex) {
//       ex.printStackTrace();
//     }
//   }
//   @Override public void removeUpdate(DocumentEvent e) {
//     /* not needed */
//   }
//   @Override public void changedUpdate(DocumentEvent e) {
//     /* not needed */
//   }
// }

// class TextAreaOutputStream extends OutputStream {
//   private final ByteArrayOutputStream buf = new ByteArrayOutputStream();
//   private final JTextArea textArea;
//   protected TextAreaOutputStream(JTextArea textArea) {
//     super();
//     this.textArea = textArea;
//   }
//   @Override public void flush() throws IOException {
//     super.flush();
//     buf.flush();
//   }
//   @Override public void close() throws IOException {
//     super.close();
//     buf.close();
//   }
//   @Override public void write(int b) throws IOException {
//     if (b == '\r') {
//       return;
//     }
//     if (b == '\n') {
//       String text = buf.toString("UTF-8");
//       buf.reset();
//       EventQueue.invokeLater(new Runnable() {
//         @Override public void run() {
//           textArea.append(text + '\n');
//           textArea.setCaretPosition(textArea.getDocument().getLength());
//         }
//       });
//       return;
//     }
//     buf.write(b);
//   }
// }

// // TEST:
// // http://www.dreamincode.net/forums/topic/117537-external-program-output-to-jtextarea/
// class TextAreaOutputStream extends OutputStream {
//   private final JTextArea textArea;
//   private final StringBuilder sb = new StringBuilder();
//
//   protected TextAreaOutputStream(JTextArea textArea) {
//     super();
//     this.textArea = textArea;
//   }
//
//   @Override public void flush() {}
//
//   @Override public void close() {}
//
//   @Override public void write(int b) throws IOException {
//     if (b == '\r') {
//      return;
//     }
//     if (b == '\n') {
//       textArea.append(sb.toString());
//       sb.setLength(0);
//     }
//     // sb.append((char) b);
//     String s;
//     if (Character.charCount(b) == 1) {
//       s = Objects.toString((char) b);
//     } else {
//       s = new String(Character.toChars(b));
//     }
//     sb.append(s);
//   }
// }

// // http://d.hatena.ne.jp/altcla/20091029/1256824750
// class TextAreaOutputStream extends OutputStream {
//   private JTextArea textArea;
//   private ByteArrayOutputStream buf;
//
//   protected TextAreaOutputStream(JTextArea area) {
//     textArea = area;
//     buf = new ByteArrayOutputStream();
//   }
//
//   @Override public void write(int i) throws IOException {
//     buf.write(i);
//   }
//   @Override public void flush() throws IOException {
//     EventQueue.invokeLater(new Runnable() {
//       @Override public void run() {
//         try {
//           textArea.append(buf.toString("UTF-8"));
//           textArea.setCaretPosition(textArea.getDocument().getLength());
//           buf.reset();
//         } catch (UnsupportedEncodingException ex) {}
//       }
//     });
//   }
// }

// class TextAreaOutputStream extends OutputStream {
//   private final JTextArea textArea;
//   protected TextAreaOutputStream(JTextArea textArea) {
//     super();
//     this.textArea = textArea;
//   }
//   @Override public void write(int i) throws IOException {
//     String s;
//     if (Character.charCount(i) == 1) {
//       s = Objects.toString((char) i);
//     } else {
//       s = new String(Character.toChars(i));
//     }
//     textArea.append(s);
//     // textArea.append(new String(Character.toChars(i)));
//     // textArea.setCaretPosition(textArea.getDocument().getLength());
//   }
// }

// class TextAreaOutputStream extends OutputStream {
//   private final JTextArea textArea;
//   protected TextAreaOutputStream(JTextArea textArea) {
//     super();
//     this.textArea = textArea;
//   }
//   @Override public void write(int i) throws IOException {
//     textArea.append(Objects.toString((char) i));
//     textArea.setCaretPosition(textArea.getDocument().getLength());
//   }
// }

// class TextAreaOutputStream extends OutputStream {
//   private final AbstractDocument doc;
//   protected TextAreaOutputStream(AbstractDocument doc) {
//     super();
//     this.doc = doc;
//   }
//   @Override public void write(int i) throws IOException {
//     try {
//       doc.replace(doc.getLength(), 0, new String(Character.toChars(i)), null);
//       // doc.createPosition(doc.getLength());
//     } catch (BadLocationException ex) {
//       throw new IOException(ex);
//     }
//   }
// }

// // https://tips4java.wordpress.com/2008/11/08/message-console/
// try {
//   PipedOutputStream pos = new PipedOutputStream();
//   PipedInputStream pis = new PipedInputStream(pos);
//   BufferedReader reader = new BufferedReader(new InputStreamReader(pis, "UTF-8"));
//   System.setOut(new PrintStream(pos, true, "UTF-8"));
//
//   new Thread(new Runnable() {
//     @Override public void run() {
//       String line = null;
//       try {
//         while ((line = reader.readLine()) != null) {
//           displayPane.append(line + "\n");
//           displayPane.setCaretPosition(displayPane.getDocument().getLength());
//         }
//       } catch (IOException ex) {
//         // JOptionPane.showMessageDialog(null, "Error redirecting output : " + ex.getMessage());
//       }
//     }
//   }).start();
// } catch (IOException ex) {}
