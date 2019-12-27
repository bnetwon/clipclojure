import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;


public class RandomFileUtil {


    /*public static void readRAFileCb(String file) throws IOException{
        java.nio.ByteBuffer bb1 = java.nio.ByteBuffer.allocateDirect(200);
        CharBuffer cb = bb1.asCharBuffer();
        StringBuffer sb = new StringBuffer();
        RandomAccessFile rafo = new RandomAccessFile(file, "r");
        FileChannel fco = rafo.getChannel();
        while(fco.read(bb1) > 0){
//            bb1.flip();
            cb.flip();

            while(cb.position()<cb.limit()-2) {
                sb.append(cb.get());
            }
            System.out.println(sb.toString());
            sb.delete(0, sb.length());
            cb.rewind();
        }

        rafo.close();

    }*/

    public static void readRAFileSb(String file) throws IOException{
        java.nio.ByteBuffer bb1 = java.nio.ByteBuffer.allocateDirect(200);
        StringBuffer sb = new StringBuffer();
        RandomAccessFile rafo = new RandomAccessFile(file, "r");
        FileChannel fco = rafo.getChannel();
        while(fco.read(bb1) > 0){
            bb1.flip();

            while(bb1.position()<bb1.limit()-2) {
                sb.append((char)bb1.get());
            }
            System.out.println(sb.toString());
            sb.delete(0, sb.length());
            bb1.rewind();
        }

        rafo.close();

    }
    public static void readRAFile(String file) throws IOException{
        byte[] ba = new byte[200];
        java.nio.ByteBuffer bb1 = java.nio.ByteBuffer.allocateDirect(200);
        RandomAccessFile rafo = new RandomAccessFile(file, "r");
        FileChannel fco = rafo.getChannel();
        int remaining = 200;
        while(fco.read(bb1) > 0){
        bb1.flip();


        while(bb1.position()<bb1.limit()-1) {
            if(bb1.remaining()<200){
                remaining = bb1.remaining();
                bb1.get(ba,0,remaining);
            }else{
                bb1.get(ba);
            }
        }
        if(remaining<200){
            System.out.print(new String(ba,0,remaining,"UTF-8"));
        }else{
            System.out.print(new String(ba,"UTF-8"));
        }
        bb1.rewind();
       }

        rafo.close();

    }
	/**
	 * @param args
	 */
	public static void main(String[] args) {
	    try {
            readRAFileSb("C:\\TEMP\\test.txt");
        } catch (IOException e) {
            e.printStackTrace();
        }

	}

}
