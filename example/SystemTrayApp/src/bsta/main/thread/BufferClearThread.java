package bsta.main.thread;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

/**
 * 外部プロセスのバッファデータ吸出しクラス
 *
 * @author W
 */
public class BufferClearThread implements Runnable {

    /**
     * 読み込み
     */
    private final BufferedReader br;

    /**
     * バッファのデータ
     */
    private final ArrayList<String> bufferData = new ArrayList<String>();

    /**
     * コンストラクタ
     */
    public BufferClearThread(InputStream is) {
        br = new BufferedReader(new InputStreamReader(is));
    }

    /**
     * スレッドの主処理
     */
    public void run() {

        try {
            while (true) {
                String line = br.readLine();
                if (line == null) {
                    break;
                }
                System.out.println("bb :"+line);
                bufferData.add(line);
            }
        } catch (IOException e) {
            System.err.println("例外発生：" + e);
        } finally {
            try {
                br.close();
            } catch (IOException e) {
                System.err.println("例外発生：" + e);
            }
        }
    }

    /**
     * 外部プロセスのバッファからのデータ吸出し
     *
     * @return バッファデータ
     */
    public String getBufferDataString() {

        StringBuilder sb = new StringBuilder();
        for (String data : bufferData) {
            sb.append(data);
        }

        return sb.toString();
    }

    public ArrayList<String> getBufferData(){
        return bufferData;
    }
}
