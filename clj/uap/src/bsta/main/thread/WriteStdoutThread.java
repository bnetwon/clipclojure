package bsta.main.thread;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;


/**
 * 外部プロセスのバッファデータ吸出しクラス
 *
 * @author W
 */
public class WriteStdoutThread implements Runnable {

    /**
     * 入力ストリーム
     */
    private InputStream inputStream;

    /**
     * 標準出力のリスト
     */
    private List<String> stdoutList;

    /**
     * コンストラクタ
     */
    public WriteStdoutThread(InputStream inputStream, List<String> stdoutList) {
        this.inputStream = inputStream;
        this.stdoutList = stdoutList;
    }

    /**
     * スレッドの主処理
     */
    public void run() {

        BufferedReader br = null;
        try {
            br = new BufferedReader(
                     new InputStreamReader(
                         inputStream,
                         "SJIS"));
            while (true) {
                String line = br.readLine();
                if (line == null) {
                    break;
                }
//                System.out.println("ws :"+line);
                stdoutList.add(line);
            }
        } catch (Exception e) {
            System.err.println("例外発生：" + e);
        } finally {
            try {
                if (br != null) {
                    br.close();
                }
            } catch (IOException e) {
                System.err.println("例外発生：" + e);
            }
        }
    }
}
