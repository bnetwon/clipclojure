package bsta.main.util;



import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import bsta.main.thread.BufferClearThread;
import bsta.main.thread.WriteStdoutThread;



/**
 * Windowsコマンド Utilityクラス
 *
 * @author M.Oshiro
 */
public class ProcessCommandUtil {

    /**
     * コマンドの実行
     *
     * @param command 実行コマンドのリスト
     * @retrun 実行したコマンドの標準出力
     * @throws Exception
     */
    public static  Hashtable<Class<?>, Object> executeCommandPlain(List<String> command) throws Exception {

        Hashtable<Class<?>, Object> ht = new Hashtable<Class<?>, Object>();

        // コマンド実行
        try {
            ProcessBuilder pbNoLock = new ProcessBuilder();

            pbNoLock.redirectErrorStream(false);
            pbNoLock.command(command);

            Process processWatched = pbNoLock.start();

            ht.put(Process.class, processWatched);
            ht.put(ProcessBuilder.class, pbNoLock);

        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }

        return ht;
    }
    /**
     * コマンドの実行
     *
     * @param command 実行コマンドのリスト
     * @retrun 実行したコマンドの標準出力
     * @throws Exception
     */
    public static  Hashtable<Class<?>, Object> executeCommand(List<String> command) throws Exception {

        Hashtable<Class<?>, Object> ht = new Hashtable<Class<?>, Object>();
        // 変数初期化
//        int rtnCode = 0;
        List<String> stdoutList = null;

        // コマンド実行
        try {
            ProcessBuilder pbNoLock = new ProcessBuilder();

            pbNoLock.redirectErrorStream(false);
            pbNoLock.command(command);

            Process processWatched = pbNoLock.start();
            System.out.println("コマンド発行：" + getCommandString(command));

            // 標準入力用と標準エラー用の２スレッドを生成してバッファからデータの吸出し
            stdoutList = new ArrayList<String>();
            WriteStdoutThread stdin = new WriteStdoutThread(processWatched.getInputStream(), stdoutList);
            BufferClearThread error = new BufferClearThread(processWatched.getErrorStream());
            Thread stdinThread = new Thread(stdin);
            Thread errorThread = new Thread(error);
            stdinThread.start();
            errorThread.start();
//            DataInputStream dis = new DataInputStream(System.in);
//            String readline = dis.readLine();
//            while(readline.length() > 5){
//                processWatched.getOutputStream().write(readline.getBytes());
//                readline = dis.readLine();
//            }

//            rtnCode = processWatched.waitFor();
////            stdinThread.join();
////            errorThread.join();
//            if (rtnCode != 0) {
////                System.out.println("222222:"+rtnCode);
//                System.out.println(error.getBufferData());
////                throw new Exception("外部プロセスの戻り値：" + rtnCode);
//            }
//            System.out.println(stdoutList);
            ht.put(Process.class, processWatched);
            ht.put(ProcessBuilder.class, pbNoLock);
            ht.put(List.class, stdoutList);
            ht.put(WriteStdoutThread.class, stdin);
            ht.put(BufferClearThread.class, error);
        } catch (Exception e) {
            e.printStackTrace();
//            logger.error("コマンド失敗：" + getCommandString(command));
            throw e;
        }

        return ht;
    }

    /**
     * コマンドの実行
     *
     * @param command 実行コマンドのリスト
     * @retrun 実行したコマンドの標準出力
     * @throws Exception
     */
    public static List<String> executeCommandWithWait(List<String> command) throws Exception {

        // 変数初期化
        int rtnCode = 0;
        List<String> stdoutList = null;

        // コマンド実行
        try {
            ProcessBuilder pbNoLock = new ProcessBuilder();
            pbNoLock.redirectErrorStream(false);
            pbNoLock.command(command);

            Process processWatched = pbNoLock.start();
            System.out.println("コマンド発行：" + getCommandString(command));

            // 標準入力用と標準エラー用の２スレッドを生成してバッファからデータの吸出し
            stdoutList = new ArrayList<String>();
            WriteStdoutThread stdin = new WriteStdoutThread(processWatched.getInputStream(), stdoutList);
            BufferClearThread error = new BufferClearThread(processWatched.getErrorStream());
            Thread stdinThread = new Thread(stdin);
            Thread errorThread = new Thread(error);
            stdinThread.start();
            errorThread.start();
//            DataInputStream dis = new DataInputStream(System.in);
//            String readline = dis.readLine();
//            while(readline.length() > 5){
//                processWatched.getOutputStream().write(readline.getBytes());
//                readline = dis.readLine();
//            }

            rtnCode = processWatched.waitFor();
//            stdinThread.join();
//            errorThread.join();
            if (rtnCode != 0) {
//                System.out.println("222222:"+rtnCode);
                System.out.println(error.getBufferData());
//                throw new Exception("外部プロセスの戻り値：" + rtnCode);
            }
            System.out.println(stdoutList);
        } catch (Exception e) {
            e.printStackTrace();
//            logger.error("コマンド失敗：" + getCommandString(command));
            throw e;
        }

        return stdoutList;
    }
/*
    *//**
     * WAON情報ファイルコピー実行
     *
     * @param fmDir コピー元ディレクトリ
     * @param toDir コピー先ディレクトリ
     * @throws Exception
     *//*
    public static void executeCommandSalepromCopy(String fmDir, String toDir) throws Exception {

        // コマンド生成
        List<String> command = new ArrayList<String>();
        command.add("XCOPY");
        command.add(WaonConstants.DOUBLE_QUOTATION + fmDir + WaonConstants.DOUBLE_QUOTATION);
        command.add(WaonConstants.DOUBLE_QUOTATION + toDir + WaonConstants.DOUBLE_QUOTATION);
        command.add("/E");
        command.add("/Y");
        command.add("/R");

        // コマンド実行
        executeCommand(command);

        return;
    }

    *//**
     * WAON情報ファイルコピー確認実行
     *
     * @param confirmDir 確認するディレクトリ
     * @param fileName 確認するファイル名
     * @retrun 実行したコマンドの標準出力
     *         確認コマンド失敗(戻り値が0以外)の場合はnull
     *//*
    public static List<String> executeCommandSalepromCopyConfirm(String confirmDir, String fileName) {

        // コマンド生成
        List<String> command = new ArrayList<String>();
        command.add("CMD");
        command.add("/C");
        command.add("DIR");
        command.add(WaonConstants.DOUBLE_QUOTATION + confirmDir + WaonConstants.DOUBLE_QUOTATION);
        command.add("|");
        command.add("FINDSTR");
        command.add(fileName);

        // コマンド実行
        //   （失敗の場合はnullを返却）
        List<String> rtnStdout = null;
        try {
            rtnStdout = executeCommand(command);
        } catch (Exception e) {
        }

        return rtnStdout;
    }

    *//**
     * AT・タスク登録コマンド実行
     *
     * @param ipAddress タスク登録先IPアドレス
     * @param taskStartDate タスク開始日
     * @param taskStartTime タスク開始時刻
     * @param startingBatch 起動バッチ名
     * @param SendKbn 配信区分
     * @param lastDirName 格納フォルダ名
     * @throws Exception
     *//*
    public static void executeCommandAtRegist(
            String ipAddress, Date taskStartDate, String taskStartTime, String startingBatch, String SendKbn, String lastDirName) throws Exception {

        List<String> command = new ArrayList<String>();

        if (SendKbn.equals(WaonConstants.SEND_NORMAL)) {
            // コマンド生成
            //Windows2012に対応するためPsExecツールを使用しATコマンドを実行する
            // 例）C:\\Program Files\\PSTools\\PsExec \\\\160.243.164.131 AT 13:38 /NEXT:27 "D:\\Temp\\ChgNPointSt\\201701271600_ChgNPointSt.bat"
            command.add(WaonConstants.DOUBLE_QUOTATION + "C:/Program Files/PsExec/psexec" + WaonConstants.DOUBLE_QUOTATION);
            command.add("\\\\\\\\" + ipAddress);
            command.add("AT");
            command.add(BaseDateTimeUtil.getFormatTimeHour0Trim(taskStartTime.substring(0, 2) + WaonConstants.DELIMITER_TIME + taskStartTime.substring(2)));
            command.add("/NEXT:" + BaseDateTimeUtil.getFormatDayofMonth0Trim(taskStartDate));
            command.add(WaonConstants.DOUBLE_QUOTATION + startingBatch.replace(WaonConstants.dollar_sign, WaonConstants.DELIMITER_TIME).substring(1) + WaonConstants.DOUBLE_QUOTATION);
        } else {
            // コマンド生成
            command.add("schtasks");
            command.add("/create");
            command.add("/S");
            command.add("\\\\\\\\" + ipAddress);
            command.add("/U");
            command.add("mentuser");
            command.add("/P");
            command.add("Password123");
            command.add("/SC");
            command.add("ONCE");
            command.add("/TN");
            command.add(WaonConstants.DOUBLE_QUOTATION + "Waon" + lastDirName + BaseDateTimeUtil.getFormatDayofMonth0Trim(taskStartDate) + taskStartTime.substring(0, 2) + taskStartTime.substring(2) + WaonConstants.DOUBLE_QUOTATION);
            command.add("/TR");
            command.add(WaonConstants.DOUBLE_QUOTATION + startingBatch.replace(WaonConstants.dollar_sign, WaonConstants.DELIMITER_TIME).substring(1) + WaonConstants.DOUBLE_QUOTATION);
            command.add("/ST");
            command.add(BaseDateTimeUtil.getFormatTimeHour0Trim(taskStartTime.substring(0, 2) + WaonConstants.DELIMITER_TIME + taskStartTime.substring(2)) + ":00");
            command.add("/SD");
            command.add(BaseDateTimeUtil.formatDateString2(taskStartDate));
            command.add("/F");
            command.add("/RL");
            command.add("HIGHEST");
            command.add("/NP");
        }

        // コマンド実行
        executeCommand(command);

        return;
    }

    *//**
     * AT・タスク確認コマンド実行
     *
     * @param ipAddress ATコマンドの引数にするサーバIPアドレス
     * @param execDay 実行日
     * @param execTime 実行時刻
     * @param startingBatch 起動バッチ名
     * @param SendKbn 配信区分
     * @param lastDirName 格納フォルダ名
     * @retrun 実行したコマンドの標準出力
     *         確認コマンド失敗(戻り値が0以外)の場合はnull
     *//*
    public static List<String> executeCommandAtConfirm(String ipAddress, Date execDay, String execTime, String startingBatch, String SendKbn, String lastDirName) {

        List<String> command = new ArrayList<String>();

        if (SendKbn.equals(WaonConstants.SEND_NORMAL)) {
            // コマンド生成
            //  ※MOREコマンドを入れているのは、ATコマンドの戻りの一部の改行コードが LF のため、
            //    CRLFに置き換えるため。（強制的にCRLFになる。）
            //Windows2012に対応するためPsExecツールを使用しATコマンドを実行する
            // 例）C:\\Program Files\\PSTools\\PsExec \\\\160.243.164.131 AT | MORE | FINDSTR C
            command.add(WaonConstants.DOUBLE_QUOTATION + "C:/Program Files/PsExec/psexec" + WaonConstants.DOUBLE_QUOTATION);
            command.add("\\\\\\\\" + ipAddress);
            command.add("CMD");
            command.add("/C");
            command.add("AT");
            command.add("|");
            command.add("MORE");
            command.add("|");
            command.add("FINDSTR");
            command.add(startingBatch.replace(WaonConstants.dollar_sign, WaonConstants.DELIMITER_TIME).substring(1));
            command.add("|");
            command.add("FINDSTR");
            command.add("/C:次 " + BaseDateTimeUtil.getFormatDayofMonth0Trim(execDay) + " ");
            command.add("|");
            command.add("FINDSTR");
            command.add("/C:" + " " + BaseDateTimeUtil.getFormatTimeHour0Trim(execTime.substring(0, 2) + WaonConstants.DELIMITER_TIME + execTime.substring(2)));
        } else {
            // コマンド生成
            command.add("CMD");
            command.add("/C");
            command.add("schtasks");
            command.add("/query");
            command.add("/S");
            command.add("\\\\\\\\" + ipAddress);
            command.add("/U");
            command.add("mentuser");
            command.add("/P");
            command.add("Password123");
            command.add("|");
            command.add("MORE");
            command.add("|");
            command.add("FINDSTR");
            command.add("Waon" + lastDirName + BaseDateTimeUtil.getFormatDayofMonth0Trim(execDay) + execTime.substring(0, 2) + execTime.substring(2));
        }

        // コマンド実行
        //   （失敗の場合はnullを返却）
        List<String> rtnStdout = null;
        try {
            rtnStdout = executeCommand(command);
        } catch (Exception e) {
        }

        return rtnStdout;
    }

    *//**
     * HULFT配信コマンド実行
     *
     * @param hulftId  HULFT_ID
     * @throws Exception
     *//*
    public static void executeCommandHulftSend(String hulftId) throws Exception {

        // コマンド生成
        List<String> command = new ArrayList<String>();
        command.add(WaonConstants.DOUBLE_QUOTATION +
                    "C:/HULFT Family/hulft7/binnt/utlsend" +
                    WaonConstants.DOUBLE_QUOTATION);
        command.add("-f");
        command.add(hulftId);
        command.add("-sync");

        // コマンド実行
        executeCommand(command);

        return;
    }
*/
    /**
     * List形式コマンドからString形式コマンドの取得
     *
     * @param list コマンド文字列リスト
     * @return コマンドライン
     */
    private static String getCommandString(List<String> command) {

        StringBuilder commandLine = new StringBuilder();
        for (String cmd : command) {
            commandLine.append(cmd).append(" ");
        }

        return commandLine.toString().trim();
    }
    public static void main(String[] args) {
        ArrayList<String> arr = new ArrayList<String>();
        for (String string : args) {
            arr.add(string);

        }

//        arr.add("java");
//        arr.add("CMD ");
//        arr.add("/C");
//        arr.add("c:\\bin\\psexec");
//        arr.add("/s");
//        arr.add(" \\\\192.168.18.215 ");
//        arr.add("cmd /c dir");
//        arr.add("/c");
//        arr.add("dir");
//        arr.add("c:\\bin\\psexec /s \\\\192.168.18.215  cmd /c dir ");
//        arr.add("c:\\bin\\psexec /s \\\\192.168.18.215  cmd /c dir ");
//        arr.add("c:\\bin\\psexec -i -u administraor  \\\\192.168.18.215  ");
//        arr.add("CMD ");
//        arr.add("/C");
//        arr.add("dir");
//        arr.add("c:\\bin\\psexec \\\\192.168.18.215  ");
//        arr.add("java");
//        arr.add("&& ");
//        arr.add("exit ");
//        arr.add(" \"c:\\bin\\psexec \\\\10.18.40.21 -i -u ej -p ej AT 16:00 /NEXT:13 D:\\temp\\ChgNPoint\\201804131600_ChgNPoint.bat\"");
//        arr.add("CMD /C \"c:\\bin\\psexec \\\\10.18.40.21 -i -u ej -p ej AT 16:00 /NEXT:13 D:\\temp\\ChgNPoint\\201804131600_ChgNPoint
        try {

            final Hashtable<Class<?>, Object>  ct = executeCommand(arr);
            new Thread(new Runnable() {

                @Override
                public void run() {

                    long start = System.currentTimeMillis();
                    @SuppressWarnings("unchecked")
                    List<String> clist = (List<String>) ct.get(List.class);
                    Hashtable<Class<?>, Object>  at = ct;

                    while(clist.size() < 2 && System.currentTimeMillis() - start < 10000){

                        if(clist.size()>0){

                        System.out.println(clist.get(0));
                        System.out.println(clist.get(clist.size()-1));
                        }
                        try {
                            Thread.sleep(1000);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                    System.out.println(clist.get(0));
                    System.out.println("end");
                    ((Process)at.get(Process.class)).destroy();
//                    ((ProcessBuilder)at.get(ProcessBuilder.class));
                }
            }).start();
        } catch (Exception e) {
            // TODO 自動生成された catch ブロック
            e.printStackTrace();
        }


    }
}
