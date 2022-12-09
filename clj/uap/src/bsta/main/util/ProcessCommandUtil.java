package bsta.main.util;



import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import bsta.main.thread.BufferClearThread;
import bsta.main.thread.WriteStdoutThread;



/**
 * Windows�R�}���h Utility�N���X
 *
 * @author M.Oshiro
 */
public class ProcessCommandUtil {

    /**
     * �R�}���h�̎��s
     *
     * @param command ���s�R�}���h�̃��X�g
     * @retrun ���s�����R�}���h�̕W���o��
     * @throws Exception
     */
    public static  Hashtable<Class<?>, Object> executeCommandPlain(List<String> command) throws Exception {

        Hashtable<Class<?>, Object> ht = new Hashtable<Class<?>, Object>();

        // �R�}���h���s
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
     * �R�}���h�̎��s
     *
     * @param command ���s�R�}���h�̃��X�g
     * @retrun ���s�����R�}���h�̕W���o��
     * @throws Exception
     */
    public static  Hashtable<Class<?>, Object> executeCommand(List<String> command) throws Exception {

        Hashtable<Class<?>, Object> ht = new Hashtable<Class<?>, Object>();
        // �ϐ�������
//        int rtnCode = 0;
        List<String> stdoutList = null;

        // �R�}���h���s
        try {
            ProcessBuilder pbNoLock = new ProcessBuilder();

            pbNoLock.redirectErrorStream(false);
            pbNoLock.command(command);

            Process processWatched = pbNoLock.start();
            System.out.println("�R�}���h���s�F" + getCommandString(command));

            // �W�����͗p�ƕW���G���[�p�̂Q�X���b�h�𐶐����ăo�b�t�@����f�[�^�̋z�o��
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
////                throw new Exception("�O���v���Z�X�̖߂�l�F" + rtnCode);
//            }
//            System.out.println(stdoutList);
            ht.put(Process.class, processWatched);
            ht.put(ProcessBuilder.class, pbNoLock);
            ht.put(List.class, stdoutList);
            ht.put(WriteStdoutThread.class, stdin);
            ht.put(BufferClearThread.class, error);
        } catch (Exception e) {
            e.printStackTrace();
//            logger.error("�R�}���h���s�F" + getCommandString(command));
            throw e;
        }

        return ht;
    }

    /**
     * �R�}���h�̎��s
     *
     * @param command ���s�R�}���h�̃��X�g
     * @retrun ���s�����R�}���h�̕W���o��
     * @throws Exception
     */
    public static List<String> executeCommandWithWait(List<String> command) throws Exception {

        // �ϐ�������
        int rtnCode = 0;
        List<String> stdoutList = null;

        // �R�}���h���s
        try {
            ProcessBuilder pbNoLock = new ProcessBuilder();
            pbNoLock.redirectErrorStream(false);
            pbNoLock.command(command);

            Process processWatched = pbNoLock.start();
            System.out.println("�R�}���h���s�F" + getCommandString(command));

            // �W�����͗p�ƕW���G���[�p�̂Q�X���b�h�𐶐����ăo�b�t�@����f�[�^�̋z�o��
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
//                throw new Exception("�O���v���Z�X�̖߂�l�F" + rtnCode);
            }
            System.out.println(stdoutList);
        } catch (Exception e) {
            e.printStackTrace();
//            logger.error("�R�}���h���s�F" + getCommandString(command));
            throw e;
        }

        return stdoutList;
    }
/*
    *//**
     * WAON���t�@�C���R�s�[���s
     *
     * @param fmDir �R�s�[���f�B���N�g��
     * @param toDir �R�s�[��f�B���N�g��
     * @throws Exception
     *//*
    public static void executeCommandSalepromCopy(String fmDir, String toDir) throws Exception {

        // �R�}���h����
        List<String> command = new ArrayList<String>();
        command.add("XCOPY");
        command.add(WaonConstants.DOUBLE_QUOTATION + fmDir + WaonConstants.DOUBLE_QUOTATION);
        command.add(WaonConstants.DOUBLE_QUOTATION + toDir + WaonConstants.DOUBLE_QUOTATION);
        command.add("/E");
        command.add("/Y");
        command.add("/R");

        // �R�}���h���s
        executeCommand(command);

        return;
    }

    *//**
     * WAON���t�@�C���R�s�[�m�F���s
     *
     * @param confirmDir �m�F����f�B���N�g��
     * @param fileName �m�F����t�@�C����
     * @retrun ���s�����R�}���h�̕W���o��
     *         �m�F�R�}���h���s(�߂�l��0�ȊO)�̏ꍇ��null
     *//*
    public static List<String> executeCommandSalepromCopyConfirm(String confirmDir, String fileName) {

        // �R�}���h����
        List<String> command = new ArrayList<String>();
        command.add("CMD");
        command.add("/C");
        command.add("DIR");
        command.add(WaonConstants.DOUBLE_QUOTATION + confirmDir + WaonConstants.DOUBLE_QUOTATION);
        command.add("|");
        command.add("FINDSTR");
        command.add(fileName);

        // �R�}���h���s
        //   �i���s�̏ꍇ��null��ԋp�j
        List<String> rtnStdout = null;
        try {
            rtnStdout = executeCommand(command);
        } catch (Exception e) {
        }

        return rtnStdout;
    }

    *//**
     * AT�E�^�X�N�o�^�R�}���h���s
     *
     * @param ipAddress �^�X�N�o�^��IP�A�h���X
     * @param taskStartDate �^�X�N�J�n��
     * @param taskStartTime �^�X�N�J�n����
     * @param startingBatch �N���o�b�`��
     * @param SendKbn �z�M�敪
     * @param lastDirName �i�[�t�H���_��
     * @throws Exception
     *//*
    public static void executeCommandAtRegist(
            String ipAddress, Date taskStartDate, String taskStartTime, String startingBatch, String SendKbn, String lastDirName) throws Exception {

        List<String> command = new ArrayList<String>();

        if (SendKbn.equals(WaonConstants.SEND_NORMAL)) {
            // �R�}���h����
            //Windows2012�ɑΉ����邽��PsExec�c�[�����g�p��AT�R�}���h�����s����
            // ��jC:\\Program Files\\PSTools\\PsExec \\\\160.243.164.131 AT 13:38 /NEXT:27 "D:\\Temp\\ChgNPointSt\\201701271600_ChgNPointSt.bat"
            command.add(WaonConstants.DOUBLE_QUOTATION + "C:/Program Files/PsExec/psexec" + WaonConstants.DOUBLE_QUOTATION);
            command.add("\\\\\\\\" + ipAddress);
            command.add("AT");
            command.add(BaseDateTimeUtil.getFormatTimeHour0Trim(taskStartTime.substring(0, 2) + WaonConstants.DELIMITER_TIME + taskStartTime.substring(2)));
            command.add("/NEXT:" + BaseDateTimeUtil.getFormatDayofMonth0Trim(taskStartDate));
            command.add(WaonConstants.DOUBLE_QUOTATION + startingBatch.replace(WaonConstants.dollar_sign, WaonConstants.DELIMITER_TIME).substring(1) + WaonConstants.DOUBLE_QUOTATION);
        } else {
            // �R�}���h����
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

        // �R�}���h���s
        executeCommand(command);

        return;
    }

    *//**
     * AT�E�^�X�N�m�F�R�}���h���s
     *
     * @param ipAddress AT�R�}���h�̈����ɂ���T�[�oIP�A�h���X
     * @param execDay ���s��
     * @param execTime ���s����
     * @param startingBatch �N���o�b�`��
     * @param SendKbn �z�M�敪
     * @param lastDirName �i�[�t�H���_��
     * @retrun ���s�����R�}���h�̕W���o��
     *         �m�F�R�}���h���s(�߂�l��0�ȊO)�̏ꍇ��null
     *//*
    public static List<String> executeCommandAtConfirm(String ipAddress, Date execDay, String execTime, String startingBatch, String SendKbn, String lastDirName) {

        List<String> command = new ArrayList<String>();

        if (SendKbn.equals(WaonConstants.SEND_NORMAL)) {
            // �R�}���h����
            //  ��MORE�R�}���h�����Ă���̂́AAT�R�}���h�̖߂�̈ꕔ�̉��s�R�[�h�� LF �̂��߁A
            //    CRLF�ɒu�������邽�߁B�i�����I��CRLF�ɂȂ�B�j
            //Windows2012�ɑΉ����邽��PsExec�c�[�����g�p��AT�R�}���h�����s����
            // ��jC:\\Program Files\\PSTools\\PsExec \\\\160.243.164.131 AT | MORE | FINDSTR C
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
            command.add("/C:�� " + BaseDateTimeUtil.getFormatDayofMonth0Trim(execDay) + " ");
            command.add("|");
            command.add("FINDSTR");
            command.add("/C:" + " " + BaseDateTimeUtil.getFormatTimeHour0Trim(execTime.substring(0, 2) + WaonConstants.DELIMITER_TIME + execTime.substring(2)));
        } else {
            // �R�}���h����
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

        // �R�}���h���s
        //   �i���s�̏ꍇ��null��ԋp�j
        List<String> rtnStdout = null;
        try {
            rtnStdout = executeCommand(command);
        } catch (Exception e) {
        }

        return rtnStdout;
    }

    *//**
     * HULFT�z�M�R�}���h���s
     *
     * @param hulftId  HULFT_ID
     * @throws Exception
     *//*
    public static void executeCommandHulftSend(String hulftId) throws Exception {

        // �R�}���h����
        List<String> command = new ArrayList<String>();
        command.add(WaonConstants.DOUBLE_QUOTATION +
                    "C:/HULFT Family/hulft7/binnt/utlsend" +
                    WaonConstants.DOUBLE_QUOTATION);
        command.add("-f");
        command.add(hulftId);
        command.add("-sync");

        // �R�}���h���s
        executeCommand(command);

        return;
    }
*/
    /**
     * List�`���R�}���h����String�`���R�}���h�̎擾
     *
     * @param list �R�}���h�����񃊃X�g
     * @return �R�}���h���C��
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
            // TODO �����������ꂽ catch �u���b�N
            e.printStackTrace();
        }


    }
}
