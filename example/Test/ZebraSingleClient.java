import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;



public class ZebraSingleClient implements Serializable{


    private static final long   serialVersionUID = 1L;
    public  static String       gHost           = new String();
    private int                 DEBUG_SW        = 0;
    private int                 gZebraPort      = 6101;
    private int                 gTimeout0       = 2000;
    private int                 gTimeout1       = 1000;
    private int                 gTimeout2       = 3500;

    // ZebraPrinter・ステータス配列
    private String []           statArray1      = new String[12];
    private String []           statArray2      = new String[12];
    private String []           statArray3      = new String[2];
    private String []           statArray4      = new String[3];
    private float               power_voltage   = 0;
    private final float         min_voltage     = new Float("6.87");
    private Socket              gSocket;
    private InetSocketAddress   gEndpoint;
    private BufferedReader      gReader;
    private BufferedWriter      gWriter;



    /** 印刷枚数 */   // 20140610現在 ： セットなしで、１枚
    private int printNo;
    /** 印刷枚数セット */
    public void setPrintNo(int printNo) {
        if (printNo < 1) {  printNo = 1;  }
        this.printNo = printNo;
    }
    /** 印刷枚数取得 */
    public int getPrintNo() {
        return printNo;
    }

    /**
     * 用紙印刷クラスのコンストラクタ
     */
    public ZebraSingleClient () {
        printNo = 1;        // 印刷枚数
    }

    public int zebraSocketStart(String gHost) throws Exception  {
        DEBUG_SW=1;
        int flg_error = 0;
        //---------------------------------------------------------------------
        // Zebraプリンタへ接続
        //---------------------------------------------------------------------
        try {
            //**********Print Log**********
            gEndpoint =
                new InetSocketAddress(InetAddress.getByName(gHost),gZebraPort);
            gSocket = new Socket();
            if( DEBUG_SW == 1 ){
                System.out.println("***** connect to:"+gHost+" gTimeout0:"+gTimeout0);
            }
            gSocket.setSoTimeout(gTimeout0);
            gSocket.connect(gEndpoint, gTimeout0);
            gSocket.setSoTimeout(gTimeout1);
            gReader = new BufferedReader(
                    new InputStreamReader(gSocket.getInputStream(),"UTF8"));
            gWriter = new BufferedWriter(
                    new OutputStreamWriter(gSocket.getOutputStream(),"UTF8"));
        } catch (UnknownHostException e) {
            //エラー：ソケット接続エラー
            if( DEBUG_SW == 1 ){
                System.out.println("***** Zebra ソケット接続エラー 301");
            }
            flg_error = 301;
            return flg_error;
        } catch (IOException e) {
            //エラー：ソケット接続エラー
            if( DEBUG_SW == 1 ){
                System.out.println("***** Zebra ソケット接続エラー 302");
            }
            flg_error = 302;
            return flg_error;
        } catch (Exception e) {
            //エラー：ソケット接続エラー
            if( DEBUG_SW == 1 ){
                System.out.println("***** Zebraプリンタ ソケット接続エラー 303");
            }
            flg_error = 303;
            return flg_error;
        }
        if( DEBUG_SW == 1 ){
            System.out.println("***** Zebra ソケット接続 OK *****");
        }
        return 0;
    }


    public int zebra_close() throws Exception{
//        try {gSocket.setSoTimeout(0); } catch (SocketException e) {       }
//        try {gReader.close();         } catch (Exception e) {     }
//        try {gWriter.close();         } catch (Exception e) {     }
//        try {gSocket.close();         } catch (Exception e) {     }
         // -------------------------------------
        // プリンタ切断(Socket)
        // -------------------------------------
        int flg_error = 0;
        try {
            if( DEBUG_SW == 1 ){
                System.out.println("***** Zebra 切断(Socket) START *****");
            }
            gSocket.setSoTimeout(0);
            gReader.close();
            gWriter.close();
            gSocket.close();
        } catch (UnknownHostException e) {
            //エラー：プリンタ通信エラー（切断）
            if( DEBUG_SW == 1 ){
                System.out.println("***** Zebra ソケット接続エラー 381");
            }
            flg_error = 381;
            //**********Print Log**********
            gSocket.setSoTimeout(0);
            gSocket.close();
            return flg_error;
        } catch (IOException e) {
            //エラー：プリンタ通信エラー（切断）
            if( DEBUG_SW == 1 ){
                System.out.println("***** Zebra ソケット接続エラー 382");
            }
            flg_error = 382;
            //**********Print Log**********
            gSocket.setSoTimeout(0);
            gSocket.close();
            return flg_error;
        }
        //**********Print Log**********
        sleep(150);
        if( DEBUG_SW == 1 ){
            System.out.println("***** Zebra 切断(Socket) END *****");
        }

        return flg_error;
    }

    public int zebra_prt_check() throws Exception{
        int flg_error = 0;
        //---------------------------------------------------------------------
        // Zebra Printer プリント
        //---------------------------------------------------------------------
        try {
            if( DEBUG_SW == 1 ){
                System.out.println("*** Zebra プリント START *** ");
                System.out.println("*----------------------------------------------");
            }
            // ZebraPrinter・ステータス確認
            if( zebra_p_check() == 0 ){
                // ZebraPrinter・バッテリー電圧取得
                zebra_p_get_power_voltage();
            }else{
                return flg_error;
            }
            // --------------------
            // カバーオープンか？
            // --------------------
            if( "1".equals(statArray2[2])){
                if( DEBUG_SW == 1 ){
                    System.out.println("***** Zebra プリンタのカバーを閉じて下さい。");
                }
                // プリンタのカバーを閉じて下さい。
                flg_error = 1;

                return flg_error;
            }
            // --------------------
            // 用紙切れか？
            // --------------------
            if( "1".equals(statArray1[1])){
                if( DEBUG_SW == 1 ){
                    System.out.println("***** Zebra 用紙切れが発生しました。用紙を入れ替えてください。");
                }
                // 用紙切れが発生しました。用紙を入れ替えてください。
                flg_error = 513;
                //**********Print Log**********

                return flg_error;
            }
            // --------------------
            // 過剰高温か？
            // --------------------
            if( "1".equals(statArray1[11])){
                if( DEBUG_SW == 1 ){
                    System.out.println("***** Zebra プリンタのサーマルヘッド高温異常が発生しました。");
                }
                // プリンタのサーマルヘッド高温異常が発生しました。
                flg_error = 18;

                return flg_error;
            }
            // -----------------------------------------
            // バッテリー電圧
            // 6.87ボルト以下になったら、メッセージ出力
            // -----------------------------------------
            if( power_voltage < min_voltage ){
                if( DEBUG_SW == 1 ){
                    System.out.println("***** Zebra プリンタのバッテリーが少なくなっています。充電してください。"+power_voltage);
                }
                flg_error = 36;

                return flg_error;
            }

        } catch (IOException e) {
            //エラー：プリンタ通信エラー
            if( DEBUG_SW == 1 ){
                System.out.println("***** Zebra ソケット接続エラー 331");
            }
            flg_error = 331;

            return flg_error;
        }
        sleep(50);
        if( DEBUG_SW == 1 ){
            System.out.println("***** Zebra プリント END *****");
        }

        return 0;
    }

    public int zebraPrintString(String str) throws Exception{
        int flg_error = 0;
        try{
            gWriter.write(str);

        gWriter.flush();
        } catch (IOException e) {
//e.printStackTrace();
//          zebra_p_end();              // ZebraPrinter・終了処理
//          gWriter.flush();
//
//          //エラー：プリンタ通信エラー
//          if( DEBUG_SW == 1 ){
//              System.out.println("***** Zebra ソケット接続エラー 331");
//          }
//          flg_error = 331;

        }
        return flg_error;
    }
    public int zebra_printEd() throws Exception{
        int flg_error = 0;
        try{
//        zebra_p_init();               // ZebraPrinter・初期処理

//        zebra_p_kikan();          // (01)期間
//
//        zebra_p_sku_name();           // (02)漢字品名,品名変更フラグ
//
//        zebra_p_pointNumber();        // (03)ポイント数
//
//        zebra_p_jan();                // (04)JANコード
//        zebra_p_bar_code();           // (04)バーコード
//
//        zebra_p_sku();                // (05)商品コード

        zebra_p_end();              // ZebraPrinter・終了処理

        gWriter.flush();
        } catch (IOException e) {
e.printStackTrace();
            zebra_p_end();              // ZebraPrinter・終了処理
            gWriter.flush();

            //エラー：プリンタ通信エラー
            if( DEBUG_SW == 1 ){
                System.out.println("***** Zebra ソケット接続エラー 331");
            }
            flg_error = 331;

        }
        return flg_error;
    }
    public StringBuffer checkBuffer    = new StringBuffer();
    public StringBuffer checkHexBuffer = new StringBuffer();
    /**
     ***************************************************************************
     * Zebra・ステータス確認        zebra_p_check
     * @return  int
     * @throws Exception
     ***************************************************************************
     */
    public int zebra_p_check() throws Exception  {
        checkBuffer    = new StringBuffer();
        checkHexBuffer = new StringBuffer();

        if( DEBUG_SW == 1 ){
            System.out.println(
            "***** Zebra・ステータス確認:zebra_p_check() *****" );
        }
        int rec_cnt = 0;
        String s = "";
        String str1 = "";
        String str2 = "";
        String stat1 = "";
        String stat2 = "";
        String stat3 = "";
        String hex_str = "";
        int readbyte;
        // -----------------------------------
        // カバー状態チェック
        // 用紙切れチェック
        // 高温異常チェック
        // -----------------------------------
        try {
            gSocket.setSoTimeout(gTimeout2);
            // ---------------------------------------------------------------
            // ＜ZebraPrinter・ステータス返却＞
            // ストリング 1:<STX>aaa,b,c,dddd,eee,f,g,h,iii,j,k,l<ETX><CR><LF>
            //                   |   | | |    |   | | | |   | | |
            //                   |   | | |    |   | | | |   | | +- 温度範囲（1:過剰高温）
            //                   |   | | |    |   | | | |   | +--- 温度範囲（1:過剰低温）
            //                   |   | | |    |   | | | |   +--- 破損 RAM フラグ（1:設定データ紛失）
            //                   |   | | |    |   | | | +--- 未使用（常に 000）
            //                   |   | | |    |   | | +--- 部分フォーマットフラグ（1:部分フォーマット進行中）
            //                   |   | | |    |   | +--- 通信診断モードフラグ（1:診断モードがアクティブ）
            //                   |   | | |    |   +--- バッファ満杯フラグ（1:受信バッファが満杯）
            //                   |   | | |    +--- 受信バッファ内のフォーマット数
            //                   |   | | +--- ラベル長（ドット数の値）
            //                   |   | +---ポーズ・フラグ（1:ポーズがアクティブ）
            //                   |   +--- 用紙切れフラグ（1:用紙切れ）
            //                   +--- 通信インターフェイス設定
            //
            // ストリング 2:<STX>mmm,n,o,p,q,r,s,t,uuuuuuuu,v,www<ETX><CR><LF>
            //                   |   | | | | | | | |        | |
            //                   |   | | | | | | | |        | +--- メモリに保存されたグラフィック・イメージの数
            //                   |   | | | | | | | |        +--- 印刷中フォーマットフラグ（常に 1）
            //                   |   | | | | | | | +--- バッチの残りのラベル
            //                   |   | | | | | | +--- ラベル待機中フラグ（1 = ラベルが剥離モードで待機中）
            //                   |   | | | | | +--- 印字幅モード
            //                   |   | | | | +--- 印字モード（0:巻取,1:剥離,2:カッター,3:アプリケ-タ）
            //                   |   | | | +--- 熱転写モードフラグ（1 = 熱転写モード選択済み）
            //                   |   | | +--- リボン切れフラグ（1 = リボン切れ）
            //                   |   | +---ヘッド上フラグ（1 = ヘッドが上のポジション）
            //                   |   +--- 未使用
            //                   +--- 機能設定
            //
            // ストリング 3:<STX>xxxx,y<ETX><CR><LF>
            //                   |    |
            //                   |    +--- 0:静的RAMインストールなし,1:静的RAMインストールあり
            //                   +--- パスワード
            // ---------------------------------------------------------------
            gWriter.write( "~HS");
            gWriter.flush();
            //loop1 = 0;
            boolean isFirst = true;
            s    = "";
            str1 = "";
            str2 = "";
            while (true) {
                readbyte = gReader.read();
                if( readbyte == -1 ){break;}    // -1が返されたら終了
                if( isFirst ){
                    isFirst = false;
                    // CREATE TEST START
                    s = String.valueOf((char)readbyte);
                    hex_str = HexCode(s);

                    checkBuffer    .append(s);
                    checkHexBuffer .append(hex_str);
                    // CREATE TEST END
                }else{
                    s = String.valueOf((char)readbyte);
                    hex_str = HexCode(s);

                    checkBuffer    .append(s);
                    checkHexBuffer .append(hex_str);
                    if( "00".equals(hex_str)){s = "";}
                    if( "02".equals(hex_str)){s = "";}
                    if( "0a".equals(hex_str)){s = "";}
                    if( "0d".equals(hex_str)){s = "";}
                    if( "03".equals(hex_str)){
                        s = "";
                        rec_cnt++;
                        if( rec_cnt == 1 ){
                            stat1 = str1;
                            str1 = "";
                        }
                        if( rec_cnt == 2 ){
                            stat2 = str1;
                            str1 = "";
                        }
                        if( rec_cnt > 2 ){
                            stat3 = str1;
                            str1 = "";
                            break;
                        }
                    }
                    str2 = str1 + s;
                    str1 = str2;
                }
            }
            statArray1 = null;
            statArray2 = null;
            statArray3 = null;
            statArray1  = stat1.split(",");
            statArray2  = stat2.split(",");
            statArray3  = stat3.split(",");
            if( DEBUG_SW == 1 ){
                System.out.println( "＜ZebraPrinter ステータス返却＞ stat1:"+stat1+"|stat2:"+stat2+"|stat3:"+stat3 );
                System.out.println( "通信インターフェイス設定  :"+statArray1[0] );
                System.out.println( "用紙切れフラグ            :"+statArray1[1] );
                System.out.println( "ポーズ・フラグ            :"+statArray1[2] );
                System.out.println( "ラベル長                  :"+statArray1[3] );
                System.out.println( "受信バッファフォーマット数:"+statArray1[4] );
                System.out.println( "バッファ満杯フラグ        :"+statArray1[5] );
                System.out.println( "通信診断モードフラグ      :"+statArray1[6] );
                System.out.println( "部分フォーマットフラグ    :"+statArray1[7] );
                System.out.println( "未使用                    :"+statArray1[8] );
                System.out.println( "破損 RAM フラグ           :"+statArray1[9] );
                System.out.println( "過剰低温 フラグ           :"+statArray1[10] );
                System.out.println( "過剰高温 フラグ           :"+statArray1[11] );
                System.out.println( "-----------------------------------------------" );
                System.out.println( "機能設定                  :"+statArray2[0] );
                System.out.println( "未使用                    :"+statArray2[1] );
                System.out.println( "ヘッド上フラグ(Cover Open):"+statArray2[2] );
                System.out.println( "リボン切れフラグ          :"+statArray2[3] );
                System.out.println( "熱転写モードフラグ        :"+statArray2[4] );
                System.out.println( "印字モード                :"+statArray2[5] );
                System.out.println( "印字幅モード              :"+statArray2[6] );
                System.out.println( "ラベル待機中フラグ        :"+statArray2[7] );
                System.out.println( "バッチの残りのラベル      :"+statArray2[8] );
                System.out.println( "印刷中フォーマットフラグ  :"+statArray2[9] );
                System.out.println( "メモリ保存グラフィック数  :"+statArray2[10] );
                System.out.println( "-----------------------------------------------" );
                System.out.println( "パスワード                :"+statArray3[0] );
                System.out.println( "RAMインストール           :"+statArray3[1] );
                System.out.println( "-----------------------------------------------" );
            }
        } catch (Exception e) {
            e.printStackTrace();
            if( DEBUG_SW == 1 ){
                System.out.println(
                "***** Zebra・ステータス確認:Exception "+e.toString() );
            }
            gSocket.setSoTimeout(0);
            gReader.close();
            gWriter.close();
            gSocket.close();
            return 9;
        }
        return 0;
    }

    public StringBuffer batteryBuffer    = new StringBuffer();
    public StringBuffer batteryHexBuffer = new StringBuffer();
    /**
     ***************************************************************************
     * Zebra・バッテリー確認        zebra_p_battery
     * @return  int
     * @throws Exception
     ***************************************************************************
     */
    public int zebra_p_battery() throws Exception  {
        batteryBuffer    = new StringBuffer();
        batteryHexBuffer = new StringBuffer();
        if( DEBUG_SW == 1 ){
            System.out.println(
            "***** Zebra・バッテリー確認:zebra_p_battery() *****" );
        }
        int     rec_cnt = 0;
        String  s       = "";
        String  str1    = "";
        String  str2    = "";
        String  stat1   = "";
        String  hex_str = "";
        int     readbyte;
        // ---------------------------------------------------------------
        // ＜バッテリー状態・ステータス ~HB＞
        // <STX>bb.bb,hh.hh,bt<ETX><CR><LF>
        //      |     |     |
        //      |     |     +---バッテリ温度（摂氏）
        //      |     +--- 1/4 ボルトに最も近い現在のヘッド電圧
        //      +--- 1/4 ボルトに最も近い現在のバッテリ電圧
        // ---------------------------------------------------------------
        try {
            gSocket.setSoTimeout(gTimeout2);
            gWriter.write( "~HB");
            gWriter.flush();
            boolean isFirst = true;
            s    = "";
            str1 = "";
            str2 = "";
            while (true) {
                readbyte = gReader.read();
                if( readbyte == -1 ){break;}    // -1が返されたら終了
                if( isFirst ){
                    isFirst = false;
                    s = String.valueOf((char)readbyte);
                    hex_str = HexCode(s);
                    batteryBuffer    .append(s);
                    batteryHexBuffer .append(hex_str);
                }else{
                    s = String.valueOf((char)readbyte);
                    hex_str = HexCode(s);
                    batteryBuffer    .append(s);
                    batteryHexBuffer .append(hex_str);
                    if( "00".equals(hex_str)){s = "";}
                    if( "02".equals(hex_str)){s = "";}
                    if( "0a".equals(hex_str)){s = "";}
                    if( "0d".equals(hex_str)){s = "";}
                    if( "03".equals(hex_str)){
                        s = "";
                        rec_cnt++;
                        if( rec_cnt == 1 ){
                            stat1 = str1;
                            str1 = "";
                            break;
                        }
                    }
                    str2 = str1 + s;
                    str1 = str2;
                }
            }
            statArray4 = null;
            statArray4  = stat1.split(",");
            if( DEBUG_SW == 1 ){
                System.out.println( "-------＜Zebra バッテリーステータス＞----------" );
                System.out.println( stat1 );
                System.out.println( "現在のバッテリ電圧  :"+statArray4[0].trim() );
                System.out.println( "現在のヘッド電圧    :"+statArray4[1].trim() );
                System.out.println( "バッテリ温度（摂氏）:"+statArray4[2].trim() );
                System.out.println( "-----------------------------------------------" );
            }
        } catch (Exception e) {
            if( DEBUG_SW == 1 ){
                System.out.println( "***** Zebra・バッテリー確認:Exception "+e.toString() );
            }
            gSocket.setSoTimeout(0);
            gReader.close();
            gWriter.close();
            gSocket.close();
            return 9;
        }
        return 0;
    }

    public StringBuffer powerBuffer    = new StringBuffer();
    public StringBuffer powerHexBuffer = new StringBuffer();
    /**
     ***************************************************************************
     * Zebra・バッテリー電圧取得      zebra_p_get_power_voltage
     * @return  int
     * @throws Exception
     ***************************************************************************
     */
    public int zebra_p_get_power_voltage() throws Exception  {
        powerBuffer    = new StringBuffer();
        powerHexBuffer = new StringBuffer();
        if( DEBUG_SW == 1 ){
            System.out.println(
            "***** Zebra・バッテリー電圧取得:zebra_p_get_power_voltage() *****" );
        }
        int     chr_cnt     = 0;
        int     cnt22       = 0;
        int     start_sw    = 0;
        String  s           = "";
        String  str1        = "";
        String  str2        = "";
        String  hex_str     = "";
        int     readbyte;
        // ---------------------------------------------------------------
        // ＜バッテリーステータス取得＞
        // Exsample : ! U1 getvar "power.status"
        // Result   : "ok" or "low"
        // ＜バッテリー電圧取得＞
        // Exsample : ! U1 getvar "power.voltage"
        // Result   : "7.25"
        // ---------------------------------------------------------------
        try {
            gSocket.setSoTimeout(gTimeout2);
            gWriter.write( "! U1 getvar \"power.voltage\"\n");
            gWriter.flush();
            s    = "";
            str1 = "";
            str2 = "";
            boolean isFirst = true;
            while (true) {
                readbyte = gReader.read();
                if( readbyte == -1 ){break;}    // -1が返されたら終了
                s       = String.valueOf((char)readbyte);
                hex_str = HexCode(s);
                powerBuffer    .append(s       );
                powerHexBuffer .append(hex_str );
                if( isFirst ){
                    isFirst = false;
                }else{
                    s = String.valueOf((char)readbyte);
                    hex_str = HexCode(s);
                    if( "00".equals(hex_str)){s = "";}
                    if( "02".equals(hex_str)){s = "";}
                    if( "03".equals(hex_str)){s = "";}
                    if( "0a".equals(hex_str)){s = "";continue;}
                    if( "0d".equals(hex_str)){s = "";}
                    if( "22".equals(hex_str)){
                        s = "";
                        start_sw = 1;
                        cnt22++;
                        if( cnt22 > 1 ){
                            chr_cnt++;
                            str2 = str1 + s;
                            str1 = str2;
                            break;
                        }
                    }
                    if( start_sw == 1 ){
                        chr_cnt++;
                        str2 = str1 + s;
                        str1 = str2;
                    }
                }
            }
            power_voltage  = Float.valueOf(str1).floatValue();
            if( DEBUG_SW == 1 ){
                System.out.println( "------＜Zebra バッテリー電圧取得＞-------");
                System.out.println( "バッテリー電圧:"+power_voltage );
                System.out.println( "-----------------------------------------");
            }
        } catch (Exception e) {
            if( DEBUG_SW == 1 ){
                System.out.println(
                "***** Zebra・バッテリー電圧取得:Exception "+e.toString() );
            }
            gSocket.setSoTimeout(0);
            gReader.close();
            gWriter.close();
            gSocket.close();
            return 9;
        }
        return 0;
    }



    /**
     ***************************************************************************
     * Zebra・終了処理・印刷        zebra_p_end
     * @return  void
     * @throws Exception
     ***************************************************************************
     */
    public void zebra_p_end() throws Exception  {
        String paper_size = "1";
        if( DEBUG_SW == 1 ){
            System.out.println( "***** Zebra・終了処理・印刷:zebra_p_end() *****" );
        }
        // -----------------  B8サンプル  -----------------
        //^MMR                                                                                   ^FX　動作モード^FS
        //^PQ1,0,0,Y                                                                             ^FX　印刷枚数^FS
        //^XZ
        //^XA^SS,,,1432^XZ                                                                       ^FX　フィード長指定^FS
        // --------------------------------------------
        if( "1".equals(paper_size) ){           // ******** B8 ********
            gWriter.write("^MMR");          // 巻き取りモード
            // ---- 印刷枚数 --------------------------------------
            // ラベル枚数=1,ポーズラベル枚数=1,シリアル番号複写枚数=1,ポーズ間隔を無視=Yes
            // ----------------------------------------------------
            gWriter.write("^PQ"+Integer.toString(printNo)+",0,0,Y");
            gWriter.write("^XZ");               // ****** 終了フォーマット ****
            gWriter.write("^XA^SS,,,1432^XZ");  // ****** 印刷後最大用紙長を送る ******
        }
        // -----------------  B9サンプル  -----------------
        // ^MMR                                                                    ^FX　動作モード^FS
        // ^PQ1,0,0,Y                                                              ^FX　印刷枚数^FS
        // ^XZ
        // ^XA^SS,,,1432^XZ                                                        ^FX　フィード長指定^FS
        // --------------------------------------------
        if( "2".equals(paper_size) ){           // ******** B9 ********
            gWriter.write("^MMR");          // 巻き取りモード
            // ---- 印刷枚数 --------------------------------------
            // ラベル枚数=1,ポーズラベル枚数=0,シリアル番号複写枚数=0,ポーズ間隔を無視=Yes
            // ----------------------------------------------------
            gWriter.write("^PQ"+Integer.toString(printNo)+",0,0,Y");
            gWriter.write("^XZ");               // ****** 終了フォーマット ****
            gWriter.write("^XA^SS,,,1432^XZ");  // ****** 印刷後最大用紙長を送る ******
        }
    }



    /**
     ***************************************************************************
     * バイトbの16進String表現を返す      byteToHex
     * @param   b       (byte)
     * @return  String
     ***************************************************************************
     */
    static public String byteToHex(byte b) {
       char hexDigit[] = {
          '0', '1', '2', '3', '4', '5', '6', '7',
          '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
       };
       char[] array = { hexDigit[(b >> 4) & 0x0f], hexDigit[b & 0x0f] };
       return new String(array);
    }


    /**
     ***************************************************************************
     * 文字cの16進String表現を返す       charToHex
     * @param   c       (char)
     * @return  String
     ***************************************************************************
     */
    static public String charToHex(char c) {
       byte hi = (byte) (c >>> 8);
       byte lo = (byte) (c & 0xff);
       return byteToHex(hi) + byteToHex(lo);
    }


    /**
     ***************************************************************************
     * 指定ミリ秒実行を止める
     * @param   msec        (long)
     * @return  void
     ***************************************************************************
     */
    public synchronized void sleep(long msec)    {
        try {
            wait(msec);
        }catch(InterruptedException e){}
    }


    /**
     ***************************************************************************
     * 指定文字列のHEX表示
     * @param   in_data     (String)
     * @return  HexString   (String)
     ***************************************************************************
     */
     public static String HexCode(String s){
         int hex;
         String result = "";
         String wk = "";
         for (int i=0; i<s.length(); i++){
             hex = (int)s.charAt(i);
             wk = Integer.toHexString(hex);
             if( wk.length() == 1 ){
                 result += "0" + wk;
             }else{
                 result += wk;
             }
         }
         return result;
     }


    /**
     ***************************************************************************
     * 全角対応 の rtrim
     * @param   in_data     (String)
     * @return  out_data    (String)
     ***************************************************************************
     */
    public static String rtrimZen(String s){
        int len = s.length();
        int st = 0;
        char[] val = s.toCharArray();
        ///// rtrim なのでコメントアウト /////    // 先頭からtrim
        ///// rtrim なのでコメントアウト /////    while (st < len && (val[st] <= ' ' || val[st] == '　')) {
        ///// rtrim なのでコメントアウト /////        st++;
        ///// rtrim なのでコメントアウト /////    }
         // 後ろからtrim
        while (st < len && (val[len - 1] <= ' ' || val[len - 1] == '　')) {
            len--;
        }
        if(st > 0 || len < s.length()) {
            return s.substring(st, len);
        }
        return s;
    }

    public static String ariagondolatop(){
    	//x : height y width
    	String st =
      		"~SD15" +
    		"^XA" +
    		"^CW1,E:PRICE.TTF^FS" +
    		"^CWJ,E:IPAMB.TTF^FS" +
    		"^CWK,E:IPAGB.TTF^FS" +
    		"^XZ" +
    		"^XA^PW512^XZ" +
    		"^XA^SS,,,712^XZ" +
    		"^XA" +
    		"^MNB" +
    		"^FO60,370^GB90,250,2,B,0^FS" +
    		"^FO150,370^GB180,250,2,B,0^FS" +
    		"^CI17^FPH,0^CFK,40,40^FWB^FO85,440^F8^FD" +
    		"A^FS" +
    		"^CI17^FPH,0^CFK,80,80^FWB^FO210,460^F8^FD" +
    		"02^FS" +
    		"" +
    		"^FO60,80^GB90,250,2,B,0^FS" +
    		"^FO150,80^GB180,250,2,B,0^FS" +
    		"^CI17^FPH,0^CFK,40,40^FWB^FO85,130^F8^FD" +
    		"G^FS" +
    		"^CI17^FPH,0^CFK,80,80^FWB^FO210,160^F8^FD" +
    		"099^FS" +
    		"^FWB^FT450,600^BY5,2.0,10^B3,Y,80,N,N^FN1^FS" +
    		"^FN1^FD" +
    		"02099" +
    		"^FS" +
    		"^MMR" +
    		"^PQ1,0,0,Y" +
    		"^XZ" +
    		"^XA^SS,,,1432^XZ" +
    		""
    		;
    	return st;
    }
    public static String ariagondolabottom(){
    	//x : height y width
    	String st =
    		"~SD15" +
    		"^XA" +
    		"^CW1,E:PRICE.TTF^FS" +
    		"^CWJ,E:IPAMB.TTF^FS" +
    		"^CWK,E:IPAGB.TTF^FS" +
    		"^XZ" +
    		"^XA^PW512^XZ" +
    		"^XA^SS,,,712^XZ" +
    		"^XA" +
    		"^MNB" +
    		"^FO60,470^GB90,180,2,B,0^FS" +
    		"^FO150,470^GB180,180,2,B,0^FS" +
    		"^FO60,270^GB90,180,2,B,0^FS" +
    		"^FO150,270^GB180,180,2,B,0^FS" +
    		"^FO60,60^GB90,180,2,B,0^FS" +
    		"^FO150,60^GB180,180,2,B,0^FS" +
    		"^CI17^FPH,0^CFK,40,40^FWB^FO85,500^F8^FD" +
    		"S^FS" +
    		"^CI17^FPH,0^CFK,40,40^FWB^FO85,300^F8^FD" +
    		"A^FS" +
    		"^CI17^FPH,0^CFK,40,40^FWB^FO85,75^F8^FD" +
    		"G^FS" +
    		"^CI17^FPH,0^CFK,80,80^FWB^FO210,320^F8^FD" +
    		"02^FS" +
    		"^CI17^FPH,0^CFK,80,80^FWB^FO210,90^F8^FD" +
    		"099^FS" +
    		"^MMR" +
    		"^PQ1,0,0,Y" +
    		"^XZ" +
    		"^XA^SS,,,1432^XZ" +
    		"";

    	return st;
    }
	/**
	 * @param args
	 */
	public static void main(String[] args) {

	    String ipad = null ;
	    String content = null;
	    if(args.length > 0){
	        ipad = args[0];
	    }else{
	        System.out.println("Insert IP Address");
	        System.exit(0);
	    }

	       if(args.length > 1){
	           content = args[1];
	        }else{
	            content = ariagondolatop();
	        }
	       ZebraSingleClient zclient = new ZebraSingleClient();

		try {
			int flg = 0;
			flg = zclient.zebraSocketStart(ipad);
			//if(flg == 0) rkxg0067.zebra_close();

			flg = zclient.zebraPrintString(content);


			zclient.zebra_close();

			System.out.println(zclient.checkBuffer.toString());
			System.out.println(zclient.powerBuffer.toString());

			System.out.println(flg);

		} catch (Exception e) {
			e.printStackTrace();
		}

	}

}
