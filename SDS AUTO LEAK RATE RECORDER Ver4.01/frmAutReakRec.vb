Imports VB = Microsoft.VisualBasic

Public Class frmAutReakRec
    '*****************************************************************
    '       AUTO REAK RATE RECORD Ver4.01.07
    '                   2022/10/19
    '
    '*****************************************************************
    ' 4.01.0.7 : modify the bug that numbers above 1000 mtorr are not recorded in the DAT file.

    Structure LastResult
        Dim Stt_DateTim As String
        Dim LeakRate As Single
        Dim UltmtPres As Single
        Dim InitPres As Single
        Dim LastPres As Single
    End Structure

    Structure Sample
        Dim Bsy As Boolean
        Dim Ctr As Long
        Dim SttPrs As Single
        Dim SttIdx As Long
        Dim EndPrs As Single
        Dim EndIdx As Long
    End Structure

    Dim RoughTms As Long
    Dim MesTim As Single

    Dim LstRslt As LastResult
    Dim SmpStt As Integer
    Dim SmpEnd As Integer
    Dim Prs() As Single
    Dim Min() As Long
    Dim Smptims As Long

    'Device No
    Dim AutoStt_PLC As String = "7100"      'M7101
    Dim AutoBsy_PLC As String = "7101"      'M7100
    Dim RoughTms_PLC As String = "1400"     'D1400
    Dim MesTim_PLC As String = "1420"       'D1420
    Dim SmpBsy_PLC As String = "7110"       'M7110
    Dim SmpsttPrs_PLC As String = "1410"    'D1410
    'PIRANI(Pres)
    Dim Prs_PLC As String = "1048"          'D1048
    Dim Prs2_PLC As String = "1048"          'D1048
    Dim Prs3_PLC As String = "1048"          'D1048
    Dim Prs4_PLC As String = "1048"          'D1048

    Dim TorrMax As Single
    Dim AutBsy As Integer
    Dim Smp As Sample

    Dim LonCLR As System.Drawing.Color = Color.Lime         'Lmp ON color
    Dim LofCLR As System.Drawing.Color = Color.Silver       'Lmp OFF color
    Dim SonCLR As System.Drawing.Color = Color.Coral        'Sw ON color
    Dim SofCLR As System.Drawing.Color = Color.LightGray  'Sw OFF color

    Dim g As System.Drawing.Graphics

    Dim Country As String = "E"
    Dim Blink As Boolean = False
    Dim WavLogEnable As Boolean = False

    Private WaveLogger As Object
    Private Ordrlvl As Integer

    Dim PLC_Connected As Boolean    'Append 2022/05/26

    Private Sub frmAutReakRec_Load(sender As Object, e As EventArgs) Handles Me.Load
        '===============
        '   Initialize
        '===============

        If Diagnostics.Process.GetProcessesByName(Diagnostics.Process.GetCurrentProcess.ProcessName).Length > 1 Then
            Application.Exit()
        End If

        '-------- Rev.2022/02
        'IP and Version of PLC are stored in HKEY_CURRENT_USER\SoftWare\SDS\PLC
        AxDBCommManager1.Peer = rgsplcip() & ":8500"    'default 192.168.3.13 & port 8500
        AxDBCommManager1.PLC = CInt(RgsPlcSr())         'default 515 = KV-5500/5000/3000

        'Try
        '    AxDBCommManager1.Connect()
        'Catch ex As Exception
        '    MessageBox.Show(ex.Message, "COM+")
        '    Exit Sub
        'End Try

        'KV-COM+の機能でPLCとの接続確認
        PLC_Connected = AxDBCommManager1_Connection_Check()
        If PLC_Connected = False Then
            MessageBox.Show("PLC not found !")
            Exit Sub
        End If

        Dim gx As Integer = PictureBox1.Size.Width
        Dim gy As Integer = PictureBox1.Size.Height
        PictureBox1.Image = New Bitmap(gx, gy)
        'g = PictureBox1.CreateGraphics
        'g = Graphics.FromImage(PictureBox1.Image)

        AxDBTriggerManager3.Active = True   'Trigger for Pirani Display
        Timer1.Enabled = True
        'WavLogEnable = WavLogCreate()
        'DrwGrid()
        DspTorr_ordr(1, 0.0001)
        Drw_LogGrid(Ordrlvl)

    End Sub

    Private Function RgsPlcIp() As String
        Dim regkey As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey("Software\SDS\PLC", True)

        'Case of 1st time installed -> Create keys, default IP:192.168.3.13 / default Ver:515(KV-5500/5000/3000)
        If regkey Is Nothing Then
            regkey = Microsoft.Win32.Registry.CurrentUser.CreateSubKey("Software\SDS\PLC")
            regkey.SetValue("IP", "192.168.3.13", Microsoft.Win32.RegistryValueKind.String)
            regkey.SetValue("Ver", "545", Microsoft.Win32.RegistryValueKind.String) '515 = KV5500/5000/3000, 545 = KV-8000
            Return "192.168.3.13"
        End If

        'Read the IP address of the PLC
        Dim stringValue As String = DirectCast(regkey.GetValue("IP", "192.168.3.13"), String)
        regkey.Close()

        Return stringValue

    End Function

    Private Function RgsPlcSr() As String
        Dim regkey As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey("Software\SDS\PLC", True)

        'Read the Model version of the PLC
        Dim stringValue As String = DirectCast(regkey.GetValue("Ver", "545"), String)
        regkey.Close()

        Return stringValue

    End Function

    Private Sub WrtPlcSr(PlcVerNum As Integer)

        Dim regkey As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey("Software\SDS\PLC", True)

        'write the model of the PLC
        regkey.SetValue("Ver", PlcVerNum, Microsoft.Win32.RegistryValueKind.String)
        regkey.Close()

    End Sub

    Private Function AxDBCommManager1_Connection_Check() As Boolean
        Dim answer As String

        Try
            AxDBCommManager1.Connect()
        Catch ex1 As Exception
            MessageBox.Show(ex1.Message, "COM+ ERROR")
            If ex1.Message = "Failed to open the port." Then
                MessageBox.Show("Please check your LAN cable and IP settings.", "COM+ ERROR")
                Return False
            Else
                MessageBox.Show("The system will search the compatible model...", "COM+ setup assistant")
            End If
            WrtPlcSr(545)
            AxDBCommManager1.PLC = 545
            MessageBox.Show("Tentative to connect KV-8000", "COM+ setup assistant")

            Try
                AxDBCommManager1.Connect()
            Catch ex2 As Exception
                WrtPlcSr(515)
                AxDBCommManager1.PLC = 515
                MessageBox.Show("Tentative to connect KV-5000/5500", "COM+ setup assistant")

                Try
                    AxDBCommManager1.Connect()
                Catch ex3 As Exception
                    WrtPlcSr(525)
                    AxDBCommManager1.PLC = 525
                    MessageBox.Show("Tentative to connect KV-7000/7500", "COM+ setup assistant")

                    Try
                        AxDBCommManager1.Connect()
                    Catch ex4 As Exception
                        answer = InputBox("Please enter the PLC model:", "PLC not registered", "65534")
                        WrtPlcSr(CInt(answer))
                        AxDBCommManager1.PLC = CInt(answer)

                        Try
                            AxDBCommManager1.Connect()
                        Catch ex5 As Exception
                            MessageBox.Show(ex5.Message, "COM+ ERROR")
                            Return False

                        End Try
                    End Try
                End Try
            End Try
        End Try
        Return True

    End Function

    Private Sub frmAutReakRec_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        'Program is closed
        If AutBsy = 1 Then
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_MR, AutoStt_PLC, 1)
        End If
        'WaveLogger = Nothing

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'Start

        'Dim interLock = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "7113")
        Dim interLock = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_MR, "7113")
        AutBsy = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_MR, AutoBsy_PLC)

        If AutBsy = 0 And interLock = 1 And Button1.Text = "Start" Then

            GrphClr()
            'DspTorr_ordr(1, 0.0001)
            Drw_LogGrid(Ordrlvl)

            TextBox3.Text = ""
            TextBox4.Text = ""
            TextBox5.Text = ""
            TextBox7.Text = ""

            TextBox1.Enabled = False
            TextBox2.Enabled = False

            RoughTms = CLng(TextBox1.Text)
            MesTim = CSng(TextBox2.Text)
            If MesTim < 0.1 Or MesTim > 100 Then
                MessageBox.Show("Mesurement Time Err: T>=0.1 and T=<100")
                Exit Sub
            End If

            'TorrMax = CSng(TextBox6.Text) * 1

            'Label14.Text = CStr(TorrMax / 2)
            Label18.Text = Format(CSng(MesTim) * 1 / 4, "0.00")
            Label15.Text = Format(CSng(MesTim) * 2 / 4, "0.00")
            Label19.Text = Format(CSng(MesTim) * 3 / 4, "0.00")
            Label16.Text = Format(CSng(MesTim) * 4 / 4, "0.00")

            Smptims = MesTim * 60 + 1

            ReDim Prs(Smptims)
            ReDim Min(Smptims)

            With AxDBCommManager1
                .WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, RoughTms_PLC, RoughTms)
                .WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, MesTim_PLC, MesTim * 60)
                .WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_MR, AutoStt_PLC, 1)
            End With

            AxDBTriggerManager1.Active = True   'AutoBsy Check

            Smp.Ctr = 0
            Smp.Bsy = False

            Blink = True
            Button1.Text = "Stop"

            'If WavLogEnable = True Then
            '    Dim m As Long = WaveLogger.Start()
            'End If

            AxDBTriggerManager2.Active = True   'Sampling trigger


        ElseIf AutBsy = 1 And Button1.Text = "Stop" Then
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_MR, AutoStt_PLC, 1)
            Do
                Application.DoEvents()
            Loop Until AutBsy = 0
            Blink = False
            AxDBTriggerManager1.Active = False
            Button1.BackColor = SofCLR
            Button1.Text = "Start"
            TextBox1.Enabled = True
            TextBox2.Enabled = True

        End If

    End Sub

    Private Function WavLogCreate() As Boolean

        WaveLogger = CreateObject("WaveLogger2.Application")
        WaveLogger.Initialize()
        WaveLogger.Visible = True

        Dim m As Long
        m = WaveLogger.SetIdentifier(0)
        If m <> 0 Then
            WaveLogger.Quit()
            WaveLogger = Nothing
            Return False
        End If

        Dim a As Long = WaveLogger.OpenFile("C:\Program Files (x86)\KEYENCE\WAVE LOGGER\default.xcf")
        Dim n As Long
        n = WaveLogger.GetUnitInfo(0)
        Dim l As Long
        l = WaveLogger.GetState()
        Dim k As Long
        k = WaveLogger.GetWindowCount()
        WaveLogger.ActivateWindow(1)
        Return True

    End Function



    Private Function XGscl(ByVal dx As Single) As Single
        'Ｘaxs scaling for Grid
        Dim xs As Single = 0
        Dim xe As Single = PictureBox1.Size.Width
        Dim gx As Integer = xe
        Dim scl As Single

        scl = gx * (dx - xs) / (xe - xs)
        Return scl

    End Function

    Private Function Xscl(ByVal dx As Single) As Single
        'Ｘaxs scaling
        Dim xs As Single = 0
        Dim xe As Single = CSng(MesTim * 60 + 0)
        Dim gx As Integer = PictureBox1.Size.Width
        Dim scl As Single

        scl = gx * (dx - xs) / (xe - xs)
        Return scl

    End Function

    Private Function YGscl(ByVal dy As Single) As Single
        'Ｙaxs scaling for Grid
        Dim ys As Single
        Dim ye As Single = PictureBox1.Size.Height
        Dim gy As Integer = ye
        Dim scl As Single

        Dim log_min As Single = Label12.Text
        Dim log_max As Single = Label32.Text

        ys = Math.Log10(log_min)
        ye = Math.Log10(log_max)

        If ys - ye <> 0 Then
            scl = gy * (dy - ye) / (ys - ye)
            Return scl
        Else
            Return 0
        End If

    End Function

    Private Function Yscl(ByVal dy As Single) As Single
        'Ｙaxs scaling
        Dim ys As Single
        Dim ye As Single
        Dim gy As Integer = PictureBox1.Size.Height
        Dim scl As Single

        'ys = 0
        'ye = TorrMax

        Dim log_min As Single = Label12.Text
        Dim log_max As Single = Label32.Text

        ys = Math.Log10(log_min)
        ye = Math.Log10(log_max)

        If ys - ye <> 0 Then
            scl = gy * (dy - ye) / (ys - ye)
            Return scl
        Else
            Return 0
        End If

    End Function

    Private Sub DrwGrid()
        '*No use
        Dim gx As Integer = PictureBox1.Size.Width
        Dim gy As Integer = PictureBox1.Size.Height
        Dim divx = 8
        Dim divy = 4

        Dim xs As Single = gx / divx
        Dim xe As Single = gx / divx
        Dim ys As Single = 0
        Dim ye As Single = gy

        'PictureBox1.Image = New Bitmap(gx, gy)
        g = PictureBox1.CreateGraphics
        g = Graphics.FromImage(PictureBox1.Image)

        'vertical line
        g.DrawLine(Pens.DarkSeaGreen, CSng(1 * xs), ys, CSng(1 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(2 * xs), ys, CSng(2 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(3 * xs), ys, CSng(3 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(4 * xs), ys, CSng(4 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(5 * xs), ys, CSng(5 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(6 * xs), ys, CSng(6 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(7 * xs), ys, CSng(7 * xe), ye)

        'Horizontal line
        xs = 0
        xe = gx
        ys = gy / divy
        ye = gy / divy

        g.DrawLine(Pens.DarkSeaGreen, xs, 1 * ys, xe, 1 * ye)
        g.DrawLine(Pens.DarkSeaGreen, xs, 2 * ys, xe, 2 * ye)
        g.DrawLine(Pens.DarkSeaGreen, xs, 3 * ys, xe, 3 * ye)
        g.DrawLine(Pens.DarkSeaGreen, xs, 4 * ys, xe, 4 * ye)


    End Sub

    Private Sub Drw_LogGrid(ByVal level As Integer)
        'LogRuledline
        Dim gx As Integer = PictureBox1.Size.Width
        Dim gy As Integer = PictureBox1.Size.Height
        Dim divx As Integer = 8
        Dim divy As Integer = 4
        Dim i As Integer

        Dim xs As Single = gx / divx
        Dim xe As Single = gx / divx
        Dim ys As Single = 0
        Dim ye As Single = gy
        g = PictureBox1.CreateGraphics
        g = Graphics.FromImage(PictureBox1.Image)

        'vertical line
        g.DrawLine(Pens.DarkSeaGreen, CSng(1 * xs), ys, CSng(1 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(2 * xs), ys, CSng(2 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(3 * xs), ys, CSng(3 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(4 * xs), ys, CSng(4 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(5 * xs), ys, CSng(5 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(6 * xs), ys, CSng(6 * xe), ye)
        g.DrawLine(Pens.DarkSeaGreen, CSng(7 * xs), ys, CSng(7 * xe), ye)

        'Horizontal line
        Dim ly(5) As Single

        xs = 0
        xe = gx

        '2022-10-19 Changed graph upper limit from 1 torr to 10 torr
        'Add level = 5
        If level = 5 Then
            ly(0) = (Label32.Text)
            ly(1) = (Label39.Text)
            ly(2) = (Label40.Text)
            ly(3) = (Label41.Text)
            ly(4) = (Label42.Text)
            ly(5) = (Label12.Text)
        End If
        If level = 4 Then
            ly(0) = (Label32.Text)
            ly(1) = (Label33.Text)
            ly(2) = (Label14.Text)
            ly(3) = (Label34.Text)
            ly(4) = (Label12.Text)
        End If
        If level = 3 Then
            ly(0) = (Label32.Text)
            ly(1) = (Label35.Text)
            ly(2) = (Label36.Text)
            ly(3) = (Label12.Text)
        End If
        If level = 2 Then
            ly(0) = (Label32.Text)
            ly(1) = (Label14.Text)
            ly(2) = (Label12.Text)
        End If
        If level = 1 Then
            ly(0) = (Label32.Text)
            ly(1) = (Label12.Text)
        End If


        For i = 0 To level - 1
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 9 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 9 / 9)))
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 8 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 8 / 9)))
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 7 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 7 / 9)))
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 6 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 6 / 9)))
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 5 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 5 / 9)))
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 4 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 4 / 9)))
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 3 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 3 / 9)))
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 2 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 2 / 9)))
            g.DrawLine(Pens.DarkSeaGreen, XGscl(xs), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 1 / 9)), XGscl(xe), YGscl(Math.Log10((ly(i) - ly(i + 1)) * 1 / 9)))
        Next

    End Sub


    Private Sub DrwGraph()

        Dim gx As Integer = PictureBox1.Size.Width
        Dim gy As Integer = PictureBox1.Size.Height
        Dim i As Integer
        Dim p1 As Single
        Dim p2 As Single

        PictureBox1.Image = New Bitmap(gx, gy)
        g = PictureBox1.CreateGraphics
        g = Graphics.FromImage(PictureBox1.Image)

        Drw_LogGrid(Ordrlvl)

        If Smp.Ctr > 1 Then
            For i = 2 To Smp.Ctr
                Try
                    p1 = Prs(i - 1) / 1000  'mTorr---->Torr
                    p2 = Prs(i) / 1000
                    g.DrawLine(Pens.DarkGreen, Xscl(Min(i - 1)), Yscl(Math.Log10(p1)), Xscl(Min(i)), Yscl(Math.Log10(p2)))
                Catch ex As Exception
                    'MessageBox.Show(ex.Message)
                End Try
            Next
        End If

    End Sub

    Private Sub ReDrwgraph()
        'Redrow graph
        Dim gx As Integer = PictureBox1.Size.Width
        Dim gy As Integer = PictureBox1.Size.Height
        Dim i As Integer
        Dim p1 As Single
        Dim p2 As Single

        PictureBox1.Image = New Bitmap(gx, gy)
        g = PictureBox1.CreateGraphics
        g = Graphics.FromImage(PictureBox1.Image)

        GrphClr()
        Drw_LogGrid(Ordrlvl)

        If Smp.Ctr > 1 Then
            For i = 2 To Smp.Ctr
                Try
                    p1 = Prs(i - 1) / 1000  'mTorr---->Torr
                    p2 = Prs(i) / 1000
                    g.DrawLine(Pens.DarkGreen, Xscl(Min(i - 1)), Yscl(Math.Log10(p1)), Xscl(Min(i)), Yscl(Math.Log10(p2)))
                Catch ex As Exception
                    'MessageBox.Show(ex.Message)
                End Try
            Next
        End If

    End Sub

    Private Sub GrphClr()
        'graph clear
        Dim gx As Integer = PictureBox1.Size.Width
        Dim gy As Integer = PictureBox1.Size.Height
        PictureBox1.Image = New Bitmap(gx, gy)
        g.Clear(Color.PaleGreen)
        'DrwGrid()

    End Sub

    Private Function Read_PresSmp() As String
        'get data from PIRANI
        Dim pirani1 As Single
        Dim pirani2 As Integer
        Dim pirani3 As Integer
        Dim pirani4 As Integer
        Dim prs As String = ""

        'Do
        prs = ""
        pirani2 = AxDBDeviceManager2.Devices(2).ValueRead
        pirani3 = AxDBDeviceManager2.Devices(3).ValueRead
        pirani4 = AxDBDeviceManager2.Devices(4).ValueRead

        If pirani3 = 0 Then  '-----　ガス使用中フラグ
            'If pirani4 = 0 Then '.***
            '    If pirani2 = 1 Then
            '        pirani1 = CSng(AxDBDeviceManager2.Devices(1).ValueRead + 50000) / 100
            '    Else
            '        pirani1 = CSng(AxDBDeviceManager2.Devices(1).ValueRead) / 100
            '    End If
            '    prs = Format(pirani1, "0.00") + "0"
            'Else
            '    pirani1 = CSng(AxDBDeviceManager2.Devices(1).ValueRead) / 1000
            '    prs = Format(pirani1, "0.000")
            'End If

            'mTorrでデータを測定する
            If pirani4 = 0 Then '.***
                If pirani2 = 1 Then
                    pirani1 = CSng(AxDBDeviceManager2.Devices(1).ValueRead + 50000) * 10
                Else
                    pirani1 = CSng(AxDBDeviceManager2.Devices(1).ValueRead) * 10
                End If
                prs = Format(pirani1, "0")
            Else
                pirani1 = CSng(AxDBDeviceManager2.Devices(1).ValueRead) * 1000 / 1000
                prs = Format(pirani1, "0")
            End If

        Else
            prs = "99999"
        End If

        'Loop Until prs <> "99999"

        Return prs

    End Function

    Private Function Read_Press() As String
        'get data from PIRANI
        Dim pirani1 As Single
        Dim pirani2 As Integer
        Dim pirani3 As Integer
        Dim pirani4 As Integer
        Dim prs As String = ""

        'Do
        prs = ""
        pirani2 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1049")
        pirani3 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1053")
        pirani4 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1063")

        If pirani3 = 0 Then  '-----
            'If pirani4 = 0 Then '.***
            '    If pirani2 = 1 Then
            '        pirani1 = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1048") + 50000) / 100
            '    Else
            '        pirani1 = CSng(AxDBDeviceManager2.Devices(1).ValueReadAxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1048")) / 100
            '    End If
            '    prs = Format(pirani1, "0.00") + "0"
            'Else
            '    pirani1 = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1048")) / 1000
            '    prs = Format(pirani1, "0.000")
            'End If

            If pirani4 = 0 Then '.***
                If pirani2 = 1 Then
                    pirani1 = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1048") + 50000) * 10
                Else
                    'pirani1 = CSng(AxDBDeviceManager2.Devices(1).ValueReadAxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1048")) * 10
                    pirani1 = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1048")) * 10    'debug 2022-10-20 @KJ
                End If
                prs = Format(pirani1, "0")
            Else
                pirani1 = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_DM, "1048")) * 1000 / 1000
                prs = Format(pirani1, "0")
            End If
        Else
            prs = "99999"
        End If

        'Loop Until prs <> "99999"

        Return prs

    End Function

    Private Function Read_PressDsp() As String
        'get data from PIRANI
        Dim pirani1 As Single
        Dim pirani2 As Integer
        Dim pirani3 As Integer
        Dim pirani4 As Integer
        Dim prs As String = ""

        'Do
        prs = ""
        pirani2 = AxDBDeviceManager3.Devices(2).ValueRead       'DM1049 PIRANI現在値
        pirani3 = AxDBDeviceManager3.Devices(3).ValueRead       'DM1053
        pirani4 = AxDBDeviceManager3.Devices(4).ValueRead       'DM1063

        If pirani3 = 0 Then  '-----
            If pirani4 = 0 Then '.***
                'If pirani2 = 1 Then
                '    pirani1 = CSng(AxDBDeviceManager3.Devices(1).ValueRead + 50000) / 100
                'Else
                '    pirani1 = CSng(AxDBDeviceManager3.Devices(1).ValueRead) / 100
                'End If
                'prs = Format(pirani1, "0.00") + "0"
                If pirani2 = 1 Then
                    pirani1 = CSng(AxDBDeviceManager3.Devices(1).ValueRead + 50000) * 10
                Else
                    pirani1 = CSng(AxDBDeviceManager3.Devices(1).ValueRead) * 10
                End If
                prs = Format(pirani1, "0")
            Else
                'pirani1 = CSng(AxDBDeviceManager3.Devices(1).ValueRead) / 1000
                'prs = Format(pirani1, "0.000")
                pirani1 = CSng(AxDBDeviceManager3.Devices(1).ValueRead) * 1000 / 1000   '*1000--->mTorr
                prs = Format(pirani1, "0")
            End If
        Else
            prs = "------"
        End If

        'Loop Until prs <> "99999"

        Return prs

    End Function

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'Debugg button

        'LstRslt.Stt_DateTim = DateString & " " & TimeString 'DateAndTime.Now
        'TextBox3.Text = LstRslt.Stt_DateTim
        'DatFileSave()

        'Sampling process(interval:1min)

        Dim i As Long
        Dim p As Single

        Do
            p = CSng(Read_Press())
        Loop Until p <> 99999

        Smp.Ctr = Smp.Ctr + 1
        i = Smp.Ctr
        'Prs(i) = p * 1000   'mTorr
        Prs(i) = p   'mTorr

        Min(i) = Smp.Ctr - 1

        If Smp.Ctr = 1 Then
            Button1.BackColor = SonCLR
            Label22.BackColor = LonCLR

            Smp.SttPrs = Prs(1)
            Min(1) = 0
            Smp.Bsy = True
            LstRslt.Stt_DateTim = DateString & " " & TimeString  'DateAndTime.Now
            LstRslt.InitPres = Smp.SttPrs
            Blink = False
            TextBox3.Text = LstRslt.Stt_DateTim
            TextBox5.Text = LstRslt.InitPres
        End If

        DrwGraph()


    End Sub

    Private Sub AxDBDeviceManager3_AfterRead(sender As Object, e As EventArgs) Handles AxDBDeviceManager3.AfterRead
        Label17.Text = Read_PressDsp()
        Label30.Text = AxDBDeviceManager3.Devices(5).ValueRead  'times of Rough
    End Sub

    Private Sub DatFileSave()
        'Data save 

        Dim fileName As String
        Dim fileNum As Integer
        Dim i As Integer = 0
        Dim s As String
        Dim d(6) As String


        'If Smp.Ctr = 0 Then Exit Sub

        If Country = "E" Then
            'with English format !!! ex. "06/08/2021/_14:50:30"
            d(0) = Mid(LstRslt.Stt_DateTim, 1, 2)    'MM
            d(1) = Mid(LstRslt.Stt_DateTim, 4, 2)    'DD
            d(2) = Mid(LstRslt.Stt_DateTim, 7, 4)    'YYYY
            d(3) = Mid(LstRslt.Stt_DateTim, 12, 2)   'hh
            d(4) = Mid(LstRslt.Stt_DateTim, 15, 2)   'mm
            d(5) = Mid(LstRslt.Stt_DateTim, 18, 2)   'ss
        Else
            'with Jpan format !!! ex. "2021/08/21/_14:50:30"
            d(0) = Mid(LstRslt.Stt_DateTim, 1, 4)    'YYYY
            d(1) = Mid(LstRslt.Stt_DateTim, 6, 2)    'MM
            d(2) = Mid(LstRslt.Stt_DateTim, 9, 2)    'DD
            d(3) = Mid(LstRslt.Stt_DateTim, 12, 2)   'hh
            d(4) = Mid(LstRslt.Stt_DateTim, 15, 2)   'mm
            d(5) = Mid(LstRslt.Stt_DateTim, 18, 2)   'ss
        End If

        Dim PATH As String

        If Country = "E" Then
            PATH = "C:\SDS6NETSYS\DAT\" & d(0) & d(1) & "_" & d(2) & "_" & d(3) & d(4) & d(5) & "_PRR\"
            fileName = PATH & d(0) & d(1) & "_" & d(2) & "_" & d(3) & d(4) & d(5) & "_PRR" & ".txt"
        Else
            PATH = "C:\SDS6NETSYS\DAT\" & d(0) & "_" & d(1) & d(2) & "_" & d(3) & d(4) & d(5) & "_PRR\"
            fileName = PATH & d(0) & "_" & d(1) & d(2) & "_" & d(3) & d(4) & d(5) & "_PRR" & ".txt"
        End If

        s = Dir(PATH)
        If s = "" Then
            MkDir(PATH)
        End If

        fileNum = FreeFile()
        FileOpen(fileNum, fileName, OpenMode.Output)
        'WriteLine(fileNum, RoughTms, MesTim, LstRslt.Stt_DateTim, LstRslt.LeakRate, LstRslt.InitPres)  '*Comment out 2022/0514
        'WriteLine(fileNum, "Purge cycle", RoughTms)
        'WriteLine(fileNum, "Measurement Time", MesTim)
        'WriteLine(fileNum, " ")
        'WriteLine(fileNum, "Start Date/Time", LstRslt.Stt_DateTim)
        'WriteLine(fileNum, "Initial Pressure", LstRslt.InitPres, "mTorr")
        'WriteLine(fileNum, "Last Pressure", LstRslt.LastPres, "mTorr")
        'WriteLine(fileNum, "Rising Rate", LstRslt.LeakRate, "mTorr/h")
        'WriteLine(fileNum, " ")
        'WriteLine(fileNum, "Time (min)", "Pressure (mTorr)")

        Print(fileNum, "Purge cycle") : Print(fileNum, vbTab) : Print(fileNum, RoughTms) : Print(fileNum, vbTab) : Print(fileNum, "Cycles") : Print(fileNum, vbCr)
        Print(fileNum, "Measurement Time") : Print(fileNum, vbTab) : Print(fileNum, Format(MesTim, "0.0")) : Print(fileNum, vbTab) : Print(fileNum, "h") : Print(fileNum, vbCr)
        Print(fileNum, " ") : Print(fileNum, vbTab) : Print(fileNum, vbCr)
        Print(fileNum, "Start Date/Time") : Print(fileNum, vbTab) : Print(fileNum, LstRslt.Stt_DateTim) : Print(fileNum, vbCr)
        Print(fileNum, "Initial Pressure") : Print(fileNum, vbTab) : Print(fileNum, LstRslt.InitPres) : Print(fileNum, vbTab) : Print(fileNum, "mTorr") : Print(fileNum, vbCr)
        Print(fileNum, "Last Pressure") : Print(fileNum, vbTab) : Print(fileNum, LstRslt.LastPres) : Print(fileNum, vbTab) : Print(fileNum, "mTorr") : Print(fileNum, vbCr)
        Print(fileNum, "Rising Rate") : Print(fileNum, vbTab) : Print(fileNum, Format(LstRslt.LeakRate, "0.00")) : Print(fileNum, vbTab) : Print(fileNum, "mTorr/h") : Print(fileNum, vbCr)
        Print(fileNum, " ") : Print(fileNum, vbTab) : Print(fileNum, vbCr)
        Print(fileNum, "Time (min)") : Print(fileNum, vbTab) : Print(fileNum, "Pressure (mTorr)") : Print(fileNum, vbCr)

        'Write Pressure to dat file
        For i = 1 To Smptims
            Print(fileNum, Min(i)) : Print(fileNum, vbTab) : Print(fileNum, Prs(i)) : Print(fileNum, vbCr)
        Next

        FileClose(fileNum)

    End Sub

    Private Sub TextBox6_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox6.KeyPress
        'Yacxs full scale change
        If e.KeyChar = vbCr Then
            TextBox6.BackColor = Color.LightSteelBlue
            TorrMax = TextBox6.Text
            Label14.Text = TorrMax / 2
            If Smp.Ctr > 1 Then
                'GrphClr()   'Append 20210626
                ReDrwgraph()
            End If
        End If
    End Sub

    Private Sub TextBox6_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox6.KeyDown
        TextBox6.BackColor = Color.Yellow
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'Lamp flicker 0.5sec
        Static onf As Boolean = False

        If Blink = True Then
            onf = Not onf
            If onf = True Then
                Button1.BackColor = SonCLR
                Label22.BackColor = LonCLR
            Else
                Button1.BackColor = SofCLR
                Label22.BackColor = LofCLR
            End If
        Else
            onf = False
        End If


        'DrwGrid()
        'ReDrwgraph()


    End Sub

    Private Sub AxDBTriggerManager1_Fire(sender As Object, e As AxDATABUILDERAXLibLB._IDBTriggerEvents_FireEvent) Handles AxDBTriggerManager1.Fire
        'AutoBsy Check
        Dim autobsy As Integer
        autobsy = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_MR, AutoBsy_PLC)

        If autobsy = 1 Then
            AutBsy = 1
            Blink = True
        Else
            AutBsy = 0
            Blink = False
            Button1.BackColor = SofCLR
            Button1.Text = "Start"
            Label22.BackColor = LofCLR
            AxDBTriggerManager2.Active = False   'Sampling trigger

            Smp.SttIdx = 1
            Smp.EndIdx = Smp.Ctr

            Dim dt As Single = CSng(Min(Smp.EndIdx) - Min(Smp.SttIdx))

            Smp.EndPrs = Prs(Smp.EndIdx)
            LstRslt.LastPres = Smp.EndPrs
            Dim dp As Single = Smp.EndPrs - Smp.SttPrs

            Try
                If Smp.Ctr >= Smptims Then    'Append 2022/07/19
                    Dim lr As Single = (dp * 60) / dt
                    Dim ls As String = Format(lr, "0.00")
                    LstRslt.LeakRate = lr ' CSng(ls)
                    TextBox4.Text = Format(LstRslt.LeakRate, "0.00")
                    TextBox7.Text = Format(LstRslt.LastPres, "0") 'Last pressure  *Append 2022/05/14
                    DatFileSave()   'move here 2022/07/19
                End If
            Catch ex As Exception
                TextBox4.Text = "---"
            End Try

            TextBox1.Enabled = True
            TextBox2.Enabled = True

            'If WavLogEnable = True Then
            '    WaveLogger.Stop()
            'End If
        End If
    End Sub


    Private Sub AxDBTriggerManager2_Fire(sender As Object, e As AxDATABUILDERAXLibLB._IDBTriggerEvents_FireEvent) Handles AxDBTriggerManager2.Fire
        'Sampling process(interval:1min)

        Dim i As Long
        Dim p As Single

        Do
            p = CSng(Read_Press())
        Loop Until p <> 99999

        Smp.Ctr = Smp.Ctr + 1
        i = Smp.Ctr
        'Prs(i) = p * 1000   'mTorr
        Prs(i) = p   'mTorr
        Min(i) = Smp.Ctr - 1

        If Smp.Ctr = 1 Then
            Button1.BackColor = SonCLR
            Label22.BackColor = LonCLR

            Smp.SttPrs = Prs(1)
            Min(1) = 0
            Smp.Bsy = True
            LstRslt.Stt_DateTim = DateString & " " & TimeString  'DateAndTime.Now
            LstRslt.InitPres = Smp.SttPrs
            Blink = False
            TextBox3.Text = LstRslt.Stt_DateTim
            TextBox5.Text = LstRslt.InitPres
        End If

        DrwGraph()

    End Sub

    Private Sub frmAutReakRec_FormClosing(sender As Object, e As FormClosingEventArgs) Handles MyBase.FormClosing
        AutBsy = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_MR, AutoBsy_PLC)
        If AutBsy = 1 Then
            MsgBox("Can not close !")
            e.Cancel = True
            Exit Sub
        End If

        If MsgBox("Do you close this program ?", MsgBoxStyle.YesNo) = vbYes Then
            e.Cancel = False
        Else
            e.Cancel = True
        End If
    End Sub

    Private Sub TextBox2_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox2.KeyPress
        'Mesurement Time

        If e.KeyChar = vbCr Then
            TextBox2.BackColor = SystemColors.Window
            TextBox2.Text = Format(Val(TextBox2.Text), "0.0")
        End If
    End Sub

    Private Sub TextBox2_KeyUp(sender As Object, e As KeyEventArgs) Handles TextBox2.KeyUp
        'Mesurement Time
        'If e.KeyData = Keys.OemPeriod Or e.KeyData = Keys.Decimal Then
        '    MessageBox.Show("Value is not integer!")
        '    TextBox2.Text = ""
        'End If
        If e.KeyData = Keys.Enter Then Exit Sub
        TextBox2.BackColor = Color.Yellow
        If IsNumeric(TextBox2.Text) = False Then
            TextBox2.Text = ""
        End If
        If Val(TextBox2.Text) > 99 Then
            MessageBox.Show("Value =< 99")
            TextBox2.Text = ""
        End If
    End Sub

    Private Sub TextBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox1.KeyPress
        'Purge Time
        If e.KeyChar = vbCr Then
            TextBox1.BackColor = SystemColors.Window
            TextBox1.Text = Format(Val(TextBox1.Text), "0")
        End If
    End Sub

    Private Sub TextBox1_KeyUp(sender As Object, e As KeyEventArgs) Handles TextBox1.KeyUp
        'Purge Time
        If e.KeyData = Keys.Enter Then Exit Sub
        TextBox1.BackColor = Color.Yellow
        If e.KeyData = Keys.OemPeriod Or e.KeyData = Keys.Decimal Then
            MessageBox.Show("Value is not integer!")
            TextBox1.Text = ""
        End If
        If IsNumeric(TextBox1.Text) = False Then
            TextBox1.Text = ""
        End If

    End Sub



    Private Sub DspTorr_ordr(ByVal torr, ByVal torr4)
        Dim r As Double = torr / torr4
        Select Case r
            '2022-10-19 Changed graph upper limit from 1 torr to 10 torr
            'Add "Case 100000" and "Label39-42"
            Case 100000
                Label33.Visible = False : Label14.Visible = False : Label34.Visible = False
                Label35.Visible = False : Label36.Visible = False
                Label39.Visible = True : Label40.Visible = True : Label41.Visible = True : Label42.Visible = True
                Label32.Text = Format(torr / 1.0, "0.0E+00")
                Label39.Text = Format(torr / 10.0, "0.0E+00")
                Label40.Text = Format(torr / 100.0, "0.0E+00")
                Label41.Text = Format(torr / 1000.0, "0.0E+00")
                Label42.Text = Format(torr / 10000.0, "0.0E+00")
                Label12.Text = Format(torr / 100000.0, "0.0E+00")
                Ordrlvl = 5
            Case 10000
                Label33.Visible = True : Label14.Visible = True : Label34.Visible = True
                Label35.Visible = False : Label36.Visible = False
                Label39.Visible = False : Label40.Visible = False : Label41.Visible = False : Label42.Visible = False

                Label32.Text = Format(torr / 1.0, "0.0E+00")
                Label33.Text = Format(torr / 10.0, "0.0E+00")
                Label14.Text = Format(torr / 100.0, "0.0E+00")
                Label34.Text = Format(torr / 1000.0, "0.0E+00")
                Label12.Text = Format(torr / 10000.0, "0.0E+00")
                Ordrlvl = 4
            Case 1000
                Label33.Visible = False : Label14.Visible = False : Label34.Visible = False
                Label35.Visible = True : Label36.Visible = True
                Label39.Visible = False : Label40.Visible = False : Label41.Visible = False : Label42.Visible = False
                Label35.Text = Format(torr / 10.0, "0.0E+00")
                Label36.Text = Format(torr / 100.0, "0.0E+00")
                Ordrlvl = 3
            Case 100
                Label33.Visible = False : Label14.Visible = True : Label34.Visible = False
                Label35.Visible = False : Label36.Visible = False
                Label39.Visible = False : Label40.Visible = False : Label41.Visible = False : Label42.Visible = False
                Label14.Text = Format(torr / 10.0, "0.0E+00")
                Ordrlvl = 2
            Case 10
                Label33.Visible = False : Label14.Visible = False : Label34.Visible = False
                Label35.Visible = False : Label36.Visible = False
                Label39.Visible = False : Label40.Visible = False : Label41.Visible = False : Label42.Visible = False
                Ordrlvl = 1
        End Select

        ReDrwgraph()

    End Sub


    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        'Torr high rang+
        Dim torr As Double = Label32.Text
        Dim torr4 As Double = Label12.Text

        torr = torr * 10.0
        'If torr > 1 Then Exit Sub      
        If torr > 10 Then Exit Sub          'Raise the upper limit of the graph from 1 Torr to 10 Torr.
        Label32.Text = Format(torr, "0.0E+00")

        DspTorr_ordr(torr, torr4)

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        'Torr high range-
        Dim torr As Double = Label32.Text
        Dim torr4 As Double = Label12.Text

        torr = torr / 10.0
        If torr = 0.0001 Then Exit Sub
        If torr = 0.001 And torr4 = 0.001 Then Exit Sub
        If torr = 0.01 And torr4 = 0.01 Then Exit Sub
        If torr = 0.1 And torr4 = 0.1 Then Exit Sub
        Label32.Text = Format(torr, "0.0E+00")

        DspTorr_ordr(torr, torr4)

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        'Torr low range+
        Dim torr As Double = Label12.Text
        Dim torr1 As Double = Label32.Text

        torr = torr * 10.0
        If torr = 10 Then Exit Sub                          '2022-10-19 Changed graph upper limit from 1 torr to 10 torr
        If torr = 1 And torr1 = 1 Then Exit Sub             '2022-10-19 Changed graph upper limit from 1 torr to 10 torr
        If torr = 0.1 And torr1 = 0.1 Then Exit Sub
        If torr = 0.01 And torr1 = 0.01 Then Exit Sub
        If torr = 0.001 And torr1 = 0.001 Then Exit Sub
        Label12.Text = Format(torr, "0.0E+00")

        DspTorr_ordr(torr1, torr)

    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        'Torr low range-
        Dim torr As Double = Label12.Text
        Dim torr1 As Double = Label32.Text

        torr = torr / 10.0
        If torr < 0.0001 Then Exit Sub
        Label12.Text = Format(torr, "0.0E+00")

        DspTorr_ordr(torr1, torr)

    End Sub


End Class


