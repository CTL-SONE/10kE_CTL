Public Class frmMainte

    '**************************************************************
    '       SDS6K-10kE MAINTENANCE Ver1.01.0.0
    '                     2024/06/11
    '**************************************************************
    'V4.04をベースに、SAIREM 10kW用に修正
    'FS4～6の追加とMW出力最低値を1000Wに変更
    'Ver1.00.0.2 20231106 MW min power change
    'Ver1.00.0.3 20231107 When PLC WRITE is clicked, 
    'Ver1.00.0.4 20231129 - Kajino - To match the format with the current parameter file "CtlVsPrm.txt" in frmMainte2. Disable Maximize of form.
    'Ver1.01.0.0 20240611 - Sone - Added the ability to change the initial position of stubs on the maintenance screen. In addition, the initial position can be moved by pressing the button


    'Gas Parameter
    Structure GasParameter
        Dim Name() As String
        Dim Fs() As Double          '[SCCM]
        Dim Cf() As Double
        Dim Decmals() As Integer    '[0-3]
        Dim Tol() As Double          '[SCCM]
        Dim DevTime() As Double      '[sec]
        Dim TooLong() As Double      '[sec]
    End Structure

    'Micro Wave Parameter
    Structure MwParameter
        Dim Max As Long           '[W]
        Dim Ramp As Long          '[W/sec]
        Dim Tolerance As Long     '[W]
        Dim DevTime As Long       '[sec]
        Dim TooLong As Long       '[sec]
        Dim RefOvrTime As Long    '[sec]    
    End Structure

    'Micro Wave Alarm
    Structure MwAlarm
        Dim HigWarSP As Integer
        Dim HigWarDT As Long
        Dim HigAlmSP As Integer
        Dim HigAlmDT As Long
        Dim LowWarSP As Integer
        Dim LowWarDT As Long
        Dim LowAlmSP As Integer
        Dim LowAlmDT As Long
    End Structure

    'Pressure Parameter
    Structure PresParameter
        Dim ProcLimit As Long     '[Torr]
        Dim Atm As Long           '[Torr]
        Dim Tolerance As Single   '[Torr]
        Dim DevTime As Long       '[sec]
        Dim TooLong As Long       '[sec]
        Dim ProcWarning As Long   '[Torr]
    End Structure

    'Plenum Parameter
    Structure PlnmParameter
        Dim Tolerance As Single  '[Torr]
        Dim Fs As Long       '[Torr] '23/3/13 Sone PLENのFull Scale追加
    End Structure

    'PyroMeter Parameter
    Structure PyroParameter
        Dim RangeL As Long        '[DegC]
        Dim RangeH As Long        '[DegC]
    End Structure

    'PyroMeter Parameter
    Structure PyroAlarm
        Dim HigWarSP As Integer
        Dim HigWarDT As Long
        Dim HigAlmSP As Integer
        Dim HigAlmDT As Long
        Dim LowWarSP As Integer
        Dim LowWarDT As Long
        Dim LowAlmSP As Integer
        Dim LowAlmDT As Long
    End Structure

    'VentCycle Parameter
    Structure VentParameter
        Dim Low As Long           '[Torr]
        Dim High As Long          '[Torr]
    End Structure

    'Rough Parameter
    Structure RoughParameter
        Dim FullRoughPoint As Long    '[Torr]
        Dim KeepTim_aftrRough As Long '[sec]
        Dim SoftBuz As Long           '[sec]
        Dim RoughBuz As Long          '[sec]
    End Structure

    'I/L Disable Parameter
    Structure ILdisableParameter
        Dim GasManifold As Long  '0=Normal / 1=disable
        Dim O2_H2 As Long        '0=Normal / 1=disable

    End Structure

    'THR PID Parameter
    Structure ThrPidParameter
        Dim Time As Long          '[10msec]
        Dim P As Long             '[1-50000]
        Dim I As Long             '[1-30000]
        Dim D As Long             '[0-30000]
        Dim DeltaLimit As Long    '[1-65535]
    End Structure

    'System Type Parameter
    Structure SystemType
        Dim Tmp As Long          '0=Normal / 1=TMP
        Dim MwDevice As Long     '0=mks / 1=DAIHEN / 2=Muegge
    End Structure

    'Water Parameter
    Structure WaterParameter
        Dim FlowFs1 As Double
        Dim FlowFs2 As Double
        Dim FlowFs3 As Double
        Dim FlowFs4 As Double 'FS4,5追加　22/02/22
        Dim FlowFs5 As Double
        Dim FlowFs6 As Double 'FS6追加　23/10/04
        Dim TempFs1 As Double
        Dim TempFs2 As Double
        Dim TempFs3 As Double
        Dim TempFs4 As Double 'FS4,5追加　22/02/22
        Dim TempFs5 As Double
        Dim TempFs6 As Double 'FS6追加　23/10/04
    End Structure

    'Water Alarm
    Structure WatrAlarm
        Dim LowFlw_WarSP As Double
        Dim LowFlw_WarDT As Long
        Dim LowFlw_AlmSP As Double
        Dim LowFlw_AlmDT As Long
        Dim HigTmp_WarSP As Double
        Dim HigTmp_WarDT As Long
        Dim HigTmp_AlmSP As Double
        Dim HigTmp_AlmDT As Long
        Dim LowTmp_WarSP As Double
        Dim LowTmp_WarDT As Long
        Dim LowTmp_AlmSP As Double
        Dim LowTmp_AlmDT As Long
    End Structure

    Structure AutoLeakRateRecord
        Dim RoughPoint As Integer
        Dim VacumTime As Integer
    End Structure



    'Safty Area
    Structure SaftyAeraParameter
        Dim SaftyArea As Long    '0=Normal / 1=Off
        Dim DMval As Long        '2023/4/19     PLC DM119 value (Safety area is DM119.0)
    End Structure

    'Autotuner Stub 1,2,3,4 Position 0611
    Structure AutotunerStubPosition
        Dim StubPos14 As Integer
        Dim StubPos23 As Integer
    End Structure

    Structure WaveLogger
        Dim Window As Integer
    End Structure

    Const PATH As String = "C:\SDS6NETSYS\"
    Const PARAMFILE_NAM As String = "Param.txt"
    Public Const LGMD_PLC As String = "47000"
    Public Const SUPRVISR = 1
    Public Const OPRATR = 0

    Public RdComp As Boolean
    Public WrComp As Boolean
    Dim VonCLR As System.Drawing.Color = Color.Red          'Valve ON color
    Dim VofCLR As System.Drawing.Color = Color.Black        'Valve OFF color
    Dim DonCLR As System.Drawing.Color = Color.Red          'Device ON color
    Dim DofCLR As System.Drawing.Color = Color.Yellow       'Device OFF color

    Dim GasName(6) As String
    Dim SysType As Integer = 0    '0:Normal/1:TMP
    Dim MwF As Single
    Dim MwR As Single

    Dim Dgstr(3) As String

    Public VerInfo As String = ""
    'Dim NowVersion As String = ""

    'Parameter
    Dim GasPrm As GasParameter
    Dim MwPrm As MwParameter
    Dim PrsPrm As PresParameter
    Dim PlnmPrm As PlnmParameter
    Dim PyroPrm As PyroParameter
    Dim VentPrm As VentParameter
    Dim RoughPrm As RoughParameter
    Dim I_LPrm As ILdisableParameter
    Dim ThrPidPrm As ThrPidParameter
    Dim SysTyp As SystemType
    Dim WtrPrm As WaterParameter

    Dim SAPrm As SaftyAeraParameter

    'Alarm   *Append 20210531
    Public MwAlm As MwAlarm
    Public WtrAlm_FS1 As WatrAlarm
    Public WtrAlm_FS2 As WatrAlarm
    Public WtrAlm_FS3 As WatrAlarm
    Public WtrAlm_FS4 As WatrAlarm 'FS4,5追加　22/02/22
    Public WtrAlm_FS5 As WatrAlarm
    Public WtrAlm_FS6 As WatrAlarm 'FS6追加　23/10/04
    Public PyrAlm As PyroAlarm
    Public AutLekrateRec As AutoLeakRateRecord

    'Stub Position追加 24/06/11
    Public ATStubPos As AutotunerStubPosition

    'Private Height_Mes As Integer
    Dim Height_mes As Integer
    Dim Height_act As Single
    Dim Height_time As Single
    Dim LoginMode As Integer

    Dim PLC_Connected As Boolean    'Append 2022/05/29



    Private Sub frmMainte_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        '//////// Initialize ///////

        ''Forbid Multi Execution
        'If Diagnostics.Process.GetProcessesByName(Diagnostics.Process.GetCurrentProcess.ProcessName).Length > 1 Then
        '    Application.Exit()
        'End If

        '-------- Rev.2022/02
        'IP and Version of PLC are stored in HKEY_CURRENT_USER\SoftWare\SDS\PLC
        AxDBCommManager1.Peer = RgsPlcIp() & ":8500"    'default 192.168.3.13 & port 8500
        AxDBCommManager1.PLC = CInt(RgsPlcSr())         'default 515 = KV-5500/5000/3000

        'Try
        '    AxDBCommManager1.Connect()
        'Catch ex As Exception
        '    MessageBox.Show(ex.Message, "COM+")
        '    Exit Sub
        'End Try

        PLC_Connected = AxDBCommManager1_Connection_Check()
        If PLC_Connected = False Then
            MessageBox.Show("PLC not found !")
            Exit Sub
        End If

        Dim rgs As String = RgsR()

        LoginMode = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000XYM_D, LGMD_PLC)
        If LoginMode = OPRATR And rgs = "0" Then
            MessageBox.Show("Operator can not open !")
            End
        End If


        Dgstr(0) = "0"
        Dgstr(1) = "0.0"
        Dgstr(2) = "0.00"
        Dgstr(3) = "0.000"

        'frmPassWord.ShowDialog()

        ReDim GasPrm.Name(5)
        ReDim GasPrm.Fs(5)
        ReDim GasPrm.Cf(5)
        ReDim GasPrm.Decmals(5)
        ReDim GasPrm.Tol(5)
        ReDim GasPrm.DevTime(5)
        ReDim GasPrm.TooLong(5)


        With GasPrm
            .Name(0) = "H2(1000)"
            .Name(1) = "CH4(100)"
            .Name(2) = "H2(10)"
            .Name(3) = "O2(20)"
            .Name(4) = "Ar(100)"
            .Name(5) = "TMB(2)"

            .Fs(0) = 1000
            .Fs(1) = 100
            .Fs(2) = 20
            .Fs(3) = 20
            .Fs(4) = 100
            .Fs(5) = 2

            .Cf(0) = 1
            .Cf(1) = 1
            .Cf(2) = 1
            .Cf(3) = 1
            .Cf(4) = 1
            .Cf(5) = 1

            .Decmals(0) = 1
            .Decmals(1) = 1
            .Decmals(2) = 2
            .Decmals(3) = 2
            .Decmals(4) = 2
            .Decmals(5) = 2

            .Tol(0) = 1
            .Tol(1) = 1
            .Tol(2) = 1
            .Tol(3) = 1
            .Tol(4) = 1
            .Tol(5) = 0.02

            .DevTime(0) = 10
            .DevTime(1) = 10
            .DevTime(2) = 10
            .DevTime(3) = 10
            .DevTime(4) = 10
            .DevTime(5) = 10

            .TooLong(0) = 30
            .TooLong(1) = 30
            .TooLong(2) = 30
            .TooLong(3) = 30
            .TooLong(4) = 30
            .TooLong(5) = 30
        End With
        With MwPrm
            .Max = 6000
            .Ramp = 10
            .Tolerance = 100
            .DevTime = 600
            .TooLong = 600
            .RefOvrTime = 20
        End With
        With PrsPrm
            .ProcLimit = 220 'Sone Setup change
            .Atm = 740
            .Tolerance = 1
            .DevTime = 600
            .TooLong = 600
            .ProcWarning = 20 'Sone Setup change
        End With
        With PlnmPrm
            .Tolerance = 1
            .Fs = 1000 '23/3/13 Sone PLENのFull Scale追加
        End With
        With PyroPrm
            .RangeL = 475
            .RangeH = 1475
        End With
        With VentPrm
            .Low = 50
            .High = 300
        End With
        With RoughPrm
            .FullRoughPoint = 5
            .KeepTim_aftrRough = 100
            .SoftBuz = 600
            .RoughBuz = 600
        End With
        With I_LPrm
            .GasManifold = 0
            .O2_H2 = 0
        End With
        With ThrPidPrm
            .Time = 50
            .P = 50000
            .I = 100
            .D = 4000
            .DeltaLimit = 15000
        End With
        With SysTyp
            .Tmp = 1
            .MwDevice = 2
        End With
        With WtrPrm
            .FlowFs1 = 1
            .FlowFs2 = 1
            .FlowFs3 = 1
            .FlowFs4 = 1            'FS4,5追加　22/02/22
            .FlowFs5 = 1
            .FlowFs6 = 1            'FS6追加　23/10/04
            .TempFs1 = 50
            .TempFs2 = 50
            .TempFs3 = 50
            .TempFs4 = 50
            .TempFs5 = 50           'FS4,5追加　22/02/22
            .TempFs6 = 50           'FS6追加　23/10/04
        End With
        With SAPrm
            .SaftyArea = 0
        End With

        With AutLekrateRec
            .RoughPoint = 10    '10Torr
            .VacumTime = 20     '20min
        End With

        Dim i As Integer = 0
        Dim s As String

        'Me.Text = Me.Text & " " & NowVersion
        s = Dir(PATH & PARAMFILE_NAM) 'Param.txtの確認
        If s <> PARAMFILE_NAM Then
            s = Dir(PATH & "\*.*")    '*Append 2014/05/28
            If s = "" Then
                'MkDir(PATH)
            End If
            SysPrm_save()
        Else
            SysPrm_read()
        End If

        DspSysprm()

        'height

        Height_act = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6000") / 100
        TextBox89.Text = Format(Height_act, "0.00")

        Height_time = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6010") / 10
        TextBox88.Text = Format(Height_time, "0.0")

        AxDBTriggerManager1.Active = True
        AxDBTriggerManager2.Active = True

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

    Private Sub frmMainte_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        AxDBCommManager1.Disconnect()
        AxDBTriggerManager1.Active = False
    End Sub



    Private Sub DspSysprm()
        'Display of System Parameter

        TextBox46.Text = VerInfo

        With GasPrm
            TextBox9.Text = .Name(0)
            Label17.Text = TextBox9.Text
            TextBox11.Text = Format(.Fs(0), Dgstr(GasPrm.Decmals(0)))
            TextBox76.Text = Format(.Cf(0), "0.00")
            TextBox12.Text = .Decmals(0)
            TextBox13.Text = Format(.Tol(0), Dgstr(GasPrm.Decmals(0)))
            TextBox14.Text = .DevTime(0)
            TextBox15.Text = .TooLong(0)

            TextBox21.Text = .Name(1)
            Label18.Text = TextBox21.Text
            TextBox20.Text = Format(.Fs(1), Dgstr(GasPrm.Decmals(1)))
            TextBox77.Text = Format(.Cf(1), "0.00")
            TextBox19.Text = .Decmals(1)
            TextBox18.Text = Format(.Tol(1), Dgstr(GasPrm.Decmals(1)))
            TextBox17.Text = .DevTime(1)
            TextBox16.Text = .TooLong(1)

            TextBox27.Text = .Name(2)
            Label19.Text = TextBox27.Text
            TextBox26.Text = Format(.Fs(2), Dgstr(GasPrm.Decmals(2)))
            TextBox78.Text = Format(.Cf(2), "0.00")
            TextBox25.Text = .Decmals(2)
            TextBox24.Text = Format(.Tol(2), Dgstr(GasPrm.Decmals(2)))
            TextBox23.Text = .DevTime(2)
            TextBox22.Text = .TooLong(2)

            TextBox33.Text = .Name(3)
            Label20.Text = TextBox33.Text
            TextBox32.Text = Format(.Fs(3), Dgstr(GasPrm.Decmals(3)))
            TextBox79.Text = Format(.Cf(3), "0.00")
            TextBox31.Text = .Decmals(3)
            TextBox30.Text = Format(.Tol(3), Dgstr(GasPrm.Decmals(3)))
            TextBox29.Text = .DevTime(3)
            TextBox28.Text = .TooLong(3)

            TextBox39.Text = .Name(4)
            Label21.Text = TextBox39.Text
            TextBox38.Text = Format(.Fs(4), Dgstr(GasPrm.Decmals(4)))
            TextBox80.Text = Format(.Cf(4), "0.00")
            TextBox37.Text = .Decmals(4)
            TextBox36.Text = Format(.Tol(4), Dgstr(GasPrm.Decmals(4)))
            TextBox35.Text = .DevTime(4)
            TextBox34.Text = .TooLong(4)

            TextBox45.Text = .Name(5)
            Label22.Text = TextBox45.Text
            TextBox44.Text = Format(.Fs(5), Dgstr(GasPrm.Decmals(5)))
            TextBox81.Text = Format(.Cf(5), "0.00")
            TextBox43.Text = .Decmals(5)
            TextBox42.Text = Format(.Tol(5), Dgstr(GasPrm.Decmals(5)))
            TextBox41.Text = .DevTime(5)
            TextBox40.Text = .TooLong(5)
        End With

        With MwPrm
            TextBox47.Text = .Max
            TextBox48.Text = .Ramp
            TextBox49.Text = .Tolerance
            TextBox50.Text = .DevTime
            TextBox51.Text = .TooLong
            TextBox52.Text = .RefOvrTime
        End With

        With PrsPrm
            TextBox57.Text = .ProcLimit
            TextBox56.Text = .Atm
            TextBox55.Text = Format(.Tolerance, "0.0")
            TextBox54.Text = .DevTime
            TextBox53.Text = .TooLong
            TextBox84.Text = .ProcWarning '2022/7/12 Sone Add
        End With

        With PlnmPrm
            TextBox58.Text = Format(.Tolerance, "0.0")
            TextBox85.Text = Format(.Fs, "0") '23/3/13 Sone PLENのFull Scale追加
        End With

        With PyroPrm
            TextBox60.Text = .RangeL
            TextBox59.Text = .RangeH
        End With

        With VentPrm
            TextBox62.Text = .Low
            TextBox61.Text = .High
        End With

        With RoughPrm
            TextBox64.Text = .FullRoughPoint
            TextBox63.Text = .KeepTim_aftrRough
            TextBox66.Text = .SoftBuz
            TextBox65.Text = .RoughBuz
        End With

        With I_LPrm
            TextBox68.Text = .GasManifold
            TextBox67.Text = .O2_H2
        End With

        With ThrPidPrm
            TextBox72.Text = .Time
            TextBox71.Text = .P
            TextBox70.Text = .I
            TextBox69.Text = .D
            TextBox73.Text = .DeltaLimit
        End With

        With SysTyp
            TextBox75.Text = .Tmp
            TextBox74.Text = .MwDevice
        End With

        With SAPrm
            TextBox90.Text = .SaftyArea
        End With

        With AutLekrateRec
            TextBox82.Text = .RoughPoint
            TextBox83.Text = .VacumTime
        End With

        'With MwAlm
        '    frmMainte2.TextBox1.Text = .HigWarSP
        '    frmMainte2.TextBox5.Text = .HigWarDT
        '    frmMainte2.TextBox2.Text = .HigAlmSP
        '    frmMainte2.TextBox6.Text = .HigAlmDT

        '    frmMainte2.TextBox3.Text = .LowWarSP
        '    frmMainte2.TextBox7.Text = .LowWarDT
        '    frmMainte2.TextBox4.Text = .LowAlmSP
        '    frmMainte2.TextBox8.Text = .LowAlmDT
        'End With

        'With WtrAlm_FS1
        '    frmMainte2.TextBox16.Text = .LowFlw_WarSP
        '    frmMainte2.TextBox12.Text = .LowFlw_WarDT
        '    frmMainte2.TextBox15.Text = .LowFlw_AlmSP
        '    frmMainte2.TextBox11.Text = .LowFlw_AlmDT

        '    frmMainte2.TextBox14.Text = .HigTmp_WarSP
        '    frmMainte2.TextBox10.Text = .HigTmp_WarDT
        '    frmMainte2.TextBox13.Text = .HigTmp_AlmSP
        '    frmMainte2.TextBox9.Text = .HigTmp_AlmDT

        '    frmMainte2.TextBox20.Text = .LowTmp_WarSP
        '    frmMainte2.TextBox18.Text = .LowTmp_WarDT
        '    frmMainte2.TextBox19.Text = .LowTmp_AlmSP
        '    frmMainte2.TextBox17.Text = .LowTmp_AlmDT
        'End With

        'With WtrAlm_FS2
        '    frmMainte2.TextBox32.Text = .LowFlw_WarSP
        '    frmMainte2.TextBox28.Text = .LowFlw_WarDT
        '    frmMainte2.TextBox31.Text = .LowFlw_AlmSP
        '    frmMainte2.TextBox27.Text = .LowFlw_AlmDT

        '    frmMainte2.TextBox30.Text = .HigTmp_WarSP
        '    frmMainte2.TextBox26.Text = .HigTmp_WarDT
        '    frmMainte2.TextBox29.Text = .HigTmp_AlmSP
        '    frmMainte2.TextBox25.Text = .HigTmp_AlmDT

        '    frmMainte2.TextBox24.Text = .LowTmp_WarSP
        '    frmMainte2.TextBox22.Text = .LowTmp_WarDT
        '    frmMainte2.TextBox23.Text = .LowTmp_AlmSP
        '    frmMainte2.TextBox21.Text = .LowTmp_AlmDT
        'End With

        'With WtrAlm_FS3
        '    frmMainte2.TextBox44.Text = .LowFlw_WarSP
        '    frmMainte2.TextBox40.Text = .LowFlw_WarDT
        '    frmMainte2.TextBox43.Text = .LowFlw_AlmSP
        '    frmMainte2.TextBox39.Text = .LowFlw_AlmDT

        '    frmMainte2.TextBox42.Text = .HigTmp_WarSP
        '    frmMainte2.TextBox38.Text = .HigTmp_WarDT
        '    frmMainte2.TextBox41.Text = .HigTmp_AlmSP
        '    frmMainte2.TextBox37.Text = .HigTmp_AlmDT

        '    frmMainte2.TextBox36.Text = .LowTmp_WarSP
        '    frmMainte2.TextBox34.Text = .LowTmp_WarDT
        '    frmMainte2.TextBox35.Text = .LowTmp_AlmSP
        '    frmMainte2.TextBox33.Text = .LowTmp_AlmDT
        'End With

        'With PyrAlm
        '    frmMainte2.TextBox52.Text = .HigWarSP
        '    frmMainte2.TextBox48.Text = .HigWarDT
        '    frmMainte2.TextBox51.Text = .HigAlmSP
        '    frmMainte2.TextBox47.Text = .HigAlmDT

        '    frmMainte2.TextBox50.Text = .LowWarSP
        '    frmMainte2.TextBox46.Text = .LowWarDT
        '    frmMainte2.TextBox49.Text = .LowAlmSP
        '    frmMainte2.TextBox45.Text = .LowAlmDT
        'End With

    End Sub

    Private Sub CollectInpPrm()
        'Collect inputed System Parameter

        VerInfo = TextBox46.Text

        With GasPrm
            .Name(0) = TextBox9.Text
            .Fs(0) = CDbl(TextBox11.Text)
            .Cf(0) = CDbl(TextBox76.Text)
            .Decmals(0) = CInt(TextBox12.Text)
            .Tol(0) = CDbl(TextBox13.Text)
            .DevTime(0) = CDbl(TextBox14.Text)
            .TooLong(0) = CDbl(TextBox15.Text)

            .Name(1) = TextBox21.Text
            .Fs(1) = CDbl(TextBox20.Text)
            .Cf(1) = CDbl(TextBox77.Text)
            .Decmals(1) = CInt(TextBox19.Text)
            .Tol(1) = CDbl(TextBox18.Text)
            .DevTime(1) = CDbl(TextBox17.Text)
            .TooLong(1) = CDbl(TextBox16.Text)

            .Name(2) = TextBox27.Text
            .Fs(2) = CDbl(TextBox26.Text)
            .Cf(2) = CDbl(TextBox78.Text)
            .Decmals(2) = CInt(TextBox25.Text)
            .Tol(2) = CDbl(TextBox24.Text)
            .DevTime(2) = CDbl(TextBox23.Text)
            .TooLong(2) = CDbl(TextBox22.Text)

            .Name(3) = TextBox33.Text
            .Fs(3) = CDbl(TextBox32.Text)
            .Cf(3) = CDbl(TextBox79.Text)
            .Decmals(3) = CInt(TextBox31.Text)
            .Tol(3) = CDbl(TextBox30.Text)
            .DevTime(3) = CDbl(TextBox29.Text)
            .TooLong(3) = CDbl(TextBox28.Text)

            .Name(4) = TextBox39.Text
            .Fs(4) = CDbl(TextBox38.Text)
            .Cf(4) = CDbl(TextBox80.Text)
            .Decmals(4) = CInt(TextBox37.Text)
            .Tol(4) = CDbl(TextBox36.Text)
            .DevTime(4) = CDbl(TextBox35.Text)
            .TooLong(4) = CDbl(TextBox34.Text)

            .Name(5) = TextBox45.Text
            .Fs(5) = CDbl(TextBox44.Text)
            .Cf(5) = CDbl(TextBox81.Text)
            .Decmals(5) = CInt(TextBox43.Text)
            .Tol(5) = CDbl(TextBox42.Text)
            .DevTime(5) = CDbl(TextBox41.Text)
            .TooLong(5) = CDbl(TextBox40.Text)
        End With

        With MwPrm
            .Max = CLng(TextBox47.Text)
            .Ramp = CLng(TextBox48.Text)
            .Tolerance = CLng(TextBox49.Text)
            .DevTime = CLng(TextBox50.Text)
            .TooLong = CLng(TextBox51.Text)
            .RefOvrTime = CLng(TextBox52.Text)
        End With

        With PrsPrm
            .ProcLimit = CLng(TextBox57.Text)
            .Atm = CLng(TextBox56.Text)
            .Tolerance = CSng(TextBox55.Text)
            .DevTime = CLng(TextBox54.Text)
            .TooLong = CLng(TextBox53.Text)
            .ProcWarning = CLng(TextBox84.Text) '2022/7/12 Sone Add
        End With

        With PlnmPrm
            .Tolerance = CSng(TextBox58.Text)
            .Fs = CSng(TextBox85.Text)            '23/3/13 Sone PLENのFull Scale追加
        End With

        With PyroPrm
            .RangeL = CLng(TextBox60.Text)
            .RangeH = CLng(TextBox59.Text)
        End With

        With VentPrm
            .Low = CLng(TextBox62.Text)
            .High = CLng(TextBox61.Text)
        End With

        With RoughPrm
            .FullRoughPoint = CLng(TextBox64.Text)
            .KeepTim_aftrRough = CLng(TextBox63.Text)
            .SoftBuz = CLng(TextBox66.Text)
            .RoughBuz = CLng(TextBox65.Text)
        End With

        With I_LPrm
            .GasManifold = CLng(TextBox68.Text)
            .O2_H2 = CLng(TextBox67.Text)
        End With

        With ThrPidPrm
            .Time = CLng(TextBox72.Text)
            .P = CLng(TextBox71.Text)
            .I = CLng(TextBox70.Text)
            .D = CLng(TextBox69.Text)
            .DeltaLimit = CLng(TextBox73.Text)
        End With

        With SysTyp
            .Tmp = CLng(TextBox75.Text)
            .MwDevice = CLng(TextBox74.Text)
        End With

        With SAPrm
            .SaftyArea = CLng(TextBox90.Text)
        End With

        With AutLekrateRec
            .RoughPoint = CInt(TextBox82.Text)
            .VacumTime = CInt(TextBox83.Text)
        End With

        'With MwAlm
        '    .HigWarSP = CInt(frmMainte2.TextBox1.Text)
        '    .HigWarDT = CLng(frmMainte2.TextBox5.Text)
        '    .HigAlmSP = CInt(frmMainte2.TextBox2.Text)
        '    .HigAlmDT = CLng(frmMainte2.TextBox6.Text)

        '    .LowWarSP = CInt(frmMainte2.TextBox3.Text)
        '    .LowWarDT = CLng(frmMainte2.TextBox7.Text)
        '    .LowAlmSP = CInt(frmMainte2.TextBox4.Text)
        '    .LowAlmDT = CLng(frmMainte2.TextBox8.Text)
        'End With

        'With WtrAlm_FS1
        '    .LowFlw_WarSP = CInt(frmMainte2.TextBox16.Text)
        '    .LowFlw_WarDT = CLng(frmMainte2.TextBox12.Text)
        '    .LowFlw_AlmSP = CInt(frmMainte2.TextBox15.Text)
        '    .LowFlw_AlmDT = CLng(frmMainte2.TextBox11.Text)

        '    .HigTmp_WarSP = CInt(frmMainte2.TextBox14.Text)
        '    .HigTmp_WarDT = CLng(frmMainte2.TextBox10.Text)
        '    .HigTmp_AlmSP = CInt(frmMainte2.TextBox13.Text)
        '    .HigTmp_AlmDT = CLng(frmMainte2.TextBox9.Text)

        '    .LowTmp_WarSP = CInt(frmMainte2.TextBox20.Text)
        '    .LowTmp_WarDT = CLng(frmMainte2.TextBox18.Text)
        '    .LowTmp_AlmSP = CInt(frmMainte2.TextBox19.Text)
        '    .LowTmp_AlmDT = CLng(frmMainte2.TextBox17.Text)
        'End With

        'With WtrAlm_FS2
        '    .LowFlw_WarSP = CInt(frmMainte2.TextBox32.Text)
        '    .LowFlw_WarDT = CLng(frmMainte2.TextBox28.Text)
        '    .LowFlw_AlmSP = CInt(frmMainte2.TextBox31.Text)
        '    .LowFlw_AlmDT = CLng(frmMainte2.TextBox27.Text)

        '    .HigTmp_WarSP = CInt(frmMainte2.TextBox30.Text)
        '    .HigTmp_WarDT = CLng(frmMainte2.TextBox26.Text)
        '    .HigTmp_AlmSP = CInt(frmMainte2.TextBox29.Text)
        '    .HigTmp_AlmDT = CLng(frmMainte2.TextBox25.Text)

        '    .LowTmp_WarSP = CInt(frmMainte2.TextBox24.Text)
        '    .LowTmp_WarDT = CLng(frmMainte2.TextBox22.Text)
        '    .LowTmp_AlmSP = CInt(frmMainte2.TextBox23.Text)
        '    .LowTmp_AlmDT = CLng(frmMainte2.TextBox21.Text)
        'End With

        'With WtrAlm_FS3
        '    .LowFlw_WarSP = CInt(frmMainte2.TextBox44.Text)
        '    .LowFlw_WarDT = CLng(frmMainte2.TextBox40.Text)
        '    .LowFlw_AlmSP = CInt(frmMainte2.TextBox43.Text)
        '    .LowFlw_AlmDT = CLng(frmMainte2.TextBox39.Text)

        '    .HigTmp_WarSP = CInt(frmMainte2.TextBox42.Text)
        '    .HigTmp_WarDT = CLng(frmMainte2.TextBox38.Text)
        '    .HigTmp_AlmSP = CInt(frmMainte2.TextBox41.Text)
        '    .HigTmp_AlmDT = CLng(frmMainte2.TextBox37.Text)

        '    .LowTmp_WarSP = CInt(frmMainte2.TextBox36.Text)
        '    .LowTmp_WarDT = CLng(frmMainte2.TextBox34.Text)
        '    .LowTmp_AlmSP = CInt(frmMainte2.TextBox35.Text)
        '    .LowTmp_AlmDT = CLng(frmMainte2.TextBox33.Text)
        'End With

        'With PyrAlm
        '    .HigWarSP = CInt(frmMainte2.TextBox52.Text)
        '    .HigWarDT = CLng(frmMainte2.TextBox48.Text)
        '    .HigAlmSP = CInt(frmMainte2.TextBox51.Text)
        '    .HigAlmDT = CLng(frmMainte2.TextBox47.Text)

        '    .LowWarSP = CInt(frmMainte2.TextBox50.Text)
        '    .LowWarDT = CLng(frmMainte2.TextBox46.Text)
        '    .LowAlmSP = CInt(frmMainte2.TextBox49.Text)
        '    .LowAlmDT = CLng(frmMainte2.TextBox45.Text)
        'End With


    End Sub

    Private Sub SysPrm_save()
        'Save Parameters to System File
        Dim fnam As String = PATH & PARAMFILE_NAM
        Dim fno As Integer = FreeFile()
        Dim i As Integer

        Try
            FileOpen(fno, fnam, OpenMode.Output)

            WriteLine(fno, VerInfo)

            With GasPrm
                For i = 0 To 5
                    WriteLine(fno, .Name(i), .Fs(i), .Cf(i), .Decmals(i), .Tol(i), .DevTime(i), .TooLong(i))
                Next
            End With
            With MwPrm
                WriteLine(fno, .Max, .Ramp, .Tolerance, .DevTime, .TooLong, .RefOvrTime)
            End With
            With PrsPrm
                WriteLine(fno, .ProcLimit, .Atm, .Tolerance, .DevTime, .TooLong, .ProcWarning) '2022/7/12 Sone Add
            End With
            With PlnmPrm
                WriteLine(fno, .Tolerance, .Fs) '23/3/13 Sone PLENのFull Scale追加
            End With
            With PyroPrm
                WriteLine(fno, .RangeL, .RangeH)
            End With
            With VentPrm
                WriteLine(fno, .Low, .High)
            End With
            With RoughPrm
                WriteLine(fno, .FullRoughPoint, .KeepTim_aftrRough, .SoftBuz, .RoughBuz)
            End With
            With I_LPrm
                WriteLine(fno, .GasManifold, .O2_H2)
            End With
            With ThrPidPrm
                WriteLine(fno, .Time, .P, .I, .D, .DeltaLimit)
            End With
            With SysTyp
                WriteLine(fno, .Tmp, .MwDevice)
            End With

            With SAPrm
                WriteLine(fno, .SaftyArea)
            End With
            With AutLekrateRec
                WriteLine(fno, .RoughPoint, .VacumTime)
            End With

            With MwAlm
                WriteLine(fno, .HigWarSP, .HigWarDT, .HigAlmSP, .HigAlmDT, .LowWarSP, .LowWarDT, .LowAlmSP, .LowAlmDT)
            End With
            With WtrAlm_FS1
                WriteLine(fno, .LowFlw_WarSP, .LowFlw_WarDT, .LowFlw_AlmSP, .LowFlw_AlmDT, .HigTmp_WarSP,
                          .HigTmp_WarDT, .HigTmp_AlmSP, .HigTmp_AlmDT, .LowTmp_WarSP, .LowTmp_WarDT, .LowTmp_AlmSP, .LowTmp_AlmDT)
            End With
            With WtrAlm_FS2
                WriteLine(fno, .LowFlw_WarSP, .LowFlw_WarDT, .LowFlw_AlmSP, .LowFlw_AlmDT, .HigTmp_WarSP,
                          .HigTmp_WarDT, .HigTmp_AlmSP, .HigTmp_AlmDT, .LowTmp_WarSP, .LowTmp_WarDT, .LowTmp_AlmSP, .LowTmp_AlmDT)
            End With
            With WtrAlm_FS3
                WriteLine(fno, .LowFlw_WarSP, .LowFlw_WarDT, .LowFlw_AlmSP, .LowFlw_AlmDT, .HigTmp_WarSP,
                          .HigTmp_WarDT, .HigTmp_AlmSP, .HigTmp_AlmDT, .LowTmp_WarSP, .LowTmp_WarDT, .LowTmp_AlmSP, .LowTmp_AlmDT)
            End With
            'FS4,5追加　22/02/22
            With WtrAlm_FS4
                WriteLine(fno, .LowFlw_WarSP, .LowFlw_WarDT, .LowFlw_AlmSP, .LowFlw_AlmDT, .HigTmp_WarSP,
                          .HigTmp_WarDT, .HigTmp_AlmSP, .HigTmp_AlmDT, .LowTmp_WarSP, .LowTmp_WarDT, .LowTmp_AlmSP, .LowTmp_AlmDT)
            End With
            With WtrAlm_FS5
                WriteLine(fno, .LowFlw_WarSP, .LowFlw_WarDT, .LowFlw_AlmSP, .LowFlw_AlmDT, .HigTmp_WarSP,
                          .HigTmp_WarDT, .HigTmp_AlmSP, .HigTmp_AlmDT, .LowTmp_WarSP, .LowTmp_WarDT, .LowTmp_AlmSP, .LowTmp_AlmDT)
            End With
            '%%%%%%%%%%%%%%%%%%%%%%%%%%%
            'FS6追加　23/10/04
            With WtrAlm_FS6
                WriteLine(fno, .LowFlw_WarSP, .LowFlw_WarDT, .LowFlw_AlmSP, .LowFlw_AlmDT, .HigTmp_WarSP,
                          .HigTmp_WarDT, .HigTmp_AlmSP, .HigTmp_AlmDT, .LowTmp_WarSP, .LowTmp_WarDT, .LowTmp_AlmSP, .LowTmp_AlmDT)
            End With

            With PyrAlm
                'WriteLine(fno, .HigWarSP, .HigWarDT, .HigAlmSP, .HigAlmDT, .LowWarSP, .LowWarDT, .LowAlmSP, .LowAlmDT)
                WriteLine(fno, .HigWarDT, .HigAlmDT, .LowWarDT, .LowAlmDT)
            End With

            'Stub Initial Position追加　24/06/11
            With ATStubPos
                WriteLine(fno, .StubPos14, .StubPos23)
            End With

            FileClose()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
            FileClose()
        End Try
    End Sub

    Private Sub SysPrm_read()
        'Read Parameters frome System File
        Dim fnam As String = PATH & PARAMFILE_NAM
        Dim fno As Integer = FreeFile()
        Dim i As Integer

        Try
            FileOpen(fno, fnam, OpenMode.Input)

            Input(fno, VerInfo)
            With GasPrm

                For i = 0 To 5
                    Input(fno, .Name(i))
                    Input(fno, .Fs(i))
                    Input(fno, .Cf(i))
                    Input(fno, .Decmals(i))
                    Input(fno, .Tol(i))
                    Input(fno, .DevTime(i))
                    Input(fno, .TooLong(i))
                Next
            End With
            With MwPrm
                Input(fno, .Max)
                Input(fno, .Ramp)
                Input(fno, .Tolerance)
                Input(fno, .DevTime)
                Input(fno, .TooLong)
                Input(fno, .RefOvrTime)
            End With
            With PrsPrm
                Input(fno, .ProcLimit)
                Input(fno, .Atm)
                Input(fno, .Tolerance)
                Input(fno, .DevTime)
                Input(fno, .TooLong)
                Input(fno, .ProcWarning)
            End With
            With PlnmPrm
                Input(fno, .Tolerance)
                Input(fno, .Fs) '23/3/13 Sone PLENのFull Scale追加
            End With
            With PyroPrm
                Input(fno, .RangeL)
                Input(fno, .RangeH)
            End With
            With VentPrm
                Input(fno, .Low)
                Input(fno, .High)
            End With
            With RoughPrm
                Input(fno, .FullRoughPoint)
                Input(fno, .KeepTim_aftrRough)
                Input(fno, .SoftBuz)
                Input(fno, .RoughBuz)
            End With
            With I_LPrm
                Input(fno, .GasManifold)
                Input(fno, .O2_H2)

            End With
            With ThrPidPrm
                Input(fno, .Time)
                Input(fno, .P)
                Input(fno, .I)
                Input(fno, .D)
                Input(fno, .DeltaLimit)
            End With
            With SysTyp
                Input(fno, .Tmp)
                Input(fno, .MwDevice)
            End With

            'With WtrPrm
            '    Input(fno, .FlowFs1)
            '    Input(fno, .FlowFs2)
            '    Input(fno, .FlowFs3)
            '    Input(fno, .TempFs1)
            '    Input(fno, .TempFs2)
            '    Input(fno, .TempFs3)
            'End With

            With SAPrm
                Input(fno, .SaftyArea)
            End With

            With AutLekrateRec
                Input(fno, .RoughPoint)
                Input(fno, .VacumTime)
            End With

            With MwAlm
                Input(fno, .HigWarSP)
                Input(fno, .HigWarDT)
                Input(fno, .HigAlmSP)
                Input(fno, .HigAlmDT)

                Input(fno, .LowWarSP)
                Input(fno, .LowWarDT)
                Input(fno, .LowAlmSP)
                Input(fno, .LowAlmDT)
            End With

            With WtrAlm_FS1
                Input(fno, .LowFlw_WarSP)
                Input(fno, .LowFlw_WarDT)
                Input(fno, .LowFlw_AlmSP)
                Input(fno, .LowFlw_AlmDT)

                Input(fno, .HigTmp_WarSP)
                Input(fno, .HigTmp_WarDT)
                Input(fno, .HigTmp_AlmSP)
                Input(fno, .HigTmp_AlmDT)

                Input(fno, .LowTmp_WarSP)
                Input(fno, .LowTmp_WarDT)
                Input(fno, .LowTmp_AlmSP)
                Input(fno, .LowTmp_AlmDT)
            End With

            With WtrAlm_FS2
                Input(fno, .LowFlw_WarSP)
                Input(fno, .LowFlw_WarDT)
                Input(fno, .LowFlw_AlmSP)
                Input(fno, .LowFlw_AlmDT)

                Input(fno, .HigTmp_WarSP)
                Input(fno, .HigTmp_WarDT)
                Input(fno, .HigTmp_AlmSP)
                Input(fno, .HigTmp_AlmDT)

                Input(fno, .LowTmp_WarSP)
                Input(fno, .LowTmp_WarDT)
                Input(fno, .LowTmp_AlmSP)
                Input(fno, .LowTmp_AlmDT)
            End With

            With WtrAlm_FS3
                Input(fno, .LowFlw_WarSP)
                Input(fno, .LowFlw_WarDT)
                Input(fno, .LowFlw_AlmSP)
                Input(fno, .LowFlw_AlmDT)

                Input(fno, .HigTmp_WarSP)
                Input(fno, .HigTmp_WarDT)
                Input(fno, .HigTmp_AlmSP)
                Input(fno, .HigTmp_AlmDT)

                Input(fno, .LowTmp_WarSP)
                Input(fno, .LowTmp_WarDT)
                Input(fno, .LowTmp_AlmSP)
                Input(fno, .LowTmp_AlmDT)
            End With
            'FS4,5追加　22/02/22
            With WtrAlm_FS4
                Input(fno, .LowFlw_WarSP)
                Input(fno, .LowFlw_WarDT)
                Input(fno, .LowFlw_AlmSP)
                Input(fno, .LowFlw_AlmDT)

                Input(fno, .HigTmp_WarSP)
                Input(fno, .HigTmp_WarDT)
                Input(fno, .HigTmp_AlmSP)
                Input(fno, .HigTmp_AlmDT)

                Input(fno, .LowTmp_WarSP)
                Input(fno, .LowTmp_WarDT)
                Input(fno, .LowTmp_AlmSP)
                Input(fno, .LowTmp_AlmDT)
            End With

            With WtrAlm_FS5
                Input(fno, .LowFlw_WarSP)
                Input(fno, .LowFlw_WarDT)
                Input(fno, .LowFlw_AlmSP)
                Input(fno, .LowFlw_AlmDT)

                Input(fno, .HigTmp_WarSP)
                Input(fno, .HigTmp_WarDT)
                Input(fno, .HigTmp_AlmSP)
                Input(fno, .HigTmp_AlmDT)

                Input(fno, .LowTmp_WarSP)
                Input(fno, .LowTmp_WarDT)
                Input(fno, .LowTmp_AlmSP)
                Input(fno, .LowTmp_AlmDT)
            End With

            'FS6追加23/10/04
            With WtrAlm_FS6
                Input(fno, .LowFlw_WarSP)
                Input(fno, .LowFlw_WarDT)
                Input(fno, .LowFlw_AlmSP)
                Input(fno, .LowFlw_AlmDT)

                Input(fno, .HigTmp_WarSP)
                Input(fno, .HigTmp_WarDT)
                Input(fno, .HigTmp_AlmSP)
                Input(fno, .HigTmp_AlmDT)

                Input(fno, .LowTmp_WarSP)
                Input(fno, .LowTmp_WarDT)
                Input(fno, .LowTmp_AlmSP)
                Input(fno, .LowTmp_AlmDT)
            End With

            With PyrAlm
                'Input(fno, .HigWarSP)
                Input(fno, .HigWarDT)
                'Input(fno, .HigAlmSP)
                Input(fno, .HigAlmDT)
                'Input(fno, .LowWarSP)
                Input(fno, .LowWarDT)
                'Input(fno, .LowAlmSP)
                Input(fno, .LowAlmDT)
            End With

            '24/6/11 AT Stub Pos追加のため変更
            With ATStubPos
                Input(fno, .StubPos14)
                Input(fno, .StubPos23)
            End With

            FileClose()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
            FileClose()
        End Try

    End Sub


    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        'PARAMETER DATA FILE READ
        SysPrm_read()
        DspSysprm()
    End Sub


    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        'PARAMETER DATA FILE WRITE
        CollectInpPrm()
        SysPrm_save()
    End Sub

    Private Sub RdPrm_PLC()
        'PARAMETER DATA PLC READ
        Dim i As Integer
        Dim device(173) As DATABUILDERAXLibLB.DBDevice '23/10/4 159からFS6追加のため変更 '23/2/2 134からFS4,5追加のため変更 '24/6/11 171からAT Stub Pos追加のため変更
        Dim rdval As Long


        RdComp = False
        AxDBDeviceManager2.ReadAll()
        Do
            My.Application.DoEvents()
        Loop Until RdComp = True

        For i = 1 To 173 '23/10/4 159からFS6追加のため変更 '23/2/2 134からFS4,5追加のため変更 '24/6/11 171からAT Stub Pos追加のため変更
            device(i) = AxDBDeviceManager2.Devices.Item(i)
            rdval = device(i).ValueRead
            Select Case i
                Case 2 : GasPrm.Fs(0) = CDbl(rdval) / Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(0)))
                Case 4 : GasPrm.Fs(1) = CDbl(rdval) / Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(1)))
                Case 6 : GasPrm.Fs(2) = CDbl(rdval) / Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(2)))
                Case 8 : GasPrm.Fs(3) = CDbl(rdval) / Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(3)))
                Case 10 : GasPrm.Fs(4) = CDbl(rdval) / Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(4)))
                Case 12 : GasPrm.Fs(5) = CDbl(rdval) / Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(5)))
                Case 17 : PyroPrm.RangeL = rdval
                Case 18 : PyroPrm.RangeH = rdval
                Case 19 : MwPrm.Max = rdval
                'Case 20 : SAPrm.SaftyArea = rdval '2023/4/19 Sone-san
                Case 20
                    SAPrm.DMval = rdval                     'This is DM119
                    SAPrm.SaftyArea = SAPrm.DMval And &H1   'This is MD119.0
                Case 21 : GasPrm.DevTime(0) = CDbl(rdval)
                Case 22 : GasPrm.TooLong(0) = CDbl(rdval)
                Case 23 : GasPrm.DevTime(1) = CDbl(rdval)
                Case 24 : GasPrm.TooLong(1) = CDbl(rdval)
                Case 25 : GasPrm.DevTime(2) = CDbl(rdval)
                Case 26 : GasPrm.TooLong(2) = CDbl(rdval)
                Case 27 : GasPrm.DevTime(3) = CDbl(rdval)
                Case 28 : GasPrm.TooLong(3) = CDbl(rdval)
                Case 29 : GasPrm.DevTime(4) = CDbl(rdval)
                Case 30 : GasPrm.TooLong(4) = CDbl(rdval)
                Case 31 : GasPrm.DevTime(5) = CDbl(rdval)
                Case 32 : GasPrm.TooLong(5) = CDbl(rdval)
                Case 33 : PrsPrm.DevTime = rdval
                Case 34 : PrsPrm.TooLong = rdval
                Case 37 : MwPrm.DevTime = rdval
                Case 38 : MwPrm.TooLong = rdval
                Case 40 : MwPrm.RefOvrTime = rdval
                Case 41 : RoughPrm.SoftBuz = rdval
                Case 42 : RoughPrm.RoughBuz = rdval
                Case 43 : RoughPrm.FullRoughPoint = rdval / 10 'DM142
                Case 44 : VentPrm.Low = rdval / 10
                Case 45 : VentPrm.High = rdval / 10
                Case 47 : PlnmPrm.Tolerance = CSng(rdval) / 10
                Case 48 : PrsPrm.ProcLimit = rdval / 10
                Case 49 : PrsPrm.Atm = rdval / 10
                Case 51 : MwPrm.Ramp = 1000 '< -DM150 Is Powermin
                Case 52 : MwPrm.Tolerance = rdval
                Case 53 : PrsPrm.Tolerance = CSng(rdval) / 10
                Case 54 : I_LPrm.GasManifold = rdval
                Case 55 : I_LPrm.O2_H2 = rdval

                Case 57 : GasPrm.Decmals(0) = rdval
                Case 58 : GasPrm.Decmals(1) = rdval
                Case 59 : GasPrm.Decmals(2) = rdval
                Case 60 : GasPrm.Decmals(3) = rdval
                Case 61 : GasPrm.Decmals(4) = rdval
                Case 62 : GasPrm.Decmals(5) = rdval

                Case 63 : RoughPrm.KeepTim_aftrRough = rdval / 10

                'Case 66 : PyrAlm.HigWarSP = rdval
                'Case 67 : PyrAlm.HigAlmSP = rdval
                'Case 68 : PyrAlm.LowWarSP = rdval
                'Case 69 : PyrAlm.LowAlmSP = rdval
                Case 70 : PyrAlm.HigWarDT = rdval / 10
                Case 71 : PyrAlm.HigAlmDT = rdval / 10
                Case 72 : PyrAlm.LowWarDT = rdval / 10
                Case 73 : PyrAlm.LowAlmDT = rdval / 10

                Case 74 : WtrAlm_FS1.LowFlw_WarSP = rdval / 10
                Case 75 : WtrAlm_FS2.LowFlw_WarSP = rdval / 10
                Case 76 : WtrAlm_FS3.LowFlw_WarSP = rdval / 10

                Case 77 : WtrAlm_FS1.LowFlw_AlmSP = rdval / 10
                Case 78 : WtrAlm_FS2.LowFlw_AlmSP = rdval / 10
                Case 79 : WtrAlm_FS3.LowFlw_AlmSP = rdval / 10

                Case 80 : WtrAlm_FS1.HigTmp_WarSP = rdval / 10
                Case 81 : WtrAlm_FS2.HigTmp_WarSP = rdval / 10
                Case 82 : WtrAlm_FS3.HigTmp_WarSP = rdval / 10

                Case 83 : WtrAlm_FS1.HigTmp_AlmSP = rdval / 10
                Case 84 : WtrAlm_FS2.HigTmp_AlmSP = rdval / 10
                Case 85 : WtrAlm_FS3.HigTmp_AlmSP = rdval / 10

                Case 86 : WtrAlm_FS1.LowTmp_WarSP = rdval / 10
                Case 87 : WtrAlm_FS2.LowTmp_WarSP = rdval / 10
                Case 88 : WtrAlm_FS3.LowTmp_WarSP = rdval / 10

                Case 89 : WtrAlm_FS1.LowTmp_AlmSP = rdval / 10
                Case 90 : WtrAlm_FS2.LowTmp_AlmSP = rdval / 10
                Case 91 : WtrAlm_FS3.LowTmp_AlmSP = rdval / 10

                Case 92 : WtrAlm_FS1.LowFlw_WarDT = rdval / 10
                Case 93 : WtrAlm_FS2.LowFlw_WarDT = rdval / 10
                Case 94 : WtrAlm_FS3.LowFlw_WarDT = rdval / 10

                Case 95 : WtrAlm_FS1.LowFlw_AlmDT = rdval / 10
                Case 96 : WtrAlm_FS2.LowFlw_AlmDT = rdval / 10
                Case 97 : WtrAlm_FS3.LowFlw_AlmDT = rdval / 10

                Case 98 : WtrAlm_FS1.HigTmp_WarDT = rdval / 10
                Case 99 : WtrAlm_FS2.HigTmp_WarDT = rdval / 10
                Case 100 : WtrAlm_FS3.HigTmp_WarDT = rdval / 10

                Case 101 : WtrAlm_FS1.HigTmp_AlmDT = rdval / 10
                Case 102 : WtrAlm_FS2.HigTmp_AlmDT = rdval / 10
                Case 103 : WtrAlm_FS3.HigTmp_AlmDT = rdval / 10

                Case 104 : WtrAlm_FS1.LowTmp_WarDT = rdval / 10
                Case 105 : WtrAlm_FS2.LowTmp_WarDT = rdval / 10
                Case 106 : WtrAlm_FS3.LowTmp_WarDT = rdval / 10

                Case 107 : WtrAlm_FS1.LowTmp_AlmDT = rdval / 10
                Case 108 : WtrAlm_FS2.LowTmp_AlmDT = rdval / 10
                Case 109 : WtrAlm_FS3.LowTmp_AlmDT = rdval / 10

                Case 110 : MwAlm.HigWarSP = rdval
                Case 111 : MwAlm.HigAlmSP = rdval
                Case 112 : MwAlm.LowWarSP = rdval
                Case 113 : MwAlm.LowAlmSP = rdval

                Case 114 : MwAlm.HigWarDT = rdval / 10
                Case 115 : MwAlm.HigAlmDT = rdval / 10
                Case 116 : MwAlm.LowWarDT = rdval / 10
                Case 117 : MwAlm.LowAlmDT = rdval / 10

                'for the Future
                'Case 74 : GasPrm.Cf(0) = rdval / 100
                'Case 75 : GasPrm.Cf(1) = rdval / 100
                'Case 76 : GasPrm.Cf(2) = rdval / 100
                'Case 77 : GasPrm.Cf(3) = rdval / 100
                'Case 78 : GasPrm.Cf(4) = rdval / 100
                'Case 79 : GasPrm.Cf(5) = rdval / 100

                Case 121 : GasPrm.Tol(0) = CDbl(rdval) / CDbl(10000) * GasPrm.Fs(0)
                Case 122 : GasPrm.Tol(1) = CDbl(rdval) / CDbl(10000) * GasPrm.Fs(1)
                Case 123 : GasPrm.Tol(2) = CDbl(rdval) / CDbl(10000) * GasPrm.Fs(2)
                Case 124 : GasPrm.Tol(3) = CDbl(rdval) / CDbl(10000) * GasPrm.Fs(3)
                Case 125 : GasPrm.Tol(4) = CDbl(rdval) / CDbl(10000) * GasPrm.Fs(4)
                Case 126 : GasPrm.Tol(5) = CDbl(rdval) / CDbl(10000) * GasPrm.Fs(5)

                Case 127 : ThrPidPrm.Time = rdval
                Case 128 : ThrPidPrm.P = rdval
                Case 129 : ThrPidPrm.I = rdval
                Case 130 : ThrPidPrm.D = rdval
                Case 131 : ThrPidPrm.DeltaLimit = rdval

                Case 132 : AutLekrateRec.RoughPoint = rdval / 10
                Case 133 : AutLekrateRec.VacumTime = rdval

                Case 134 : PrsPrm.ProcWarning = rdval / 10 'Sone Scale change

                    '23/2/2 FS4,5追加
                Case 135 : WtrAlm_FS4.LowFlw_WarSP = rdval / 10
                Case 136 : WtrAlm_FS5.LowFlw_WarSP = rdval / 10

                Case 137 : WtrAlm_FS4.LowFlw_AlmSP = rdval / 10
                Case 138 : WtrAlm_FS5.LowFlw_AlmSP = rdval / 10

                Case 139 : WtrAlm_FS4.HigTmp_WarSP = rdval / 10
                Case 140 : WtrAlm_FS5.HigTmp_WarSP = rdval / 10

                Case 141 : WtrAlm_FS4.HigTmp_AlmSP = rdval / 10
                Case 142 : WtrAlm_FS5.HigTmp_AlmSP = rdval / 10

                Case 143 : WtrAlm_FS4.LowTmp_WarSP = rdval / 10
                Case 144 : WtrAlm_FS5.LowTmp_WarSP = rdval / 10

                Case 145 : WtrAlm_FS4.LowTmp_AlmSP = rdval / 10
                Case 146 : WtrAlm_FS5.LowTmp_AlmSP = rdval / 10

                Case 147 : WtrAlm_FS4.LowFlw_WarDT = rdval / 10
                Case 148 : WtrAlm_FS5.LowFlw_WarDT = rdval / 10

                Case 149 : WtrAlm_FS4.LowFlw_AlmDT = rdval / 10
                Case 150 : WtrAlm_FS5.LowFlw_AlmDT = rdval / 10

                Case 151 : WtrAlm_FS4.HigTmp_WarDT = rdval / 10
                Case 152 : WtrAlm_FS5.HigTmp_WarDT = rdval / 10

                Case 153 : WtrAlm_FS4.HigTmp_AlmDT = rdval / 10
                Case 154 : WtrAlm_FS5.HigTmp_AlmDT = rdval / 10

                Case 155 : WtrAlm_FS4.LowTmp_WarDT = rdval / 10
                Case 156 : WtrAlm_FS5.LowTmp_WarDT = rdval / 10

                Case 157 : WtrAlm_FS4.LowTmp_AlmDT = rdval / 10
                Case 158 : WtrAlm_FS5.LowTmp_AlmDT = rdval / 10


                Case 159 : PlnmPrm.Fs = rdval / 10 '23/3/13 Sone PLENのFull Scale追加

               '23/10/04 FS6追加
                Case 160 : WtrAlm_FS6.LowFlw_WarSP = rdval / 10
                Case 161 : WtrAlm_FS6.LowFlw_AlmSP = rdval / 10

                Case 162 : WtrAlm_FS6.HigTmp_WarSP = rdval / 10
                Case 163 : WtrAlm_FS6.HigTmp_AlmSP = rdval / 10

                Case 164 : WtrAlm_FS6.LowTmp_WarSP = rdval / 10
                Case 165 : WtrAlm_FS6.LowTmp_AlmSP = rdval / 10

                Case 166 : WtrAlm_FS6.LowFlw_WarDT = rdval / 10
                Case 167 : WtrAlm_FS6.LowFlw_AlmDT = rdval / 10

                Case 168 : WtrAlm_FS6.HigTmp_WarDT = rdval / 10
                Case 169 : WtrAlm_FS6.HigTmp_AlmDT = rdval / 10

                Case 170 : WtrAlm_FS6.LowTmp_WarDT = rdval / 10
                Case 171 : WtrAlm_FS6.LowTmp_AlmDT = rdval / 10

                     '24/6/11 171からAT Stub Pos追加のため変更
                Case 172 : ATStubPos.StubPos14 = rdval
                Case 173 : ATStubPos.StubPos23 = rdval

            End Select
        Next
    End Sub

    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        'PARAMETER DATA PLC READ

        RdPrm_PLC()
        DspSysprm()

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        'PARAMETER DATA PLC WRITE
        Dim i As Integer
        Dim device(173) As DATABUILDERAXLibLB.DBDevice  '23/10/4 159からFS6追加のため変更 '23/2/2 134からFS4,5追加のため変更 '24/6/11 171からAT Stub Pos追加のため変更
        Dim wrval As Long

        '-----------2023/11/07 kajino PLC WRITE前に、現在の値を読み込み追加-------------
        Dim rdval As Long

        Dim device20 As DATABUILDERAXLibLB.DBDevice
        RdComp = False
        AxDBDeviceManager2.ReadAll()
        Do
            My.Application.DoEvents()
        Loop Until RdComp = True
        device20 = AxDBDeviceManager2.Devices.Item(20)
        rdval = device20.ValueRead

        '-------------------------------------

        CollectInpPrm()

        'These 4 Parameters are wrote on the Control panel
        PyrAlm.HigWarSP = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "165")
        PyrAlm.HigAlmSP = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "166")
        PyrAlm.LowWarSP = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "167")
        PyrAlm.LowAlmSP = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "168")

        'SAPrm.SaftyArea = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "119")

        For i = 1 To 173  '23/10/4 159からFS6追加のため変更 '23/2/2 134からFS4,5追加のため変更 159  '24/6/11 171からAT Stub Pos追加のため変更
            Select Case i
                Case 2 : wrval = CLng(GasPrm.Fs(0) * Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(0))))
                Case 4 : wrval = CLng(GasPrm.Fs(1) * Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(1))))
                Case 6 : wrval = CLng(GasPrm.Fs(2) * Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(2))))
                Case 8 : wrval = CLng(GasPrm.Fs(3) * Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(3))))
                Case 10 : wrval = CLng(GasPrm.Fs(4) * Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(4))))
                Case 12 : wrval = CLng(GasPrm.Fs(5) * Math.Pow(CDbl(10), CDbl(GasPrm.Decmals(5))))
                Case 17 : wrval = PyroPrm.RangeL
                Case 18 : wrval = PyroPrm.RangeH
                Case 19 : wrval = MwPrm.Max
                'Case 20 : wrval = SAPrm.SaftyArea          '2023/4/19 Sone-san
                Case 20
                    SAPrm.DMval = rdval                     'This is DM119　2023/11/07 PLC WRITE前に、現在の値を読み込み追加
                    If SAPrm.SaftyArea = 1 Then
                        wrval = SAPrm.DMval Or &H1          'DM119.0 = 1
                    Else
                        wrval = SAPrm.DMval And &H1E     'DM119.0 = 0 2023/11/07 PLC WRITEするとDM119が更新されるバグ修正

                    End If

                Case 21 : wrval = CLng(GasPrm.DevTime(0))
                Case 22 : wrval = CLng(GasPrm.TooLong(0))
                Case 23 : wrval = CLng(GasPrm.DevTime(1))
                Case 24 : wrval = CLng(GasPrm.TooLong(1))
                Case 25 : wrval = CLng(GasPrm.DevTime(2))
                Case 26 : wrval = CLng(GasPrm.TooLong(2))
                Case 27 : wrval = CLng(GasPrm.DevTime(3))
                Case 28 : wrval = CLng(GasPrm.TooLong(3))
                Case 29 : wrval = CLng(GasPrm.DevTime(4))
                Case 30 : wrval = CLng(GasPrm.TooLong(4))
                Case 31 : wrval = CLng(GasPrm.DevTime(5))
                Case 32 : wrval = CLng(GasPrm.TooLong(5))
                Case 33 : wrval = PrsPrm.DevTime
                Case 34 : wrval = PrsPrm.TooLong
                Case 37 : wrval = MwPrm.DevTime
                Case 38 : wrval = MwPrm.TooLong
                Case 40 : wrval = MwPrm.RefOvrTime
                Case 41 : wrval = RoughPrm.SoftBuz
                Case 42 : wrval = RoughPrm.RoughBuz
                Case 43 : wrval = RoughPrm.FullRoughPoint * 10 'DM142
                Case 44 : wrval = VentPrm.Low * 10
                Case 45 : wrval = VentPrm.High * 10
                Case 47 : wrval = PlnmPrm.Tolerance * 10
                Case 48 : wrval = PrsPrm.ProcLimit * 10
                Case 49 : wrval = PrsPrm.Atm * 10
                Case 51 : wrval = 1000 'MwPrm.Ramp <- DM150 is Powermin
                Case 52 : wrval = MwPrm.Tolerance
                Case 53 : wrval = PrsPrm.Tolerance * 10
                Case 54 : wrval = I_LPrm.GasManifold
                Case 55 : wrval = I_LPrm.O2_H2

                Case 57 : wrval = GasPrm.Decmals(0)
                Case 58 : wrval = GasPrm.Decmals(1)
                Case 59 : wrval = GasPrm.Decmals(2)
                Case 60 : wrval = GasPrm.Decmals(3)
                Case 61 : wrval = GasPrm.Decmals(4)
                Case 62 : wrval = GasPrm.Decmals(5)

                Case 63 : wrval = RoughPrm.KeepTim_aftrRough * 10
                Case 64 : wrval = 0
                Case 65 : wrval = 0

                Case 66 : wrval = PyrAlm.HigWarSP
                Case 67 : wrval = PyrAlm.HigAlmSP
                Case 68 : wrval = PyrAlm.LowWarSP
                Case 69 : wrval = PyrAlm.LowAlmSP

                Case 70 : wrval = PyrAlm.HigWarDT * 10
                Case 71 : wrval = PyrAlm.HigAlmDT * 10
                Case 72 : wrval = PyrAlm.LowWarDT * 10
                Case 73 : wrval = PyrAlm.LowAlmDT * 10

                Case 74 : wrval = WtrAlm_FS1.LowFlw_WarSP * 10
                Case 75 : wrval = WtrAlm_FS2.LowFlw_WarSP * 10
                Case 76 : wrval = WtrAlm_FS3.LowFlw_WarSP * 10

                Case 77 : wrval = WtrAlm_FS1.LowFlw_AlmSP * 10
                Case 78 : wrval = WtrAlm_FS2.LowFlw_AlmSP * 10
                Case 79 : wrval = WtrAlm_FS3.LowFlw_AlmSP * 10

                Case 80 : wrval = WtrAlm_FS1.HigTmp_WarSP * 10
                Case 81 : wrval = WtrAlm_FS2.HigTmp_WarSP * 10
                Case 82 : wrval = WtrAlm_FS3.HigTmp_WarSP * 10

                Case 83 : wrval = WtrAlm_FS1.HigTmp_AlmSP * 10
                Case 84 : wrval = WtrAlm_FS2.HigTmp_AlmSP * 10
                Case 85 : wrval = WtrAlm_FS3.HigTmp_AlmSP * 10

                Case 86 : wrval = WtrAlm_FS1.LowTmp_WarSP * 10
                Case 87 : wrval = WtrAlm_FS2.LowTmp_WarSP * 10
                Case 88 : wrval = WtrAlm_FS3.LowTmp_WarSP * 10

                Case 89 : wrval = WtrAlm_FS1.LowTmp_AlmSP * 10
                Case 90 : wrval = WtrAlm_FS2.LowTmp_AlmSP * 10
                Case 91 : wrval = WtrAlm_FS3.LowTmp_AlmSP * 10

                Case 92 : wrval = WtrAlm_FS1.LowFlw_WarDT * 10
                Case 93 : wrval = WtrAlm_FS2.LowFlw_WarDT * 10
                Case 94 : wrval = WtrAlm_FS3.LowFlw_WarDT * 10

                Case 95 : wrval = WtrAlm_FS1.LowFlw_AlmDT * 10
                Case 96 : wrval = WtrAlm_FS2.LowFlw_AlmDT * 10
                Case 97 : wrval = WtrAlm_FS3.LowFlw_AlmDT * 10

                Case 98 : wrval = WtrAlm_FS1.HigTmp_WarDT * 10
                Case 99 : wrval = WtrAlm_FS2.HigTmp_WarDT * 10
                Case 100 : wrval = WtrAlm_FS3.HigTmp_WarDT * 10

                Case 101 : wrval = WtrAlm_FS1.HigTmp_AlmDT * 10
                Case 102 : wrval = WtrAlm_FS2.HigTmp_AlmDT * 10
                Case 103 : wrval = WtrAlm_FS3.HigTmp_AlmDT * 10

                Case 104 : wrval = WtrAlm_FS1.LowTmp_WarDT * 10
                Case 105 : wrval = WtrAlm_FS2.LowTmp_WarDT * 10
                Case 106 : wrval = WtrAlm_FS3.LowTmp_WarDT * 10

                Case 107 : wrval = WtrAlm_FS1.LowTmp_AlmDT * 10
                Case 108 : wrval = WtrAlm_FS2.LowTmp_AlmDT * 10
                Case 109 : wrval = WtrAlm_FS3.LowTmp_AlmDT * 10

                Case 110 : wrval = MwAlm.HigWarSP
                Case 111 : wrval = MwAlm.HigAlmSP
                Case 112 : wrval = MwAlm.LowWarSP
                Case 113 : wrval = MwAlm.LowAlmSP

                Case 114 : wrval = MwAlm.HigWarDT * 10
                Case 115 : wrval = MwAlm.HigAlmDT * 10
                Case 116 : wrval = MwAlm.LowWarDT * 10
                Case 117 : wrval = MwAlm.LowAlmDT * 10

                Case 118 : wrval = 0
                Case 119 : wrval = 0
                Case 120 : wrval = 0


                'for the future
                'Case 74 : wrval = GasPrm.Cf(0) * 100
                'Case 75 : wrval = GasPrm.Cf(1) * 100
                'Case 76 : wrval = GasPrm.Cf(2) * 100
                'Case 77 : wrval = GasPrm.Cf(3) * 100
                'Case 78 : wrval = GasPrm.Cf(4) * 100
                'Case 79 : wrval = GasPrm.Cf(5) * 100

                Case 121 : wrval = GasPrm.Tol(0) * CDbl(10000) / GasPrm.Fs(0)
                Case 122 : wrval = GasPrm.Tol(1) * CDbl(10000) / GasPrm.Fs(1)
                Case 123 : wrval = GasPrm.Tol(2) * CDbl(10000) / GasPrm.Fs(2)
                Case 124 : wrval = GasPrm.Tol(3) * CDbl(10000) / GasPrm.Fs(3)
                Case 125 : wrval = GasPrm.Tol(4) * CDbl(10000) / GasPrm.Fs(4)
                Case 126 : wrval = GasPrm.Tol(5) * CDbl(10000) / GasPrm.Fs(5)

                Case 127 : wrval = ThrPidPrm.Time
                Case 128 : wrval = ThrPidPrm.P
                Case 129 : wrval = ThrPidPrm.I
                Case 130 : wrval = ThrPidPrm.D
                Case 131 : wrval = ThrPidPrm.DeltaLimit

                Case 132 : wrval = AutLekrateRec.RoughPoint * 10
                Case 133 : wrval = AutLekrateRec.VacumTime

                Case 134 : wrval = PrsPrm.ProcWarning * 10 'Sone Scale change

                '23/2/2 FS4,5追加
                Case 135 : wrval = WtrAlm_FS4.LowFlw_WarSP * 10
                Case 136 : wrval = WtrAlm_FS5.LowFlw_WarSP * 10

                Case 137 : wrval = WtrAlm_FS4.LowFlw_AlmSP * 10
                Case 138 : wrval = WtrAlm_FS5.LowFlw_AlmSP * 10

                Case 139 : wrval = WtrAlm_FS4.HigTmp_WarSP * 10
                Case 140 : wrval = WtrAlm_FS5.HigTmp_WarSP * 10

                Case 141 : wrval = WtrAlm_FS4.HigTmp_AlmSP * 10
                Case 142 : wrval = WtrAlm_FS5.HigTmp_AlmSP * 10

                Case 143 : wrval = WtrAlm_FS4.LowTmp_WarSP * 10
                Case 144 : wrval = WtrAlm_FS5.LowTmp_WarSP * 10

                Case 145 : wrval = WtrAlm_FS4.LowTmp_AlmSP * 10
                Case 146 : wrval = WtrAlm_FS5.LowTmp_AlmSP * 10

                Case 147 : wrval = WtrAlm_FS4.LowFlw_WarDT * 10
                Case 148 : wrval = WtrAlm_FS5.LowFlw_WarDT * 10

                Case 149 : wrval = WtrAlm_FS4.LowFlw_AlmDT * 10
                Case 150 : wrval = WtrAlm_FS5.LowFlw_AlmDT * 10

                Case 151 : wrval = WtrAlm_FS4.HigTmp_WarDT * 10
                Case 152 : wrval = WtrAlm_FS5.HigTmp_WarDT * 10

                Case 153 : wrval = WtrAlm_FS4.HigTmp_AlmDT * 10
                Case 154 : wrval = WtrAlm_FS5.HigTmp_AlmDT * 10

                Case 155 : wrval = WtrAlm_FS4.LowTmp_WarDT * 10
                Case 156 : wrval = WtrAlm_FS5.LowTmp_WarDT * 10

                Case 157 : wrval = WtrAlm_FS4.LowTmp_AlmDT * 10
                Case 158 : wrval = WtrAlm_FS5.LowTmp_AlmDT * 10

                Case 159 : wrval = PlnmPrm.Fs * 10 '23/3/13 Sone PLENのFull Scale追加

                '23/10/04 FS6追加
                Case 160 : wrval = WtrAlm_FS6.LowFlw_WarSP * 10
                Case 161 : wrval = WtrAlm_FS6.LowFlw_AlmSP * 10

                Case 162 : wrval = WtrAlm_FS6.HigTmp_WarSP * 10
                Case 163 : wrval = WtrAlm_FS6.HigTmp_AlmSP * 10

                Case 164 : wrval = WtrAlm_FS6.LowTmp_WarSP * 10
                Case 165 : wrval = WtrAlm_FS6.LowTmp_AlmSP * 10

                Case 166 : wrval = WtrAlm_FS6.LowFlw_WarDT * 10
                Case 167 : wrval = WtrAlm_FS6.LowFlw_AlmDT * 10

                Case 168 : wrval = WtrAlm_FS6.HigTmp_WarDT * 10
                Case 169 : wrval = WtrAlm_FS6.HigTmp_AlmDT * 10

                Case 170 : wrval = WtrAlm_FS6.LowTmp_WarDT * 10
                Case 171 : wrval = WtrAlm_FS6.LowTmp_AlmDT * 10

               '24/6/11 171からAT Stub Pos追加のため変更
                Case 172 : wrval = ATStubPos.StubPos14
                Case 173 : wrval = ATStubPos.StubPos23


            End Select

            device(i) = AxDBDeviceManager2.Devices.Item(i)
            device(i).ValueWrite = wrval

        Next

        WrComp = False
        AxDBDeviceManager2.WriteAll()
        Do
            My.Application.DoEvents()
        Loop Until WrComp = True
        RdPrm_PLC()

    End Sub

    Private Sub AxDBDeviceManager1_AfterRead(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager1.AfterRead
        '////// SYSTEM DEVICE MONITOR //////

        'PUMP
        If AxDBDeviceManager1.Devices(1).ValueRead = 1 Then
            RectangleShape56.FillColor = DonCLR
        Else
            RectangleShape56.FillColor = DofCLR
        End If

        'MW STANDBY
        If AxDBDeviceManager1.Devices(2).ValueRead = 1 Then
            Label57.BackColor = VonCLR
        Else
            Label57.BackColor = VofCLR
        End If

        'MW POWER
        If AxDBDeviceManager1.Devices(34).ValueRead = 1 Then
            Label58.BackColor = VonCLR
        Else
            Label58.BackColor = VofCLR
        End If

        'PLASMA
        If AxDBDeviceManager1.Devices(34).ValueRead = 1 Then
            OvalShape8.FillStyle = PowerPacks.FillStyle.Solid
            OvalShape8.FillColor = Color.Violet
        Else
            OvalShape8.FillStyle = PowerPacks.FillStyle.Transparent
            OvalShape8.FillColor = Color.Transparent
        End If

        'ROUGH(V1)
        If AxDBDeviceManager1.Devices(3).ValueRead = 1 Then
            RectangleShape28.FillColor = VonCLR
        Else
            RectangleShape28.FillColor = VofCLR
        End If

        'THR(V2)
        If AxDBDeviceManager1.Devices(4).ValueRead = 1 Then
            RectangleShape45.FillColor = VonCLR
        Else
            RectangleShape45.FillColor = VofCLR
        End If

        'PLNUM(V5)
        If AxDBDeviceManager1.Devices(5).ValueRead = 1 Then
            RectangleShape40.FillColor = VonCLR
        Else
            RectangleShape40.FillColor = VofCLR
        End If

        'VENT(V3)
        If AxDBDeviceManager1.Devices(6).ValueRead = 1 Then
            RectangleShape30.FillColor = VonCLR
        Else
            RectangleShape30.FillColor = VofCLR
        End If

        'PROCESS(V4)
        If AxDBDeviceManager1.Devices(7).ValueRead = 1 Then
            RectangleShape38.FillColor = VonCLR
        Else
            RectangleShape38.FillColor = VofCLR
        End If

        'V11
        If AxDBDeviceManager1.Devices(8).ValueRead = 1 Then
            RectangleShape67.FillColor = VonCLR
        Else
            RectangleShape67.FillColor = VofCLR
        End If

        'V12
        If AxDBDeviceManager1.Devices(9).ValueRead = 1 Then
            RectangleShape112.FillColor = VonCLR
        Else
            RectangleShape112.FillColor = VofCLR
        End If

        'V13
        If AxDBDeviceManager1.Devices(10).ValueRead = 1 Then
            RectangleShape84.FillColor = VonCLR
        Else
            RectangleShape84.FillColor = VofCLR
        End If

        'V21
        If AxDBDeviceManager1.Devices(11).ValueRead = 1 Then
            RectangleShape69.FillColor = VonCLR
        Else
            RectangleShape69.FillColor = VofCLR
        End If

        'V22
        If AxDBDeviceManager1.Devices(12).ValueRead = 1 Then
            RectangleShape114.FillColor = VonCLR
        Else
            RectangleShape114.FillColor = VofCLR
        End If

        'V23
        If AxDBDeviceManager1.Devices(13).ValueRead = 1 Then
            RectangleShape102.FillColor = VonCLR
        Else
            RectangleShape102.FillColor = VofCLR
        End If

        'V31
        If AxDBDeviceManager1.Devices(14).ValueRead = 1 Then
            RectangleShape71.FillColor = VonCLR
        Else
            RectangleShape71.FillColor = VofCLR
        End If

        'V32
        If AxDBDeviceManager1.Devices(15).ValueRead = 1 Then
            RectangleShape116.FillColor = VonCLR
        Else
            RectangleShape116.FillColor = VofCLR
        End If

        'V33
        If AxDBDeviceManager1.Devices(16).ValueRead = 1 Then
            RectangleShape104.FillColor = VonCLR
        Else
            RectangleShape104.FillColor = VofCLR
        End If

        'V41
        If AxDBDeviceManager1.Devices(17).ValueRead = 1 Then
            RectangleShape73.FillColor = VonCLR
        Else
            RectangleShape73.FillColor = VofCLR
        End If

        'V42
        If AxDBDeviceManager1.Devices(18).ValueRead = 1 Then
            RectangleShape118.FillColor = VonCLR
        Else
            RectangleShape118.FillColor = VofCLR
        End If

        'V43
        If AxDBDeviceManager1.Devices(19).ValueRead = 1 Then
            RectangleShape106.FillColor = VonCLR
        Else
            RectangleShape106.FillColor = VofCLR
        End If

        'V51
        If AxDBDeviceManager1.Devices(20).ValueRead = 1 Then
            RectangleShape75.FillColor = VonCLR
        Else
            RectangleShape75.FillColor = VofCLR
        End If

        'V52
        If AxDBDeviceManager1.Devices(21).ValueRead = 1 Then
            RectangleShape120.FillColor = VonCLR
        Else
            RectangleShape120.FillColor = VofCLR
        End If

        'V53
        If AxDBDeviceManager1.Devices(22).ValueRead = 1 Then
            RectangleShape108.FillColor = VonCLR
        Else
            RectangleShape108.FillColor = VofCLR
        End If

        'V61
        If AxDBDeviceManager1.Devices(23).ValueRead = 1 Then
            RectangleShape77.FillColor = VonCLR
        Else
            RectangleShape77.FillColor = VofCLR
        End If

        'V62
        If AxDBDeviceManager1.Devices(24).ValueRead = 1 Then
            RectangleShape122.FillColor = VonCLR
        Else
            RectangleShape122.FillColor = VofCLR
        End If

        'V63
        If AxDBDeviceManager1.Devices(25).ValueRead = 1 Then
            RectangleShape110.FillColor = VonCLR
        Else
            RectangleShape110.FillColor = VofCLR
        End If

        'V71
        If AxDBDeviceManager1.Devices(26).ValueRead = 1 Then
            RectangleShape79.FillColor = VonCLR
        Else
            RectangleShape79.FillColor = VofCLR
        End If

        'V72
        If AxDBDeviceManager1.Devices(27).ValueRead = 1 Then
            RectangleShape124.FillColor = VonCLR
        Else
            RectangleShape124.FillColor = VofCLR
        End If

        'V6
        If AxDBDeviceManager1.Devices(35).ValueRead = 1 Then
            RectangleShape51.FillColor = VonCLR
        Else
            RectangleShape51.FillColor = VofCLR
        End If

        'V7
        If AxDBDeviceManager1.Devices(36).ValueRead = 1 Then
            RectangleShape53.FillColor = VonCLR
        Else
            RectangleShape53.FillColor = VofCLR
        End If

        'TMP
        If AxDBDeviceManager1.Devices(37).ValueRead = 1 Then
            RectangleShape55.FillColor = DonCLR
        Else
            RectangleShape55.FillColor = DofCLR
        End If

        'PS
        If AxDBDeviceManager1.Devices(28).ValueRead = 1 Then
            OvalShape6.FillColor = DonCLR
            Label7.BackColor = DonCLR
        Else
            OvalShape6.FillColor = Color.Lime
            Label7.BackColor = Color.Lime
        End If

        'DG ???
        If AxDBDeviceManager1.Devices(29).ValueRead = 1 Then
            If AxDBDeviceManager1.Devices(30).ValueRead = 1 Then
                OvalShape2.FillColor = Color.LightGray
            Else
                OvalShape2.FillColor = DonCLR
            End If
        Else
            If AxDBDeviceManager1.Devices(30).ValueRead = 0 Then
                OvalShape2.FillColor = Color.Lime
            End If
        End If

        'IG
        If AxDBDeviceManager1.Devices(38).ValueRead = 1 Then
            OvalShape1.FillColor = Color.Lime
            Label1.BackColor = Color.Lime
        Else
            OvalShape1.FillColor = DonCLR
            Label1.BackColor = DonCLR
        End If

        'PIRANI(PiG)
        If AxDBDeviceManager1.Devices(39).ValueRead = 1 Then
            OvalShape7.FillColor = Color.Lime
            Label155.BackColor = Color.Lime
        Else
            OvalShape7.FillColor = DonCLR
            Label155.BackColor = DonCLR
        End If


        'HEIGHT
        If AxDBDeviceManager1.Devices(41).ValueRead = 1 Then
            Height_time = CSng(AxDBDeviceManager3.Devices(67).ValueRead) / 10
            TextBox88.Text = Format(Height_time, "0.0")
        End If

        If AxDBDeviceManager1.Devices(41).ValueRead = 1 Then
            Height_act = CSng(AxDBDeviceManager3.Devices(68).ValueRead) / 100
            TextBox89.Text = Format(Height_act, "0.00")
        End If


        'Height
        If AxDBDeviceManager1.Devices(42).ValueRead = 1 Then    'M3800
            Button30.BackColor = Color.Coral
        Else
            Button30.BackColor = Color.Transparent
        End If

        If AxDBDeviceManager1.Devices(43).ValueRead = 1 Then    'M3709
            Label171.BackColor = Color.Lime
        Else
            Label171.BackColor = Color.Silver
        End If

        If AxDBDeviceManager1.Devices(44).ValueRead = 1 Then    'M3801
            Label163.BackColor = Color.SkyBlue
        Else
            Label163.BackColor = Color.Silver

        End If

        If AxDBDeviceManager1.Devices(40).ValueRead = 1 Then    'M3801
            Button31.BackColor = Color.Coral
        Else
            Button31.BackColor = Color.Transparent
        End If


        'HEIGHT
        Height_mes = CSng(AxDBDeviceManager3.Devices(69).ValueRead)

        Dim Hei As Single

        If Height_mes >= 32768 Then
            Hei = Format((65536 - Height_mes) * (-1), "0")
        Else
            Hei = Format(Height_mes, "0")

        End If

        Hei = Hei / 1000

        Label170.Text = Format(Hei, "0.00")



        'gruopbox

        If AxDBDeviceManager1.Devices(45).ValueRead = 1 Then  'Height Control Box
            Me.GroupBox12.Visible = False
            'Me.Label116.Visible = True
            Me.TextBox58.Visible = True
        Else
            Me.GroupBox12.Visible = True
            'Me.Label116.Visible = False
            Me.TextBox58.Visible = False
        End If

        'If AxDBDeviceManager1.Devices(67).ValueRead = 1 Then  'Temp Control Box
        '    Me.GroupBox11.Visible = False
        'Else
        '    Me.GroupBox11.Visible = True
        'End If


        If AxDBDeviceManager3.Devices(70).ValueRead = 39321 Then
            Exit Sub
        Else
            Application.Exit()
        End If

    End Sub

    Public Sub Cnv_rs485toVal(ByVal rsvd As String)
        '-----------------------------------------------------------------------
        '                            RS485cnv
        '-----------------------------------------------------------------------
        Dim mwfwd As Single
        Dim mwref As Single
        'Dim i As Integer
        'Dim j As Integer
        'Dim k As Integer
        'Dim s As String = ""
        'Dim mw_fwdtxt As String
        'Dim mw_reftxt As String

        Try
            'If SysTyp.MwDevice > 0 Then
            mwfwd = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1003")
            mwref = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1021")
            'Else
            ''2xxxx,xxxxFF 2x,xFF 2xx,xxFF
            'i = 0
            'Do Until Mid(rsvd, i + 1, 1) = Chr(0)
            '    s = s & Mid(rsvd, i + 1, 1)
            '    i = i + 1
            '    If i > 12 Then Exit Do
            'Loop

            'i = 0
            'Do
            '    i = i + 1
            'Loop Until Mid(s, i, 1) = ","

            'mw_fwdtxt = Mid(s, 2, i - 2)
            'mwfwd = Val(mw_fwdtxt) * 10
            'j = i + 1
            'k = Len(s) - i - 2
            ''Do
            ''    i = i + 1
            ''    DoEvents
            ''Loop Until Mid(rsvd, i, 1) = vbCrLf
            'mw_reftxt = Mid(s, j, k)
            'mwref = Val(mw_reftxt) * 10
            'End If
        Catch ex As Exception
            mwfwd = -1
            mwref = -1
        End Try

        MwF = mwfwd
        MwR = mwref

    End Sub

    Private Sub AxDBDeviceManager3_BeforeRead(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager3.BeforeRead
        '////// SYSTEM DATA MONITOR //////

        'Dim pcu As Single
        Dim plnum As Single

        Dim pres As Single
        Dim gas(6) As Single
        Dim thr As Single
        Dim temp As Single
        Dim i As Integer

        'TEMP(PYRO)
        temp = AxDBDeviceManager3.Devices(24).ValueRead '(24)=D1023
        Label105.Text = Format(temp, "0")

        'PLENUM(PCU)
        plnum = AxDBDeviceManager3.Devices(2).ValueRead '(2)=D1001
        plnum = plnum / 10
        Label72.Text = Format(plnum, "0.0")

        'PRESS
        pres = AxDBDeviceManager3.Devices(6).ValueRead
        pres = pres / 10
        Label73.Text = Format(pres, "0.0")

        'GAS1
        gas(1) = AxDBDeviceManager3.Devices(8).ValueRead
        gas(1) = gas(1) / 10000 * GasPrm.Fs(0)
        Label30.Text = Format(gas(1), Dgstr(GasPrm.Decmals(0)))

        'GAS2
        gas(2) = AxDBDeviceManager3.Devices(10).ValueRead
        gas(2) = gas(2) / 10000 * GasPrm.Fs(1)
        Label10.Text = Format(gas(2), Dgstr(GasPrm.Decmals(1)))

        'GAS3
        gas(3) = AxDBDeviceManager3.Devices(12).ValueRead
        gas(3) = gas(3) / 10000 * GasPrm.Fs(2)
        Label11.Text = Format(gas(3), Dgstr(GasPrm.Decmals(2)))

        'GAS4
        gas(4) = AxDBDeviceManager3.Devices(14).ValueRead
        gas(4) = gas(4) / 10000 * GasPrm.Fs(3)
        Label12.Text = Format(gas(4), Dgstr(GasPrm.Decmals(3)))

        'GAS5
        gas(5) = AxDBDeviceManager3.Devices(16).ValueRead
        gas(5) = gas(5) / 10000 * GasPrm.Fs(4)
        Label13.Text = Format(gas(5), Dgstr(GasPrm.Decmals(4)))

        'GAS6
        gas(6) = AxDBDeviceManager3.Devices(18).ValueRead
        gas(6) = gas(6) / 10000 * GasPrm.Fs(5)
        Label14.Text = Format(gas(6), Dgstr(GasPrm.Decmals(5)))

        'Thr
        thr = AxDBDeviceManager3.Devices(63).ValueRead    '(63)=D1062
        Label8.Text = Format(thr / 10, "0.0")

        'for Rs485 Type
        Dim mw1 As Integer
        Dim mw2 As Integer
        Dim mw3 As Integer
        Dim mw4 As Integer
        Dim mw5 As Integer
        Dim mw6 As Integer
        Dim mwH As String
        Dim rsdt(12) As String
        'Dim mw_act As Single

        mw1 = AxDBDeviceManager3.Devices(61).ValueRead 'D1060
        mw2 = AxDBDeviceManager3.Devices(62).ValueRead 'D1061
        mw3 = AxDBDeviceManager3.Devices(63).ValueRead 'D1062
        mw4 = AxDBDeviceManager3.Devices(64).ValueRead 'D1063
        mw5 = AxDBDeviceManager3.Devices(65).ValueRead 'D1064
        mw6 = AxDBDeviceManager3.Devices(66).ValueRead 'D1065

        mwH = Hex(mw1) & Hex(mw2) & Hex(mw3) & Hex(mw4) & Hex(mw5) & Hex(mw6)

        For i = 0 To 11
            rsdt(i + 1) = Mid(mwH, 2 * i + 1, 2)
            rsdt(0) = rsdt(0) & Chr(Val("&H" & rsdt(i + 1)))
        Next

        Cnv_rs485toVal(rsdt(0))

        If AxDBDeviceManager1.Devices(34).ValueRead = 1 Then
            Label56.Text = Format(MwF / 1, "0") ' 2023/3/30 Sone /10→/1に修正　10kWは/10不要
            Label60.Text = Format(MwR / 1, "0")
        Else
            Label56.Text = Format(0, "0")
            Label60.Text = Format(0, "0")
        End If
        'mw_act = CSng(AxDBDeviceManager3.Devices(3).ValueRead) / 1      'D1002
        'TextBox6.Text = Format(mw_act, "0")

    End Sub


    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click
        'SHUT ALL VALVES
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1202", 1)
    End Sub

    Private Sub RectangleShape56_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape56.Click
        'PUMP
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "700", 1)
    End Sub

    Private Sub RectangleShape28_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape28.Click, RectangleShape_V1.Click
        'ROUGH(V1)
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1002", 1)
    End Sub

    Private Sub RectangleShape45_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape45.Click, RectangleShape_V2.Click
        'THR(V2)
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1003", 1)
    End Sub

    Private Sub RectangleShape40_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape40.Click, RectangleShape_V5.Click
        'PLNUM(V5)
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1004", 1)
    End Sub

    Private Sub RectangleShape30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape30.Click, RectangleShape_V3.Click
        'VENT(V3)
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1005", 1)
    End Sub

    Private Sub RectangleShape38_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape38.Click, RectangleShape_V4.Click
        'PROCESS(V4)
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1006", 1)
    End Sub

    Private Sub RectangleShape67_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape67.Click, RectangleShape_V11.Click
        'V11
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1007", 1)
    End Sub

    Private Sub RectangleShape112_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape112.Click, RectangleShape_V12.Click
        'V12
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1008", 1)
    End Sub

    Private Sub RectangleShape84_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape84.Click, RectangleShape_V13.Click
        'V13
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1009", 1)
    End Sub

    Private Sub RectangleShape69_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape69.Click, RectangleShape_V21.Click
        'V21
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1010", 1)
    End Sub

    Private Sub RectangleShape114_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape114.Click, RectangleShape_V22.Click
        'V22
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1011", 1)
    End Sub

    Private Sub RectangleShape102_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape102.Click, RectangleShape_V23.Click
        'V23
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1012", 1)
    End Sub

    Private Sub RectangleShape71_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape71.Click, RectangleShape_V31.Click
        'V31
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1013", 1)
    End Sub

    Private Sub RectangleShape116_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape116.Click, RectangleShape_V32.Click
        'V32
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1014", 1)
    End Sub

    Private Sub RectangleShape104_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape104.Click, RectangleShape_V33.Click
        'V33
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1015", 1)
    End Sub

    Private Sub RectangleShape73_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape73.Click, RectangleShape_V41.Click
        'V41
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1100", 1)
    End Sub

    Private Sub RectangleShape118_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape118.Click, RectangleShape_V42.Click
        'V42
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1101", 1)
    End Sub

    Private Sub RectangleShape106_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape106.Click, RectangleShape_V43.Click
        'V43
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1102", 1)
    End Sub

    Private Sub RectangleShape75_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape75.Click, RectangleShape_V51.Click
        'V51
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1103", 1)
    End Sub

    Private Sub RectangleShape120_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape120.Click, RectangleShape_V52.Click
        'V52
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1104", 1)
    End Sub

    Private Sub RectangleShape108_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape108.Click, RectangleShape_V53.Click
        'V53
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1105", 1)
    End Sub

    Private Sub RectangleShape77_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape77.Click, RectangleShape_V61.Click
        'V61
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1106", 1)
    End Sub

    Private Sub RectangleShape122_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape122.Click, RectangleShape_V62.Click
        'V62
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1107", 1)
    End Sub

    Private Sub RectangleShape110_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape110.Click, RectangleShape_V63.Click
        'V63
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1108", 1)
    End Sub

    Private Sub RectangleShape79_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape79.Click, RectangleShape_V71.Click
        'V71
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1109", 1)
    End Sub

    Private Sub RectangleShape124_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape124.Click, RectangleShape_V72.Click
        'V72
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1110", 1)
    End Sub

    Private Sub RectangleShape51_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape51.Click, RectangleShape_V6.Click
        'V6
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1111", 1)
    End Sub

    Private Sub RectangleShape53_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape53.Click, RectangleShape_V7.Click
        'V7
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_RLY_B, "1112", 1)
    End Sub


    Private Sub Button30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button30.Click '20160928 App
        'HEIGHT
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "810", 1)
    End Sub
    Private Sub Button31_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button31.Click
        'HEIGHT HOME
        If AxDBDeviceManager1.Devices(40).ValueRead = 0 Then

            If MessageBox.Show("Return to Origin?", "", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "811", 1)
            Else
                Exit Sub
            End If

        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "811", 1)

        End If

    End Sub

    Private Sub Button33_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button33.Click
        'HEIGHT PRESET
        If MessageBox.Show("Z AXIS POSITION PRESET ?", "", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "812", 1)
        Else
            Exit Sub
        End If

    End Sub



    Private Sub TextBox89_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox89.KeyPress
        If e.KeyChar = vbCr Then
            'If CSng(TextBox89.Text) > 25.01 Then
            '    Height_act = 25.0

            'ElseIf (TextBox89.Text) < 0 Then
            '    Height_act = 0

            'Else
            Height_act = CSng(TextBox89.Text)

            'End If

            'Height_act = CSng(TextBox89.Text)

            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6000", Height_act * 100)
            TextBox89.Text = Format(Height_act, "0.00")

        End If
    End Sub

    Private Sub TextBox88_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox88.KeyPress
        'HEIGHT time
        If e.KeyChar = vbCr Then
            If CSng(TextBox88.Text) > 10910 Then
                Height_time = 10900
            Else
                Height_time = CSng(TextBox88.Text)

            End If

            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6010", Height_time * 10)
            TextBox88.Text = Format(Height_time, "0.0")
        End If

    End Sub




    Private Sub TextBox10_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox10.KeyPress
        'GAS1 SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox10.Text) / GasPrm.Fs(0) * 10000)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1006", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1006") / 10000 * GasPrm.Fs(0))
            TextBox10.Text = Format(rd, Dgstr(GasPrm.Decmals(0)))
        End If
    End Sub

    Private Sub TextBox1_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox1.KeyPress
        'GAS2 SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox1.Text) / GasPrm.Fs(1) * 10000)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1008", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1008") / 10000 * GasPrm.Fs(1))
            TextBox1.Text = Format(rd, Dgstr(GasPrm.Decmals(1)))
        End If
    End Sub

    Private Sub TextBox2_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox2.KeyPress
        'GAS3 SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox2.Text) / GasPrm.Fs(2) * 10000)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1010", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1010") / 10000 * GasPrm.Fs(2))
            TextBox2.Text = Format(rd, Dgstr(GasPrm.Decmals(2)))
        End If
    End Sub

    Private Sub TextBox3_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox3.KeyPress
        'GAS4 SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox3.Text) / GasPrm.Fs(3) * 10000)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1012", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1012") / 10000 * GasPrm.Fs(3))
            TextBox3.Text = Format(rd, Dgstr(GasPrm.Decmals(3)))
        End If
    End Sub

    Private Sub TextBox4_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox4.KeyPress
        'GAS5 SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox4.Text) / GasPrm.Fs(4) * 10000)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1014", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1014") / 10000 * GasPrm.Fs(4))
            TextBox4.Text = Format(rd, Dgstr(GasPrm.Decmals(4)))
        End If
    End Sub

    Private Sub TextBox5_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox5.KeyPress
        'GAS6 SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox5.Text) / GasPrm.Fs(5) * 10000)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1016", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1016") / 10000 * GasPrm.Fs(5))
            TextBox5.Text = Format(rd, Dgstr(GasPrm.Decmals(5)))
        End If
    End Sub

    Private Sub TextBox6_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox6.KeyPress
        'MW SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox6.Text))
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1002", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1002"))
            TextBox6.Text = Format(rd, "0")
        End If
    End Sub

    Private Sub TextBox7_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox7.KeyPress
        'PRESSURE CH SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox7.Text) * 10)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1004", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1004") / 10)
            TextBox7.Text = Format(rd, "0.0")
        End If
    End Sub

    Private Sub TextBox8_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox8.KeyPress
        'PRESSURE PL SETPOINT
        If e.KeyChar = vbCr Then
            Dim sp As Long = CLng(Val(TextBox8.Text) * 10)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1000", sp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1000") / 10)
            TextBox8.Text = Format(rd, "0.0")
        End If
    End Sub

    Private Sub AxDBDeviceManager2_AfterRead(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager2.AfterRead
        RdComp = True
    End Sub

    Private Sub AxDBDeviceManager2_AfterWrite(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager2.AfterWrite
        WrComp = True
        WrComp = True
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        frmMainte2.ShowDialog()
    End Sub

    Private Function SHA256_cnv(ByVal pasw As String) As System.Text.StringBuilder
        'SHA256 hash conversion

        Dim sha256 As System.Security.Cryptography.SHA256 = New System.Security.Cryptography.SHA256CryptoServiceProvider
        Dim origByte As Byte() = System.Text.Encoding.UTF8.GetBytes(pasw)
        Dim hashValue As Byte() = sha256.ComputeHash(origByte)

        'byte arrays convert to Hex strings
        Dim result As New System.Text.StringBuilder()
        Dim b As Byte

        For Each b In hashValue
            result.Append(b.ToString("x2"))
        Next

        Return result

    End Function

    Private Function RgsR() As String
        Dim regkey As Microsoft.Win32.RegistryKey =
                         Microsoft.Win32.Registry.CurrentUser.OpenSubKey("Software\SDS\Unlock", True)
        If regkey Is Nothing Then
            Return ""
        End If
        Dim stringValue As String = DirectCast(regkey.GetValue("string", "default"), String)
        regkey.Close()
        Return stringValue
        'Dim intValue As Integer = CInt(Microsoft.Win32.Registry.GetValue("HKEY_CURRENT_USER\Software\SDS\Unlock", "int", 0))

    End Function

    Private Sub AxDBDeviceManager4_AfterWrite(sender As Object, e As EventArgs) Handles AxDBDeviceManager4.AfterWrite
        WrComp = True
    End Sub

    Private Sub AxDBTriggerManager2_Fire(sender As Object, e As AxDATABUILDERAXLibLB._IDBTriggerEvents_FireEvent) Handles AxDBTriggerManager2.Fire
        ' Close this program When Login mode is changed in "Operator" !
        Me.Close()
    End Sub

    Private Sub TextBox90_TextChanged(sender As Object, e As EventArgs) Handles TextBox90.TextChanged    '2023/4/19 Alex
        If IsNumeric(CInt(TextBox90.Text)) = False Then
            TextBox90.Text = "0"
        Else
            If CInt(TextBox90.Text) > 1 Then
                TextBox90.Text = "0"
            End If
        End If

    End Sub

    Private Sub TextBox67_TextChanged(sender As Object, e As EventArgs) Handles TextBox67.TextChanged    '2023/4/19 Alex
        If IsNumeric(CInt(TextBox67.Text)) = False Then
            TextBox67.Text = "0"
        Else
            If CInt(TextBox67.Text) > 1 Then
                TextBox67.Text = "0"
            End If
        End If

    End Sub
    Private Sub TextBox68_TextChanged(sender As Object, e As EventArgs) Handles TextBox68.TextChanged    '2023/4/19 Alex
        If IsNumeric(CInt(TextBox68.Text)) = False Then
            TextBox68.Text = "0"
        Else
            If CInt(TextBox68.Text) > 1 Then
                TextBox68.Text = "0"
            End If
        End If

    End Sub

End Class
