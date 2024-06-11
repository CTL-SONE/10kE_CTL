Imports System.ComponentModel
Imports VB = Microsoft.VisualBasic

Public Class frmControl

    '**************************************************************
    '            SDS6K-10kE CONTROL Ver1.01.0
    '                      2024/05/28
    '**************************************************************
    'Ver1.00.0 20231005 Ver4.04ベース
    'SAIREM 10kW用にPulseモード、SafetyArea追加
    'Ver1.00.1 20231010 MW mode and Plasma Select are changed the name
    'When MW output is ON, can't select CW and Pulse, Plasma Size
    'Ver1.00.2 20231024 AlarmLogger auto start and Vent/rough cycle. GUI cleaning
    'Ver1.00.3 20231025 Default Safety area is nothing 
    'Ver1.00.4 20231031 Freq and D.C. Input mode revise and ATC ramp is OFF when safety area 3" and 4" selected
    'Ver1.00.5 20231106 Freq input revise(0.0 -> 0)
    'Ver1.00.6 20231127 REF-AT and HEAD exchange and FS2 name is changed to "FS2 BASE PLATE"
    'Ver1.00.7 20231206 Set Duty Cycle limit during Pulse mode
    'Ver1.01.0 20240528 Pyro2 measured add to Control

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
        Dim Tolerance As Single   '[Torr]
    End Structure

    'PyroMeter Parameter
    Structure PyroParameter
        Dim RangeL As Long        '[DegC]
        Dim RangeH As Long        '[DegC]
        'Dim PyroAlarmL As Long    '[DegC]
        'Dim PyroAlarmH As Long    '[DegC]
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

    'Wave Logger parameter  Append 2021/08/20
    Structure WavLog_prm
        Dim Window As Integer
    End Structure


    'Water Parameter 23/2/1 10kW-pulse
    Structure WaterParameter
        Dim FlowFs1 As Double
        Dim FlowFs2 As Double
        Dim FlowFs3 As Double
        Dim FlowFs4 As Double
        Dim FlowFs5 As Double
        Dim FlowFs6 As Double
        Dim TempFs1 As Double
        Dim TempFs2 As Double
        Dim TempFs3 As Double
        Dim TempFs4 As Double
        Dim TempFs5 As Double
        Dim TempFs6 As Double
    End Structure

    'Height Control

    'Control Visible Prameter
    Structure ControlVisible
        Dim Hold_button As Boolean
        Dim SysMon_button As Boolean
        Dim WavLog_button As Boolean
        Dim Clr_button As Boolean
        Dim Pyro_panel As Boolean
        Dim Pump_button As Boolean
        Dim Roug_button As Boolean
        Dim Vent_button As Boolean
        Dim Roug_setP As Boolean
        Dim PrsChm_button As Boolean
        Dim PrsChm_setP As Boolean
        Dim PrsPln_button As Boolean
        Dim PrsPln_setP As Boolean
        Dim MwStnby_button As Boolean
        Dim MwFwd_button As Boolean
        Dim MwFwd_setP As Boolean
        Dim MwRef_setP As Boolean
        Dim GasFlwName_lbl As Boolean
        Dim GasFlw_button As Boolean
        Dim GasFlw_lmp As Boolean
        Dim GasFlw_setP_inAct As Boolean
        Dim GasFlw_meas As Boolean
        Dim GasFlw_Devlmp As Boolean
        Dim TmpCtl_Buuton As Boolean
        Dim TmpCtl_START As Boolean
        Dim TmpCtl_SetPoint As Boolean
        Dim TmpCtl_InAction As Boolean
        Dim TmpCtl_Measured As Boolean
        Dim Recp_jmpto As Boolean
        Dim Recp_Step As Boolean
        Dim Recp_Pres As Boolean
        Dim Recp_Plen As Boolean
        Dim Recp_Temp As Boolean
        Dim Recp_Power As Boolean
        Dim Recp_MFC As Boolean
        Dim WavlogDsp As Boolean
    End Structure

    Const PATH As String = "C:\SDS6NETSYS\"
    Const PARAMFILE_NAM As String = "Param.txt"

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

    Dim Dgstr(3) As String      '"#.###"
    Dim Dpntpos(6) As Integer

    Dim VerInfo As String = ""


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
    Dim WvLg As WavLog_prm
    Dim WtrPrm As WaterParameter

    Dim Looptemp As Single
    Dim Looplimit As Single
    Dim Loopramp As Single
    Dim Looptime As Single
    Dim Looprange As Single

    Dim Looptemp_act As Single
    Dim Looplimit_act As Single
    Dim Loopramp_act As Single
    Dim Looptime_act As Single
    Dim Looprange_act As Single

    'Private Height_Mes As Integer
    Dim Height_mes As Integer
    Dim Height_act As Single
    Dim Height_time As Single
    Dim Height_actramp As Single

    'PIRANI
    Dim PIRANI1 As Single
    Dim PIRANI2 As Single
    Dim PIRANI3 As Single
    Dim PIRANI4 As Single

    'Semiramp
    Dim Ramp_time As Single
    Dim Ramp_timeact As Single
    Dim Ramp_pres As Single
    Dim Ramp_pren As Single
    Dim Ramp_height As Single
    Dim Ramp_MW As Single
    Dim Ramp_MFC1 As Single
    Dim Ramp_MFC2 As Single
    Dim Ramp_MFC3 As Single
    Dim Ramp_MFC4 As Single
    Dim Ramp_MFC5 As Single
    Dim Ramp_MFC6 As Single


    Dim LonCLR As System.Drawing.Color = Color.Lime         'Lmp ON color
    Dim LofCLR As System.Drawing.Color = Color.Silver       'Lmp OFF color
    Dim SonCLR As System.Drawing.Color = Color.Coral        'Sw ON color
    Dim SofCLR As System.Drawing.Color = Color.Transparent  'Sw OFF color
    Dim AonCLR As System.Drawing.Color = Color.Red          'Alm ON color
    Dim AofCLR As System.Drawing.Color = Color.DimGray      'Alm OFF color
    Dim Aontext As System.Drawing.Color = Color.DarkSalmon
    Dim Aon2text As System.Drawing.Color = Color.OrangeRed
    Dim Aofftext As System.Drawing.Color = Color.Gainsboro


    Dim Rough_Stp As Single
    Dim Rough_Act As Single
    Dim Rough_Mes As Single

    Dim Mw_Stp As Single
    Dim Mw_Act As Single
    Dim Mw_Mes As Single
    Dim Mw_Actramp As Single
    Dim Mw_Ave As Single

    Dim MwRef_Stp As Single
    Dim MwRef_Act As Single
    Dim MwRef_Mes As Single
    Dim AtRef_Mes As Single

    Dim Pres_Stp As Single
    Dim Pres_Act As Single
    Dim Pres_Mes As Single
    Dim Pres_Actramp As Single

    Dim Plnum_Stp As Single
    Dim Plnum_Act As Single
    Dim Plnum_Mes As Single

    Dim Gas1_Stp As Single
    Dim Gas1_Act As Single
    Dim Gas1_Mes As Single
    Dim Gas1_Actramp As Single

    Dim Gas2_Stp As Single
    Dim Gas2_Act As Single
    Dim Gas2_Mes As Single
    Dim Gas2_Actramp As Single

    Dim Gas3_Stp As Single
    Dim Gas3_Act As Single
    Dim Gas3_Mes As Single
    Dim Gas3_Actramp As Single

    Dim Gas4_Stp As Single
    Dim Gas4_Act As Single
    Dim Gas4_Mes As Single
    Dim Gas4_Actramp As Single

    Dim Gas5_Stp As Single
    Dim Gas5_Act As Single
    Dim Gas5_Mes As Single
    Dim Gas5_Actramp As Single

    Dim Gas6_Stp As Single
    Dim Gas6_Act As Single
    Dim Gas6_Mes As Single
    Dim Gas6_Actramp As Single

    Dim FS1flow As Single
    Dim FS2flow As Single
    Dim FS3flow As Single
    Dim FS4flow As Single
    Dim FS5flow As Single
    Dim FS6flow As Single

    Dim FS1temp As Single
    Dim FS2temp As Single
    Dim FS3temp As Single
    Dim FS4temp As Single
    Dim FS5temp As Single
    Dim FS6temp As Single

    Dim PyroAlarmL As Integer
    Dim PyroAlarmH As Integer
    Dim PyroWarL As Integer
    Dim PyroWarH As Integer

    Dim Cycle As Integer
    Dim ProcsT_pv As Integer
    Dim VentCnt_Stp As Integer
    Dim VentCount As Integer
    Dim StepT_min As Long
    Dim Stept_sec As Long
    Dim StepT As Long

    'Pulse mode 23/2/1 10kW-pulse
    Dim Ton_Stp As Single
    Dim Toff_Stp As Single
    Dim Ton_Act As Single             '----------- Added 23/10/27
    Dim Toff_Act As Single            '----------- Added 23/10/27
    Dim Freq_Act As Single
    Dim DutyCycle_Act As Single
    Dim Freq_Stp As Single            '----------- Added 23/10/27
    Dim DutyCycle_Stp As Single       '----------- Added 23/10/27
    Dim MWMode_Time As Boolean = True '----------- Added 23/10/27

    Dim Tbx_MWMode_Freq_edit As Boolean
    Dim Tbx_MWMode_Duty_edit As Boolean

    Public LoginMode As Integer = OPRATR 'Supervisor:1/ Operator:0
    Public Const SUPRVISR = 1
    Public Const OPRATR = 0
    Public Const LGMD_PLC As String = "47000"
    Public Ctlvsbl As ControlVisible
    Dim LoginKey As String = “e9c4efe35e2a90630ccfc6332d827167”

    Private WaveLogger As Object
    Private CxDoc As Object
    Private WLok As Integer = 99
    Public Expler As System.Diagnostics.Process

    Const WavLogrAbnormal_Ry As String = "6400" 'R6400

    Dim PLC_Connected As Boolean    'Append 2022/05/29
    Dim Frm_Loaded As Boolean

    Private Sub frmControl_Load(sender As Object, e As System.EventArgs) Handles Me.Load
        '*****************************
        '          Initialize  
        '*****************************

        'Forbid Multi Execution
        If Diagnostics.Process.GetProcessesByName(Diagnostics.Process.GetCurrentProcess.ProcessName).Length > 1 Then
            Application.Exit()
        End If

        Frm_Loaded = False

        '-------- Rev.2022/02
        'IP and Version of PLC are stored in HKEY_CURRENT_USER\SoftWare\SDS\PLC
        AxDBCommManager1.Peer = RgsPlcIp() & ":8500"    'default 192.168.3.13 & port 8500
        AxDBCommManager1.PLC = CInt(RgsPlcSr())         'default 515 = KV-5500/5000/3000

        'Try
        '    AxDBCommManager1.Connect()
        '    AxDBTriggerManager1.Active = True
        '    AxDBTriggerManager2.Active = True
        '    AxDBTriggerManager3.Active = True
        'Catch ex As Exception
        '    MessageBox.Show(ex.Message, "COM+")
        '    Exit Sub
        'End Try

        PLC_Connected = AxDBCommManager1_Connection_Check()
        If PLC_Connected = False Then
            MessageBox.Show("PLC not found !")
            Exit Sub
        End If

        AxDBTriggerManager1.Active = True
        AxDBTriggerManager2.Active = True
        AxDBTriggerManager3.Active = True

        LoginMode = OPRATR
        AxDBCommManager1.WriteDevice(DBPlcDevice.DKV5000XYM_D, LGMD_PLC, LoginMode)
        RgsW("0")

        ReadCtlvsblPrm()
        CtlVsble_Operation(LoginMode)

        'WLok = WavLogCreate()
        'If WLok <> 0 Then
        '    MessageBox.Show("Wave Logger Can not open!")
        'End If

        Dim s As String = RgsR()

        Dgstr(0) = "0"
        Dgstr(1) = "0.0"
        Dgstr(2) = "0.00"
        Dgstr(3) = "0.000"

        ReDim GasPrm.Name(5)
        ReDim GasPrm.Fs(5)
        ReDim GasPrm.Cf(5)
        ReDim GasPrm.Decmals(5)
        ReDim GasPrm.Tol(5)
        ReDim GasPrm.DevTime(5)
        ReDim GasPrm.TooLong(5)

        SysPrm_read()

        Label52.Text = GasPrm.Name(0)
        Label51.Text = GasPrm.Name(1)
        Label60.Text = GasPrm.Name(2)
        Label59.Text = GasPrm.Name(3)
        Label68.Text = GasPrm.Name(4)
        Label67.Text = GasPrm.Name(5)

        'Label174.Text = GasPrm.Name(0)
        'Label175.Text = GasPrm.Name(1)
        'Label176.Text = GasPrm.Name(2)
        'Label177.Text = GasPrm.Name(3)
        'Label178.Text = GasPrm.Name(4)
        'Label179.Text = GasPrm.Name(5)

        Rough_Stp = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1022") / 10
        TextBox7.Text = Format(Rough_Stp, "0.0")

        '-----------2021/07/16 Change----------
        PyroWarH = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "165")
        TextBox19.Text = Format(PyroWarH, "0")

        PyroAlarmH = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "166")
        TextBox20.Text = Format(PyroAlarmH, "0")

        PyroWarL = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "167")
        TextBox38.Text = Format(PyroWarL, "0")

        PyroAlarmL = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "168")
        TextBox39.Text = Format(PyroAlarmL, "0")
        '---------------------------------------

        'Looptemp = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1080")
        'TextBox21.Text = Format(Looptemp, "0")

        'Looplimit = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1082")
        'TextBox22.Text = Format(Looplimit, "0")

        'Loopramp = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1084")
        'TextBox23.Text = Format(Loopramp, "0")

        'Looptime = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1086")
        'TextBox24.Text = Format(Looptime, "0")

        'Looprange = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1088")
        'TextBox25.Text = Format(Looprange, "0")


        'Looptemp_act = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "2080")
        'Label125.Text = Format(Looptemp_act, "0")

        'Looplimit_act = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "2082")
        'Label120.Text = Format(Looplimit_act, "0")

        'Loopramp_act = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "2084")
        'Label127.Text = Format(Loopramp_act, "0")

        'Looptime_act = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "2040") / 10
        'Label128.Text = Format(Looptime_act, "0")

        'Looprange_act = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "2050")
        'Label133.Text = Format(Looprange_act, "0")

        'height

        'Height_act = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6000") / 100
        'TextBox27.Text = Format(Height_act, "0.00")

        'Height_time = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6010") / 10
        'TextBox26.Text = Format(Height_time, "0.0")

        'Ramptime
        'Ramp_time = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6050") / 10
        'TextBox28.Text = Format(Ramp_time, "0.0")

        ''Ramp press
        'Ramp_pres = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6060") / 10
        'TextBox29.Text = Format(Ramp_pres, "0.0")

        ''Ramp Height
        'Ramp_height = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6080") / 100
        'TextBox30.Text = Format(Ramp_height, "0.00")

        ''Ramp MW
        'Ramp_MW = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6100")
        'TextBox31.Text = Format(Ramp_MW, "0")


        'Ramp MFC1
        'Ramp_MFC1 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6120") / 10
        'TextBox32.Text = Format(Ramp_MFC1, "0.0")


        'Ramp_MFC1 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6120" * 10000 / GasPrm.Fs(0))
        'Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6120"))
        'TextBox32.Text = Format(rd * GasPrm.Fs(0) / 10000, Dgstr(GasPrm.Decmals(0)))


        'Ramp_MFC2 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6140" * 10000 / GasPrm.Fs(0))
        'Dim rd2 As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6140"))
        'TextBox33.Text = Format(rd2 * GasPrm.Fs(1) / 10000, Dgstr(GasPrm.Decmals(1)))


        'Ramp_MFC3 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6160" * 10000 / GasPrm.Fs(0))
        'Dim rd3 As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6160"))
        'TextBox34.Text = Format(rd3 * GasPrm.Fs(2) / 10000, Dgstr(GasPrm.Decmals(2)))


        'Ramp_MFC4 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6180" * 10000 / GasPrm.Fs(0))
        'Dim rd4 As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6180"))
        'TextBox35.Text = Format(rd4 * GasPrm.Fs(3) / 10000, Dgstr(GasPrm.Decmals(3)))


        'Ramp_MFC5 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6200" * 10000 / GasPrm.Fs(0))
        'Dim rd5 As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6200"))
        'TextBox36.Text = Format(rd5 * GasPrm.Fs(4) / 10000, Dgstr(GasPrm.Decmals(4)))

        'Ramp_MFC6 = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6220" * 10000 / GasPrm.Fs(0))
        'Dim rd6 As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6220"))
        'TextBox37.Text = Format(rd6 * GasPrm.Fs(5) / 10000, Dgstr(GasPrm.Decmals(5)))

        AxDBCommManager1.ReadText(DBPlcDevice.DKV5000_DM, "40000", 32, s)
        Label210.Text = s 'Operator name
        AxDBCommManager1.ReadText(DBPlcDevice.DKV5000_DM, "40100", 32, s)
        Label211.Text = s 'Lot number

        WLok = WavLogCreate()
        If WLok <> 0 Then
            MessageBox.Show("Wave Logger Can not open!")
        End If

        Timer1.Enabled = True

        ''2023/9/28 Sone
        ''---- Pulse---------------------------------------------------------------
        'Toff_Stp = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1278")) / 100
        'Tbx_MWMode_TOFF.Text = Format(Toff_Stp, "0.00")

        'Ton_Stp = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1280")) / 100
        'Tbx_MWMode_TON.Text = Format(Ton_Stp, "0.00")
        ''--------------------------------------------------------------------------

        '-------- Rev.2023/11/06 Sone
        If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6406") = 1 Then '"Pulse mode is ON = CW mode is OFF"
            Rbn_Mode_Pulse.Checked = True
            Rbn_Mode_CW.Checked = False
            Lbl_SA_Msg.Visible = True
            '%%%%%%%%%%%%%%%%%%%%%%%%%%2023/11/28 Sone Added Invisible SA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            Label133.Visible = False 'Safety Area Pres name
            Label135.Visible = False 'Safety Area Pres Lamp
            Label198.Visible = False 'Safety Area Power name
            Label199.Visible = False 'Safety Area Power Lamp
        Else  '"Pulse mode is OFF = CW mode is ON"
            Rbn_Mode_Pulse.Checked = False
            Rbn_Mode_CW.Checked = True
            Lbl_SA_Msg.Visible = False
            '%%%%%%%%%%%%%%%%%%%%%%%%%%2023/11/28 Sone Added Invisible SA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            Label133.Visible = True 'Safety Area Pres name
            Label135.Visible = True 'Safety Area Pres Lamp
            Label198.Visible = True 'Safety Area Power name
            Label199.Visible = True 'Safety Area Power Lamp
        End If

        '-------- Rev.2023/11/08 Sone
        If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6407") = 1 Then '"Input Time mode "
            RadioButton_MWMode_Freq.Checked = False
            RadioButton_MWMode_Time.Checked = True
            Tbx_MWMode_TON.BackColor = SystemColors.ControlLight
            Tbx_MWMode_TOFF.BackColor = SystemColors.ControlLight
            Tbx_MWMode_Duty.BackColor = Color.Khaki
            Tbx_MWMode_Freq.BackColor = Color.Khaki

            Tbx_MWMode_TON.ForeColor = SystemColors.WindowText
            Tbx_MWMode_TOFF.ForeColor = SystemColors.WindowText
            Tbx_MWMode_Duty.ForeColor = Color.SaddleBrown
            Tbx_MWMode_Freq.ForeColor = Color.SaddleBrown

            Tbx_MWMode_TON.Enabled = True
            Tbx_MWMode_TOFF.Enabled = True
            Tbx_MWMode_Duty.Enabled = False
            Tbx_MWMode_Freq.Enabled = False

        Else  '"Input Frequency mode"
            RadioButton_MWMode_Freq.Checked = True
            RadioButton_MWMode_Time.Checked = False
            Tbx_MWMode_TON.BackColor = Color.Khaki
            Tbx_MWMode_TOFF.BackColor = Color.Khaki
            Tbx_MWMode_Duty.BackColor = SystemColors.ControlLight
            Tbx_MWMode_Freq.BackColor = SystemColors.ControlLight

            Tbx_MWMode_TON.ForeColor = Color.SaddleBrown
            Tbx_MWMode_TOFF.ForeColor = Color.SaddleBrown
            Tbx_MWMode_Duty.ForeColor = SystemColors.WindowText
            Tbx_MWMode_Freq.ForeColor = SystemColors.WindowText

            Tbx_MWMode_TON.Enabled = False
            Tbx_MWMode_TOFF.Enabled = False
            Tbx_MWMode_Duty.Enabled = True
            Tbx_MWMode_Freq.Enabled = True
        End If


        '---- Pulse　Freq and Duty Cycle---------------------------------------------------------------
        Freq_Stp = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1246")) / 1
        Tbx_MWMode_Freq.Text = Format(Freq_Stp, "0")

        DutyCycle_Stp = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1247")) / 100
        Tbx_MWMode_Duty.Text = Format(DutyCycle_Stp, "0.0")
        '---- Pulse Ton and Toff---------------------------------------------------------------
        Toff_Stp = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1278")) / 100
        Tbx_MWMode_TOFF.Text = Format(Toff_Stp, "0.00")

        Ton_Stp = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1280")) / 100
        Tbx_MWMode_TON.Text = Format(Ton_Stp, "0.00")
        '--------------------------------------------------------------------------


        'Tbx_MWMode_TON.Enabled = False
        'Tbx_MWMode_TOFF.Enabled = False
        'Tbx_MWMode_Duty.Enabled = True
        'Tbx_MWMode_Freq.Enabled = True


        '-------- Rev.2023/09/28 Ver4.05 Safety area
        ' -> use a design procedure
        Puck_Size_Design()

        '-------- Rev.2023/09/27 Sone Title revise 
        Me.Text = "SDS6K-10kE CONTROL Ver1.01" 'Revised 2024/05/28

        '-------- Rev.2023/05/17 Ver4.05 Safety Area -> missing item by Alex
        Frm_Loaded = True

    End Sub

    '-------- Rev.2023/09/28 Ver4.05 Safety area Select
    Private Sub Puck_Size_Design()

        Dim SA_value As Integer
        SA_value = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "119")

        Dim Bit_119() As Boolean = GetBitArray(SA_value)

        '1' Select button V4.05には未実装
        'If Config_MR_17(1) = 1 Then               'with a Z-stage, 1in plasma is not possible
        '    Rbn_PUCK_1in.Text = "Z"
        'Else                                        '1in plasma is not possible, no Z-stage
        '    Rbn_PUCK_1in.Text = "1in"
        'End If

        If Bit_119(5) Then SA_value = 5 'Safety area of 1"/Z plasama" '-------- Rev.2023/11/06
        If Bit_119(2) Then SA_value = 2 'Safety area of 2" plasama"
        If Bit_119(3) Then SA_value = 3 'Safety area of 3" plasama"
        If Bit_119(4) Then SA_value = 4 'Safety area of 4" plasama"

        Select Case SA_value

            'Case 1
            '    Rbn_PUCK_1in.Checked = True
            Case 2
                Rbn_PUCK_2in.Checked = True
                Label135.Visible = False '2023/09/28 Sone When 2' selected, Pres Safety area is invisible
                Label133.Visible = False

            Case 3
                Rbn_PUCK_3in.Checked = True
                Label135.Visible = True '2023/09/28 Sone When 3' and 4' selected, Pres Safety area is visible
                Label133.Visible = True
                If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6406") = 1 Then '"Pulse mode is ON = CW mode is OFF"
                    '%%%%%%%%%%%%%%%%%%%%%%%%%%2023/11/28 Sone Added Invisible SA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    Label133.Visible = False 'Safety Area Pres name
                    Label135.Visible = False 'Safety Area Pres Lamp
                End If

            Case 4
                Rbn_PUCK_4in.Checked = True
                Label135.Visible = True '2023/09/28 Sone When 3' and 4' selected, Pres Safety area is visible
                Label133.Visible = True
                If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6406") = 1 Then '"Pulse mode is ON = CW mode is OFF"
                    '%%%%%%%%%%%%%%%%%%%%%%%%%%2023/11/28 Sone Added Invisible SA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    Label133.Visible = False 'Safety Area Pres name
                    Label135.Visible = False 'Safety Area Pres Lamp
                End If
        End Select

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

    Private Sub frmControl_FormClosed(sender As Object, e As FormClosedEventArgs) Handles MyBase.FormClosed
        'End process
        LoginMode = OPRATR
        AxDBCommManager1.WriteDevice(DBPlcDevice.DKV5000XYM_D, LGMD_PLC, LoginMode)
        RgsW("0")
        If WLok = 0 Then
            WaveLogger.Quit()
        End If
        WaveLogger = Nothing

    End Sub

    Private Sub frmControl_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "907") = 1 Then
            e.Cancel = True
        End If
    End Sub
    Private Sub ReadCtlvsblPrm()
        'Read Parameters of controls visible
        Dim fnam As String = PATH & "CtlVsPrm.txt"
        Dim fno As Integer = FreeFile()
        Dim dummy As String = ""
        Try
            FileOpen(fno, fnam, OpenMode.Input)

            Input(fno, dummy)
            With Ctlvsbl
                Input(fno, .Hold_button)
                Input(fno, .SysMon_button)
                Input(fno, .WavLog_button)
                Input(fno, .Clr_button)
                Input(fno, .Pyro_panel)
                Input(fno, .Pump_button)
                Input(fno, .Roug_button)
                Input(fno, .Vent_button)
                Input(fno, .Roug_setP)
                Input(fno, .PrsChm_button)
                Input(fno, .PrsChm_setP)
                Input(fno, .PrsPln_button)
                Input(fno, .PrsPln_setP)
                Input(fno, .MwStnby_button)
                Input(fno, .MwFwd_button)
                Input(fno, .MwFwd_setP)
                Input(fno, .MwRef_setP)
                Input(fno, .GasFlwName_lbl)
                Input(fno, .GasFlw_button)
                Input(fno, .GasFlw_lmp)
                Input(fno, .GasFlw_setP_inAct)
                Input(fno, .GasFlw_meas)
                Input(fno, .GasFlw_Devlmp)
                Input(fno, .TmpCtl_Buuton)
                Input(fno, .TmpCtl_START)
                Input(fno, .TmpCtl_SetPoint)
                Input(fno, .TmpCtl_InAction)
                Input(fno, .TmpCtl_Measured)
                Input(fno, .Recp_jmpto)
                Input(fno, .Recp_Step)
                Input(fno, .Recp_Pres)
                Input(fno, .Recp_Plen)
                Input(fno, .Recp_Temp)
                Input(fno, .Recp_Power)
                Input(fno, .Recp_MFC)
                Input(fno, .WavlogDsp)
            End With

            FileClose()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
        End Try

    End Sub
    Public Sub CtlVsble_Operation(login_mode As Integer)
        If login_mode = OPRATR Then
            With Ctlvsbl
                Button2.Visible = .Hold_button
                Button8.Visible = .SysMon_button
                Button10.Visible = .WavLog_button
                Button6.Visible = .Clr_button
                Panel1.Visible = .Pyro_panel
                Button11.Visible = .Pump_button
                Button12.Visible = .Roug_button
                Button13.Visible = .Vent_button
                TextBox7.Visible = .Roug_setP
                Button17.Visible = .PrsChm_button
                TextBox12.Visible = .PrsChm_setP
                Button14.Visible = .PrsPln_button
                TextBox11.Visible = .PrsPln_setP
                Button16.Visible = .MwStnby_button
                Button15.Visible = .MwFwd_button
                TextBox10.Visible = .MwFwd_setP
                TextBox9.Visible = .MwRef_setP
                Label52.Visible = .GasFlwName_lbl
                Label51.Visible = .GasFlwName_lbl
                Label60.Visible = .GasFlwName_lbl
                Label59.Visible = .GasFlwName_lbl
                Label68.Visible = .GasFlwName_lbl
                Label67.Visible = .GasFlwName_lbl
                Button19.Visible = .GasFlw_button
                Button18.Visible = .GasFlw_button
                Button21.Visible = .GasFlw_button
                Button20.Visible = .GasFlw_button
                Button23.Visible = .GasFlw_button
                Button22.Visible = .GasFlw_button
                Label50.Visible = .GasFlw_lmp
                Label47.Visible = .GasFlw_lmp
                Label58.Visible = .GasFlw_lmp
                Label57.Visible = .GasFlw_lmp
                Label66.Visible = .GasFlw_lmp
                Label65.Visible = .GasFlw_lmp
                TextBox14.Visible = .GasFlw_setP_inAct
                TextBox13.Visible = .GasFlw_setP_inAct
                TextBox16.Visible = .GasFlw_setP_inAct
                TextBox15.Visible = .GasFlw_setP_inAct
                TextBox18.Visible = .GasFlw_setP_inAct
                TextBox17.Visible = .GasFlw_setP_inAct
                Label42.Visible = .GasFlw_setP_inAct
                Label40.Visible = .GasFlw_setP_inAct
                Label56.Visible = .GasFlw_setP_inAct
                Label54.Visible = .GasFlw_setP_inAct
                Label64.Visible = .GasFlw_setP_inAct
                Label62.Visible = .GasFlw_setP_inAct
                Label41.Visible = .GasFlw_meas
                Label39.Visible = .GasFlw_meas
                Label55.Visible = .GasFlw_meas
                Label53.Visible = .GasFlw_meas
                Label63.Visible = .GasFlw_meas
                Label61.Visible = .GasFlw_meas
                Label72.Visible = .GasFlw_Devlmp
                Label73.Visible = .GasFlw_Devlmp
                Label74.Visible = .GasFlw_Devlmp
                Label75.Visible = .GasFlw_Devlmp
                Label76.Visible = .GasFlw_Devlmp
                Label78.Visible = .GasFlw_Devlmp

                Button38.Visible = .TmpCtl_Buuton

                'Button32.Visible = .TmpCtl_buuton
                'Button32.Enabled = .TmpCtl_buuton
                'Label129.Visible = .TmpCtl_lmp

                'Label132.Visible = .TmpCtl_setTmp
                'TextBox21.Visible = .TmpCtl_setTmp
                'Label125.Visible = .TmpCtl_setTmp
                'Label212.Visible = .TmpCtl_setTmp

                'Label131.Visible = .TmpCtl_powLmt
                'TextBox22.Visible = .TmpCtl_powLmt
                'Label120.Visible = .TmpCtl_powLmt
                'Label137.Visible = .TmpCtl_powLmt

                'Label130.Visible = .TmpCtl_powRmp
                'TextBox23.Visible = .TmpCtl_powRmp
                'Label127.Visible = .TmpCtl_powRmp
                'Label138.Visible = .TmpCtl_powRmp

                'Label134.Visible = .TmpCtl_smpTim
                'TextBox24.Visible = .TmpCtl_smpTim
                'Label128.Visible = .TmpCtl_smpTim
                'Label139.Visible = .TmpCtl_smpTim

                'Label135.Visible = .TmpCtl_tolerance
                'TextBox25.Visible = .TmpCtl_tolerance
                'Label133.Visible = .TmpCtl_tolerance
                'Label140.Visible = .TmpCtl_tolerance

                'Label126.Visible = .TmpCtl_measured
                'Label136.Visible = .TmpCtl_measured
                '--------------------------------------------
                'Button32.Visible = False    '.TmpCtl_buuton
                'Label129.Visible = False    '.TmpCtl_lmp
                'Label132.Visible = False    '.TmpCtl_setTmp
                'TextBox21.Visible = False   '.TmpCtl_setTmp
                'Label125.Visible = False    '.TmpCtl_setTmp

                'Label131.Visible = False    '.TmpCtl_powLmt
                'TextBox22.Visible = False   '.TmpCtl_powLmt
                'Label120.Visible = False    '.TmpCtl_powLmt

                'Label130.Visible = False    '.TmpCtl_powRmp
                'TextBox23.Visible = False   '.TmpCtl_powRmp
                'Label127.Visible = False    '.TmpCtl_powRmp

                'Label134.Visible = False    '.TmpCtl_smpTim
                'TextBox24.Visible = False    ' .TmpCtl_smpTim
                'Label128.Visible = False    '.TmpCtl_smpTim

                'Label135.Visible = False    '.TmpCtl_tolerance
                'TextBox25.Visible = False   '.TmpCtl_tolerance
                'Label133.Visible = False    '.TmpCtl_tolerance

                'Label126.Visible = False    '.TmpCtl_measured
                'GroupBox11.Visible = False
                '----------------------------------------------
                Try
                    If Expler.CloseMainWindow() = False Then
                        Expler.Kill()
                    End If
                Catch ex As Exception

                End Try
                Panel2.Visible = Not .GasFlwName_lbl
                Button9.Text = "RECIPE LOADER"
                Button29.Text = "SUPERVISOR"
                RgsW("0")

            End With
        Else
            Button2.Visible = True
            Button8.Visible = True
            Button10.Visible = True
            Button6.Visible = True
            Panel1.Visible = True
            Button11.Visible = True
            Button12.Visible = True
            Button13.Visible = True
            TextBox7.Visible = True
            Button17.Visible = True
            TextBox12.Visible = True
            Button14.Visible = True
            TextBox11.Visible = True
            Button16.Visible = True
            Button15.Visible = True
            TextBox10.Visible = True
            TextBox9.Visible = True
            Label52.Visible = True
            Label51.Visible = True
            Label60.Visible = True
            Label59.Visible = True
            Label68.Visible = True
            Label67.Visible = True
            Button19.Visible = True
            Button18.Visible = True
            Button21.Visible = True
            Button20.Visible = True
            Button23.Visible = True
            Button22.Visible = True
            Label50.Visible = True
            Label47.Visible = True
            Label58.Visible = True
            Label57.Visible = True
            Label66.Visible = True
            Label65.Visible = True
            TextBox14.Visible = True
            TextBox13.Visible = True
            TextBox16.Visible = True
            TextBox15.Visible = True
            TextBox18.Visible = True
            TextBox17.Visible = True
            Label42.Visible = True
            Label40.Visible = True
            Label56.Visible = True
            Label54.Visible = True
            Label64.Visible = True
            Label62.Visible = True
            Label41.Visible = True
            Label39.Visible = True
            Label55.Visible = True
            Label53.Visible = True
            Label63.Visible = True
            Label61.Visible = True
            Label72.Visible = True
            Label73.Visible = True
            Label74.Visible = True
            Label75.Visible = True
            Label76.Visible = True
            Label78.Visible = True

            Button38.Visible = True

            'Button32.Visible = True
            'Button32.Enabled = True
            'Label129.Visible = True
            'Label132.Visible = True
            'TextBox21.Visible = True
            'Label125.Visible = True

            'Label131.Visible = True 'False
            'TextBox22.Visible = True 'False
            'Label120.Visible = True 'False
            'Label137.Visible = True

            'Label130.Visible = True 'False
            'TextBox23.Visible = True 'False
            'Label127.Visible = True 'False
            'Label138.Visible = True

            'Label134.Visible = True 'False
            'TextBox24.Visible = True 'False
            'Label128.Visible = True 'False
            'Label139.Visible = True

            'Label135.Visible = True 'False
            'TextBox25.Visible = True 'False
            'Label133.Visible = True 'False
            'Label140.Visible = True

            'Label126.Visible = True 'False
            'Label136.Visible = True
            'GroupBox11.Visible = True 'False

            Panel2.Visible = False
            Button9.Text = "RECIPE EDIT"
            Button29.Text = "LOG OUT"
            RgsW(LoginKey)

        End If
        AxDBCommManager1.WriteDevice(DBPlcDevice.DKV5000XYM_D, LGMD_PLC, login_mode)

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
            FileClose()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
            FileClose()
        End Try

    End Sub

    Private Sub Button11_Click(sender As System.Object, e As System.EventArgs) Handles Button11.Click
        'PUMP
        If AxDBDeviceManager1.Devices(1).ValueRead = 0 Then
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "700", 1)
        Else
            If MessageBox.Show("PUMP OFF?", "", MessageBoxButtons.YesNo, MessageBoxIcon.Warning) = Windows.Forms.DialogResult.Yes Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "700", 1)
            End If
        End If

        Me.ActiveControl = Nothing

    End Sub

    Private Sub Button12_Click(sender As System.Object, e As System.EventArgs) Handles Button12.Click
        'ROUGH
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "711", 1)

        Me.ActiveControl = Nothing

    End Sub

    Private Sub Button13_Click(sender As System.Object, e As System.EventArgs) Handles Button13.Click
        'VENT
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "712", 1)

        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button16_Click(sender As System.Object, e As System.EventArgs) Handles Button16.Click
        'MW STAND-BY
        If AxDBDeviceManager1.Devices(49).ValueRead = 0 Then
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "701", 1)
        Else
            If MessageBox.Show("MW STAND-BY OFF?", "", MessageBoxButtons.YesNo, MessageBoxIcon.Warning) = Windows.Forms.DialogResult.Yes Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "701", 1)
            End If
        End If
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button15_Click(sender As System.Object, e As System.EventArgs) Handles Button15.Click
        'MW FWD POWER

        If AxDBDeviceManager1.Devices(7).ValueRead = 0 Then

            If TextBox10.Text > 0 Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "703", 1)
                Me.ActiveControl = Nothing
            End If

        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "703", 1)
            Me.ActiveControl = Nothing

        End If

    End Sub

    Private Sub Button17_Click(sender As System.Object, e As System.EventArgs) Handles Button17.Click
        'PRESS CHAMBER
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "704", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button14_Click(sender As System.Object, e As System.EventArgs) Handles Button14.Click
        'PLENUM
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "702", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button19_Click(sender As System.Object, e As System.EventArgs) Handles Button19.Click
        'GAS1
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "705", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button18_Click(sender As System.Object, e As System.EventArgs) Handles Button18.Click
        'GAS2
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "706", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button21_Click(sender As System.Object, e As System.EventArgs) Handles Button21.Click
        'GAS3
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "707", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button20_Click(sender As System.Object, e As System.EventArgs) Handles Button20.Click
        'GAS4
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "708", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button23_Click(sender As System.Object, e As System.EventArgs) Handles Button23.Click
        'GAS5
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "709", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button22_Click(sender As System.Object, e As System.EventArgs) Handles Button22.Click
        'GAS6
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "710", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button25_Click(sender As System.Object, e As System.EventArgs) Handles Button25.Click
        'CATE V7 CLOSE
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1201", 1)
    End Sub

    Private Sub Button26_Click(sender As System.Object, e As System.EventArgs) Handles Button26.Click
        'CATE V7 OPEN
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1200", 1)
    End Sub

    Private Sub Button24_Click(sender As System.Object, e As System.EventArgs) Handles Button24.Click
        'TMP
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "714", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button28_Click(sender As System.Object, e As System.EventArgs) Handles Button28.Click
        'BACK V6 CLOSE
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1115", 1)
    End Sub

    Private Sub Button27_Click(sender As System.Object, e As System.EventArgs) Handles Button27.Click
        'BACK V6 OPEN
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1114", 1)
    End Sub

    Private Sub Button1_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click
        'AUTO SEQUENCE START
        If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000XYM_M, "7101") = 1 Then
            MessageBox.Show("Automatic Pressure Recorder is busy. Can not start Recipe!")
            Exit Sub
        End If

        If AxDBDeviceManager1.Devices(40).ValueRead = 0 Then
            If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then

                frmEntrbtchInfo.ShowDialog()
                If frmEntrbtchInfo.BatchInfo_OK = False Then Exit Sub

                If MessageBox.Show("Are you sure to start Auto Sequence?", "", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                    If WLok = 0 Then
                        Dim m As Long = WaveLogger.GetWindowCount()  'GetState()
                        Dim s As Long = WaveLogger.GetState()

                        If m = 0 Then   'case of closing the WL_window
                            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 1)
                            If WL_warningDialog.ShowDialog = DialogResult.Ignore Then
                                'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 0)
                                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "805", 1)
                            Else
                                Exit Sub
                            End If
                        Else
                            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "805", 1)
                            If s <> 5 Then  'Case of stopping Wave logger
                                m = WaveLogger.Start()
                                If m <> 0 Then
                                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 1)
                                    frmWLalrm.Show()
                                    'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 0)
                                    Button10.Visible = True
                                Else
                                    Timer2.Enabled = True
                                End If
                            Else
                                Timer2.Enabled = True
                            End If
                        End If
                    Else
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 1)
                        If WL_warningDialog.ShowDialog = DialogResult.Ignore Then
                            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 0)
                            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "805", 1)
                        Else
                            Exit Sub
                        End If
                    End If
                End If
            End If
        Else
            If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "805", 1)
                'Try
                '    WaveLogger.Stop()
                '    WaveLogger = Nothing
                'Catch ex As Exception

                'End Try
            End If
        End If

        Me.ActiveControl = Nothing

    End Sub

    Private Function WavLogCreate() As Integer

        WaveLogger = CreateObject("WaveLogger2.Application")
        WaveLogger.Initialize()
        WaveLogger.Visible = Ctlvsbl.WavlogDsp

        Dim m As Long
        m = WaveLogger.SetIdentifier(0)

        If m <> 0 Then
            'MessageBox.Show("Can not open WaveLogger!")
            'WaveLogger.Quit()
            'WaveLogger = Nothing
            Return 1
        Else
            'Dim a As Long = WaveLogger.OpenFile("C:\Program Files (x86)\KEYENCE\WAVE LOGGER\default.xcf")
            Dim a As Long = WaveLogger.OpenFile("C:\SDS6NETSYS\default.xcf")
            Dim n As Long
            n = WaveLogger.GetUnitInfo(0)
            Dim l As Long
            l = WaveLogger.GetState()
            Dim k As Long
            k = WaveLogger.GetWindowCount()
            WaveLogger.ActivateWindow(1)
            Return 0
        End If

    End Function

    Private Sub Button2_Click(sender As System.Object, e As System.EventArgs) Handles Button2.Click
        'AUTO SEQENCE HOLD
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "806", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button5_Click(sender As System.Object, e As System.EventArgs) Handles Button5.Click
        'PROCESS TIMER GO
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "800", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button6_Click(sender As System.Object, e As System.EventArgs) Handles Button6.Click
        'PROCESS TIMER CLR
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "801", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button3_Click(sender As System.Object, e As System.EventArgs) Handles Button3.Click
        'BUZZER STOP
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "808", 1)
    End Sub

    Private Sub Button4_Click(sender As System.Object, e As System.EventArgs) Handles Button4.Click
        'ALARM RESET
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "802", 1)
    End Sub

    Private Sub Button7_Click(sender As System.Object, e As System.EventArgs) Handles Button7.Click
        'ALARM LOG
        Dim rslt As Integer
        rslt = Shell("C:\SDS6K\SDS6K-10kEALRM.EXE", AppWinStyle.NormalFocus)
    End Sub

    Private Sub Button8_Click(sender As System.Object, e As System.EventArgs) Handles Button8.Click
        'SYS MON
        Dim rslt As Integer
        rslt = Shell("C:\SDS6K\SDS6K-10kESYSMON.EXE", AppWinStyle.NormalFocus)
    End Sub

    Private Sub Button9_Click(sender As System.Object, e As System.EventArgs) Handles Button9.Click
        'RECIPE EDIT
        Dim rslt As Integer
        rslt = Shell("C:\SDS6K\SDS6K-10kERECIPE.EXE", AppWinStyle.NormalFocus)
        'Process.Start("C:\SDS6K\SDS6KRECIPE.EXE", LoginMode)

    End Sub

    Private Sub Button10_Click(sender As System.Object, e As System.EventArgs) Handles Button10.Click
        'WAVE LOG

        Dim rslt As Long

        WaveLogger.Quit()
        WaveLogger = Nothing
        WLok = WavLogCreate()
        'AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "805")
        If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000XYM_M, "907") = 1 Then 'Auto run Lmp
            If WLok = 0 Then
                rslt = WaveLogger.start()
                If rslt = 0 Then
                    Timer2.Enabled = True
                Else
                    MessageBox.Show("WaveLogger can not start !")
                End If
            Else
                MessageBox.Show("WaveLogger can not open !")
            End If
        End If

        If LoginMode = SUPRVISR Then
            Button10.Visible = True
        Else
            Button10.Visible = Ctlvsbl.WavLog_button
        End If


    End Sub

    Private Sub Button32_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'LOOP START
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1205", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button29_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'SKIP
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1206", 1)
    End Sub

    Private Sub Button30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'HEIGHT
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "810", 1)
        Else
            Exit Sub
        End If

        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button31_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'HEIGHT HOME
        If AxDBDeviceManager1.Devices(65).ValueRead = 0 Then

            If MessageBox.Show("Return to Origin?", "", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "811", 1)
            Else
                Exit Sub
            End If

        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "811", 1)

        End If
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button33_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'HEIGHT PRESET
        If MessageBox.Show("Z AXIS POSITION PRESET ?", "", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "812", 1)
        Else
            Exit Sub
        End If
        Me.ActiveControl = Nothing
    End Sub

    Private Sub Button34_Click(sender As System.Object, e As System.EventArgs) Handles Button34.Click
        'SEMI RAMP MAIN SWIICH  2017/5/16 Add
        If AxDBDeviceManager3.Devices(53).ValueRead = 0 Then
            'If MessageBox.Show("Are you sure to start SemiAuto Sequence?", "", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1300", 1)
            'End If
        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1300", 1)
        End If

        Me.ActiveControl = Nothing

    End Sub

    'SEMI RAMP START
    Private Sub Button35_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1302", 1)
    End Sub

    'SEMI RAMP TIMER START
    Private Sub Button36_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1303", 1)
    End Sub

    'SEMI RAMP TIMER RESET
    Private Sub Button37_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1314", 1)
    End Sub


    Private Sub AxDBDeviceManager1_AfterRead(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager1.AfterRead
        '-------------------------------------------------------
        '                   SYSTEM MONITOR
        '-------------------------------------------------------
        '================= AUTO SEQUENCE =======================
        'START
        If AxDBDeviceManager1.Devices(40).ValueRead = 1 Then    'MR907
            Button1.Text = "ABORT"
            Button1.BackColor = Color.Red

            'TextBox21.Text = 0
            TextBox13.Text = 0
            TextBox14.Text = 0
            TextBox15.Text = 0
            TextBox16.Text = 0
            TextBox17.Text = 0
            TextBox18.Text = 0
            TextBox10.Text = 0
            TextBox11.Text = 0
            TextBox12.Text = 0

        Else
            Button1.Text = "START"
            Button1.BackColor = Color.Green
            ''-------- WaveLogger stop----*2021/10/05 Comment out---
            'Try
            '    WaveLogger.Stop()
            '    Timer2.Enabled = False
            'Catch ex As Exception

            'End Try
        End If

        'HOLD
        If AxDBDeviceManager1.Devices(42).ValueRead = 1 Then
            Button2.Text = "RESUME"
            Button2.BackColor = SonCLR
        Else
            Button2.Text = "HOLD"
            Button2.BackColor = SofCLR
        End If

        '================= PROCESS TIMER =======================
        'GO
        If AxDBDeviceManager1.Devices(44).ValueRead = 1 Then
            Button5.Text = "OFF"
            Button5.BackColor = SonCLR
        Else
            Button5.Text = "GO"
            Button5.BackColor = SofCLR
        End If

        '====================== VACUUM =========================
        'PUMP
        If AxDBDeviceManager1.Devices(1).ValueRead = 1 Then
            Button11.Text = "STOP"
            Button11.BackColor = SonCLR
            Label14.BackColor = LonCLR
        Else
            Button11.Text = "START"
            Button11.BackColor = SofCLR
            Label14.BackColor = LofCLR
        End If

        'ROUGH
        If AxDBDeviceManager1.Devices(35).ValueRead = 1 Then
            Button12.Text = "STOP"
            Button12.BackColor = SonCLR
        Else
            Button12.Text = "START"
            Button12.BackColor = SofCLR
        End If

        If AxDBDeviceManager1.Devices(26).ValueRead = 1 Then    'M809 Rough sw
            If CSng(TextBox7.Text) > 1000 Then
                Rough_Stp = 1000
            Else
                Rough_Stp = CSng(TextBox7.Text)
            End If
            TextBox7.Text = Format(Rough_Stp, "0.0")
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1022", Rough_Stp * 10)
        End If

        If AxDBDeviceManager1.Devices(36).ValueRead = 1 Then    'M903 Rough Done
            Label12.BackColor = LonCLR
        Else
            Label12.BackColor = LofCLR
        End If

        'VENT
        If AxDBDeviceManager1.Devices(37).ValueRead = 1 Then    'M904
            Button13.Text = "STOP"
            Button13.BackColor = SonCLR
        Else
            Button13.Text = "START"
            Button13.BackColor = SofCLR
        End If

        If AxDBDeviceManager1.Devices(39).ValueRead = 1 Then    'M906 Vent Done
            Label15.BackColor = LonCLR
        Else
            Label15.BackColor = LofCLR
        End If

        VentCnt_Stp = Int(TextBox8.Text)
        TextBox8.Text = Format(VentCnt_Stp, "0")
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1040", VentCnt_Stp)

        '===================== MICRO-WAVE ======================
        'MW STAND-BY
        If AxDBDeviceManager1.Devices(31).ValueRead = 1 Then    'M814
            Button16.Text = "STOP"
            Button16.BackColor = SonCLR
        Else
            Button16.Text = "START"
            Button16.BackColor = SofCLR
        End If

        If AxDBDeviceManager1.Devices(3).ValueRead = 1 Then     'M702 MW STANDBY COMPLETE
            Label34.BackColor = LonCLR
        Else
            Label34.BackColor = LofCLR
        End If

        'FWD POWER
        'If AxDBDeviceManager3.Devices(72).ValueRead = 0 Then
        If AxDBDeviceManager1.Devices(7).ValueRead = 1 Then    'M814
            Button15.Text = "STOP"
            Button15.BackColor = SonCLR
        Else
            Button15.Text = "START"
            Button15.BackColor = SofCLR
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1002", 0)
        End If

        'End If

        If AxDBDeviceManager1.Devices(8).ValueRead = 1 Then    'M707
            If CSng(TextBox10.Text) > MwPrm.Max Then
                Mw_Stp = MwPrm.Max
            Else
                Mw_Stp = CSng(TextBox10.Text)
            End If
            TextBox10.Text = Format(Mw_Stp, "0")
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1002", Mw_Stp)
        End If

        If AxDBDeviceManager1.Devices(9).ValueRead = 1 Then    'M708 MW Done
            Label33.BackColor = LonCLR
        Else
            Label33.BackColor = LofCLR
        End If


        '===================== PRESSURE ======================
        'CHAMBER
        'If AxDBDeviceManager3.Devices(69).ValueRead = 0 Then
        If AxDBDeviceManager1.Devices(10).ValueRead = 1 Then    'M709 Semi Press
            Button17.Text = "STOP"
            Button17.BackColor = SonCLR
        Else
            Button17.Text = "START"
            Button17.BackColor = SofCLR
        End If
        'End If

        If AxDBDeviceManager1.Devices(11).ValueRead = 1 Then    'M710 Press pls
            If CSng(TextBox12.Text) > 1000 Then
                Pres_Stp = 1000
            Else
                Pres_Stp = CSng(TextBox12.Text)
            End If
            TextBox12.Text = Format(Pres_Stp, "0.0")
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1004", Pres_Stp * 10)
        End If

        If AxDBDeviceManager1.Devices(12).ValueRead = 1 Then    'M711 Pres Done
            Label46.BackColor = LonCLR
        Else
            Label46.BackColor = LofCLR
        End If

        'PLENUM
        If AxDBDeviceManager1.Devices(4).ValueRead = 1 Then     'M703 Semi Plenum
            Button14.Text = "STOP"
            Button14.BackColor = SonCLR
        Else
            Button14.Text = "START"
            Button14.BackColor = SofCLR
        End If

        If AxDBDeviceManager1.Devices(5).ValueRead = 1 Then     'M704 Plenum pls
            If CSng(TextBox11.Text) > 1000 Then
                Plnum_Stp = 1000
            Else
                Plnum_Stp = CSng(TextBox11.Text)
            End If
            TextBox11.Text = Format(Plnum_Stp, "0.0")
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1000", Plnum_Stp * 10)
        End If

        If AxDBDeviceManager1.Devices(6).ValueRead = 1 Then    'M705 Plenum Done
            Label45.BackColor = LonCLR
        Else
            Label45.BackColor = LofCLR
        End If


        '===================== GAS FLOW ======================

        'GAS1
        If AxDBDeviceManager1.Devices(13).ValueRead = 1 Then     'M712 Gas1
            Button19.Text = "STOP"
            Button19.BackColor = SonCLR
        Else
            Button19.Text = "START"
            Button19.BackColor = SofCLR
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1006", 0)
        End If

        If AxDBDeviceManager1.Devices(14).ValueRead = 1 Then     'M713 Gas1 pls
            If CSng(TextBox14.Text) > GasPrm.Fs(0) Then
                Gas1_Stp = GasPrm.Fs(0)
            Else
                Gas1_Stp = CSng(TextBox14.Text)
            End If
            TextBox14.Text = Format(Gas1_Stp, Dgstr(GasPrm.Decmals(0)))
            If GasPrm.Fs(0) <> 0 Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1006", Gas1_Stp * 10000 / GasPrm.Fs(0))
            End If
        End If

        If AxDBDeviceManager1.Devices(15).ValueRead = 1 Then     'M714 Gas1 Done
            Label50.BackColor = LonCLR
        Else
            Label50.BackColor = LofCLR
        End If


        'GAS2
        If AxDBDeviceManager1.Devices(16).ValueRead = 1 Then     'M715 Gas2
            Button18.Text = "STOP"
            Button18.BackColor = SonCLR
        Else
            Button18.Text = "START"
            Button18.BackColor = SofCLR
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1008", 0)
        End If

        If AxDBDeviceManager1.Devices(17).ValueRead = 1 Then     'M716 Gas2 pls
            If CSng(TextBox13.Text) > GasPrm.Fs(1) Then
                Gas2_Stp = GasPrm.Fs(1)
            Else
                Gas2_Stp = CSng(TextBox13.Text)
            End If
            TextBox13.Text = Format(Gas2_Stp, Dgstr(GasPrm.Decmals(1)))
            If GasPrm.Fs(1) <> 0 Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1008", Gas2_Stp * 10000 / GasPrm.Fs(1))
            End If
        End If

        If AxDBDeviceManager1.Devices(18).ValueRead = 1 Then     'M717 Gas2 Done
            Label47.BackColor = LonCLR
        Else
            Label47.BackColor = LofCLR
        End If


        'GAS3
        If AxDBDeviceManager1.Devices(19).ValueRead = 1 Then     'M802 Gas3
            Button21.Text = "STOP"
            Button21.BackColor = SonCLR
        Else
            Button21.Text = "START"
            Button21.BackColor = SofCLR
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1010", 0)
        End If

        If AxDBDeviceManager1.Devices(20).ValueRead = 1 Then     'M803 Gas3 pls
            If CSng(TextBox16.Text) > GasPrm.Fs(2) Then
                Gas3_Stp = GasPrm.Fs(2)
            Else
                Gas3_Stp = CSng(TextBox16.Text)
            End If
            TextBox16.Text = Format(Gas3_Stp, Dgstr(GasPrm.Decmals(2)))
            If GasPrm.Fs(2) <> 0 Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1010", Gas3_Stp * 10000 / GasPrm.Fs(2))
            End If
        End If

        If AxDBDeviceManager1.Devices(21).ValueRead = 1 Then     'M804 Gas3 Done
            Label58.BackColor = LonCLR
        Else
            Label58.BackColor = LofCLR
        End If


        'GAS4
        If AxDBDeviceManager1.Devices(22).ValueRead = 1 Then     'M805 Gas4
            Button20.Text = "STOP"
            Button20.BackColor = SonCLR
        Else
            Button20.Text = "START"
            Button20.BackColor = SofCLR
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1012", 0)
        End If

        If AxDBDeviceManager1.Devices(23).ValueRead = 1 Then     'M806 Gas4 pls
            If CSng(TextBox15.Text) > GasPrm.Fs(3) Then
                Gas4_Stp = GasPrm.Fs(3)
            Else
                Gas4_Stp = CSng(TextBox15.Text)
            End If
            TextBox15.Text = Format(Gas4_Stp, Dgstr(GasPrm.Decmals(3)))
            If GasPrm.Fs(3) <> 0 Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1012", Gas4_Stp * 10000 / GasPrm.Fs(3))
            End If
        End If

        If AxDBDeviceManager1.Devices(24).ValueRead = 1 Then     'M807 Gas4 Done
            Label57.BackColor = LonCLR
        Else
            Label57.BackColor = LofCLR
        End If


        'GAS5
        If AxDBDeviceManager1.Devices(25).ValueRead = 1 Then     'M808 Gas5
            Button23.Text = "STOP"
            Button23.BackColor = SonCLR
        Else
            Button23.Text = "START"
            Button23.BackColor = SofCLR
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1014", 0)
        End If

        If AxDBDeviceManager1.Devices(46).ValueRead = 1 Then     'M913 Gas5 pls
            If CSng(TextBox18.Text) > GasPrm.Fs(4) Then
                Gas5_Stp = GasPrm.Fs(4)
            Else
                Gas5_Stp = CSng(TextBox18.Text)
            End If
            TextBox18.Text = Format(Gas5_Stp, Dgstr(GasPrm.Decmals(4)))
            If GasPrm.Fs(4) <> 0 Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1014", Gas5_Stp * 10000 / GasPrm.Fs(4))
            End If
        End If

        If AxDBDeviceManager1.Devices(27).ValueRead = 1 Then     'M810 Gas5 Done
            Label66.BackColor = LonCLR
        Else
            Label66.BackColor = LofCLR
        End If


        'GAS6
        If AxDBDeviceManager1.Devices(28).ValueRead = 1 Then     'M811 Gas6
            Button22.Text = "STOP"
            Button22.BackColor = SonCLR
        Else
            Button22.Text = "START"
            Button22.BackColor = SofCLR
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1016", 0)
        End If

        If AxDBDeviceManager1.Devices(47).ValueRead = 1 Then     'M914 Gas6 pls
            If CSng(TextBox17.Text) > GasPrm.Fs(5) Then
                Gas6_Stp = GasPrm.Fs(5)
            Else
                Gas6_Stp = CSng(TextBox17.Text)
            End If
            TextBox17.Text = Format(Gas6_Stp, Dgstr(GasPrm.Decmals(5)))
            If GasPrm.Fs(5) <> 0 Then
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1016", Gas6_Stp * 10000 / GasPrm.Fs(5))
            End If
        End If

        If AxDBDeviceManager1.Devices(30).ValueRead = 1 Then     'M813 Gas6 Done
            Label65.BackColor = LonCLR
        Else
            Label65.BackColor = LofCLR
        End If




        'gruopbox()

        If AxDBDeviceManager1.Devices(66).ValueRead = 1 Then  'Height Control Box
            'Me.GroupBox12.Visible = False
            Me.Label149.Visible = False
        Else
            'Me.GroupBox12.Visible = True
            Me.Label48.Visible = False
            Me.Label38.Visible = False
            Me.Label32.Visible = False
            Me.Label71.Visible = False
            Me.Label45.Visible = False
            Me.Button14.Visible = False
            Me.TextBox11.Visible = False

        End If

        'If AxDBDeviceManager1.Devices(67).ValueRead = 1 Then  'Temp Control Box
        '    Me.GroupBox11.Visible = False
        'Else
        '    Me.GroupBox11.Visible = True
        'End If

        If AxDBDeviceManager1.Devices(68).ValueRead = 1 Then     'Add 2017/07/26 MW Power SaftyArea Lump
            Label199.BackColor = LonCLR
        Else
            Label199.BackColor = LofCLR
        End If

        If AxDBDeviceManager1.Devices(71).ValueRead = 1 Then     'Add 2023/04/20 Chamber SaftyArea Lump
            Label135.BackColor = LonCLR
        Else
            Label135.BackColor = LofCLR
        End If


        'Pyro Controll Disable in Parameter   2017/10/19

        'If AxDBDeviceManager1.Devices(69).ValueRead = 1 Then  'Height Control Box
        '    'Me.GroupBox11.Visible = False
        '    Me.Label125.Visible = False
        '    Me.Label120.Visible = False
        '    Me.Label127.Visible = False
        '    Me.Label128.Visible = False
        '    Me.Label133.Visible = False
        '    Me.Label126.Visible = False
        '    Me.Label129.Visible = False

        'Else
        '    If LoginMode = SUPRVISR Then
        '        'Me.GroupBox11.Enabled = True
        '        'Me.GroupBox11.Visible = True
        '        Me.Label125.Visible = True
        '        Me.Label120.Visible = True
        '        Me.Label127.Visible = True
        '        Me.Label128.Visible = True
        '        Me.Label133.Visible = True
        '        Me.Label126.Visible = True
        '        Me.Label129.Visible = True
        '    End If
        'End If






        '===================== TURBO MOLECULAR PUMP ======================

        'GATE V7
        If AxDBDeviceManager1.Devices(54).ValueRead = 1 Then     'R39008 Gate open LS
            Button25.BackColor = SonCLR 'Close Button
            Button26.BackColor = SofCLR 'Open Button
        Else
            Button25.BackColor = SofCLR 'Close Button
            Button26.BackColor = SonCLR 'Open Button
        End If

        If AxDBDeviceManager1.Devices(58).ValueRead = 1 Then     '
            Label82.BackColor = LonCLR  'Gate open ok
        Else
            Label82.BackColor = LofCLR
        End If


        'TMP
        If AxDBDeviceManager1.Devices(51).ValueRead = 1 Then     'M611 Turbo OK
            Button24.Text = "STOP"
            Button24.BackColor = SonCLR
            Label77.BackColor = LonCLR
        Else
            Button24.Text = "START"
            Button24.BackColor = SofCLR
            Label77.BackColor = LofCLR
        End If

        If AxDBDeviceManager1.Devices(56).ValueRead = 1 Then     'M414 Turbo ON Lmp
            Label77.BackColor = LonCLR
        Else
            Label77.BackColor = LofCLR
        End If

        If AxDBDeviceManager1.Devices(57).ValueRead = 1 Then     '
            Label83.BackColor = LonCLR  'accel
        Else
            Label83.BackColor = LofCLR
        End If

        If AxDBDeviceManager1.Devices(55).ValueRead = 1 Then     '
            Label84.BackColor = LonCLR  'normal
        Else
            Label84.BackColor = LofCLR
        End If



        'BACK V6
        If AxDBDeviceManager1.Devices(53).ValueRead = 1 Then     'R39007 Back open LS
            Button28.BackColor = SonCLR 'Close Button
            Button27.BackColor = SofCLR 'Open Button
        Else
            Button28.BackColor = SofCLR 'Close Button
            Button27.BackColor = SonCLR 'Open Button
        End If

        If AxDBDeviceManager1.Devices(59).ValueRead = 1 Then     '
            Label85.BackColor = LonCLR  'Back pres
        Else
            Label85.BackColor = LofCLR
        End If



        '===================== LOOP CONTROL ======================



        'If AxDBDeviceManager1.Devices(60).ValueRead = 1 Then    'M1410
        '    Button32.Text = "STOP"
        '    Button32.BackColor = SonCLR


        'Else
        '    Button32.Text = "START"
        '    Button32.BackColor = SofCLR
        'End If


        'If AxDBDeviceManager1.Devices(61).ValueRead = 1 Then    'M1411
        '    Label129.BackColor = LonCLR
        'Else
        '    Label129.BackColor = LofCLR
        'End If

        '===================== HEIGHT CONTROL ======================



        'If AxDBDeviceManager1.Devices(62).ValueRead = 1 Then    'M3800
        '    Button30.Text = "STOP"
        '    Button30.BackColor = SonCLR
        'Else
        '    Button30.Text = "START"
        '    Button30.BackColor = SofCLR
        'End If

        'If AxDBDeviceManager1.Devices(63).ValueRead = 1 Then    'M3709
        '    Label148.BackColor = Color.SkyBlue
        'Else
        '    Label148.BackColor = SofCLR
        'End If

        'If AxDBDeviceManager1.Devices(64).ValueRead = 1 Then    'M3606
        '    Label150.BackColor = LonCLR
        'Else
        '    Label150.BackColor = LofCLR
        'End If


        'If AxDBDeviceManager1.Devices(65).ValueRead = 1 Then    'M3801
        '    Button31.BackColor = SonCLR
        'Else
        '    Button31.BackColor = SofCLR
        'End If

        'If AxDBDeviceManager1.Devices(40).ValueRead = 1 Then
        '    TextBox26.Enabled = False
        'Else
        '    TextBox26.Enabled = Enabled
        'End If
        '    Height_time = CSng(AxDBDeviceManager2.Devices(80).ValueRead) / 10
        '    TextBox26.Text = Format(Height_time, "0.0")
        'Else
        '    Height_time = "5"
        '    TextBox26.Text = Format(Height_time, "0.0")
        'End If

        'If AxDBDeviceManager1.Devices(40).ValueRead = 1 Then
        '    Height_act = CSng(AxDBDeviceManager2.Devices(81).ValueRead) / 100
        '    TextBox27.Text = Format(Height_act, "0.00")
        'End If

        'If AxDBDeviceManager3.Devices(55).ValueRead = 1 Then                                'add 2017/5/22
        '    'Height_act = CSng(AxDBDeviceManager2.Devices(81).ValueRead) / 100
        '    TextBox27.Text = TextBox30.Text
        'End If



        Ramp_timeact = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6056") / 10
        'Label154.Text = Format(Ramp_timeact, "0.0")



        'AUTO LEAK
        If AxDBDeviceManager1.Devices(70).ValueRead = 1 Then    'M7101
            Button21.Enabled = False
            Button20.Enabled = False
            Button23.Enabled = False
            Button22.Enabled = False
            Button16.Enabled = False
            Button14.Enabled = False
            Button17.Enabled = False
            Button13.Enabled = False
            Button12.Enabled = False
            Button11.Enabled = False
            Button19.Enabled = False
            Button18.Enabled = False
            Button15.Enabled = False
            Button16.Enabled = False
            'Button32.Enabled = False

        Else
            Button21.Enabled = True
            Button20.Enabled = True
            Button23.Enabled = True
            Button22.Enabled = True
            Button16.Enabled = True
            Button14.Enabled = True
            Button17.Enabled = True
            Button13.Enabled = True
            Button12.Enabled = True
            Button11.Enabled = True
            Button19.Enabled = True
            Button18.Enabled = True
            Button15.Enabled = True
            Button16.Enabled = True
            'Button32.Enabled = True

        End If

        '================= Microwave Mode Select ======================= '2023/10/10 Sone 
        'When MW output is ON, can't select CW and Pulse, Plasma Size
        If AxDBDeviceManager1.Devices(7).ValueRead = 1 Then    'MR706
            Rbn_Mode_CW.Enabled = False
            Rbn_Mode_Pulse.Enabled = False
            Rbn_PUCK_2in.Enabled = False
            Rbn_PUCK_3in.Enabled = False
            Rbn_PUCK_4in.Enabled = False
            RadioButton_MWMode_Time.Enabled = False
            RadioButton_MWMode_Freq.Enabled = False
        Else
            Rbn_Mode_CW.Enabled = True
            Rbn_Mode_Pulse.Enabled = True
            Rbn_PUCK_2in.Enabled = True
            Rbn_PUCK_3in.Enabled = True
            Rbn_PUCK_4in.Enabled = True
            RadioButton_MWMode_Time.Enabled = True
            RadioButton_MWMode_Freq.Enabled = True
        End If

    End Sub

    Private Sub AxDBDeviceManager2_AfterRead(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager2.AfterRead
        '----------------------------------------------------------
        '                     DATA MONITOR
        '----------------------------------------------------------
        'PLENUM
        Plnum_Act = CSng(AxDBDeviceManager2.Devices(1).ValueRead) / 10   'D1000
        Plnum_Mes = CSng(AxDBDeviceManager2.Devices(2).ValueRead) / 10   'D1001
        Label38.Text = Format(Plnum_Act, "0.0")
        Label32.Text = Format(Plnum_Mes, "0.0")

        'MICRO WAVE  2017/08/04

        If AxDBDeviceManager2.Devices(93).ValueRead = 1 Then 'Safety area ON/OFF
            Mw_Act = CSng(AxDBDeviceManager2.Devices(92).ValueRead) / 1      'D1106
        Else
            Mw_Act = CSng(AxDBDeviceManager2.Devices(3).ValueRead) / 1      'D1002
        End If


        'Mw_Act = CSng(AxDBDeviceManager2.Devices(3).ValueRead) / 1      'D1002
        Mw_Mes = CSng(AxDBDeviceManager2.Devices(4).ValueRead) / 1     'DM1003 2023/3/30 Sone /10→/1に修正　10kWは/10不要
        Mw_Actramp = CSng(AxDBDeviceManager2.Devices(85).ValueRead)     'add 2017/5/23
        Label31.Text = Format(Mw_Act, "0")
        Label30.Text = Format(Mw_Mes, "0")
        'Label161.Text = Format(Mw_Actramp, "0")                     'add 2017/5/23
        'Label160.Text = Format(Mw_Mes, "0")                         'add 2017/5/23

        'PRESS
        Pres_Act = CSng(AxDBDeviceManager2.Devices(5).ValueRead) / 10     '2023/4/20 DM change 1004 to 1156
        Pres_Mes = CSng(AxDBDeviceManager2.Devices(6).ValueRead) / 10     'D1005
        Pres_Actramp = CSng(AxDBDeviceManager2.Devices(83).ValueRead) / 10     'D6062  'add 2017/5/23
        Label44.Text = Format(Pres_Act, "0.0")
        Label43.Text = Format(Pres_Mes, "0.0")
        Label21.Text = Format(Pres_Mes, "0.0")                            'Rough Mesured
        'Label157.Text = Format(Pres_Actramp, "0.0")                       'add 2017/5/23
        'Label156.Text = Format(Pres_Mes, "0.0")

        'MW REF
        MwRef_Act = CSng(AxDBDeviceManager2.Devices(21).ValueRead)           'D1020
        Label16.Text = Format(MwRef_Act, "0")
        MwRef_Mes = CSng(AxDBDeviceManager2.Devices(22).ValueRead)           'D1021
        Label17.Text = Format(MwRef_Mes, "0")
        AtRef_Mes = CSng(AxDBDeviceManager2.Devices(109).ValueRead)           'DM9918
        AT_REF_MES.Text = Format(AtRef_Mes, "0")

        '2023/10/27 Add the MW mode select function 
        'MW Pulse mode追加 Frequency, Duty Cycle 23/10/27 10kW-pulse
        'Frequency, Duty Cycleの表示が反対&エラーで赤点灯 23/2/28
        Freq_Act = CSng(AxDBDeviceManager2.Devices(102).ValueRead) / 1          'DM1216
        If MWMode_Time = True Then Tbx_MWMode_Freq.Text = Format(Freq_Act, "0")        '23/08/23 Revised

        If AxDBDeviceManager3.Devices(123).ValueRead = 1 And Tbx_MWMode_Freq_edit = False Then 'MR6402
            Tbx_MWMode_Freq.BackColor = Color.Red
        Else
            If MWMode_Time = True Then
                Tbx_MWMode_Freq.BackColor = Color.Khaki                                        '23/03/01 Color.Khaki
                'Tbx_MWMode_Freq.BackColor = Color.LightGray                                       '23/08/23 Revised
            Else
                If Tbx_MWMode_Freq_edit = True Then
                    Tbx_MWMode_Freq.BackColor = Color.Yellow
                Else
                    Tbx_MWMode_Freq.BackColor = SystemColors.ControlLight

                End If
            End If
        End If

        DutyCycle_Act = CSng(AxDBDeviceManager2.Devices(103).ValueRead) / 100       'DM1217
        If MWMode_Time = True Then Tbx_MWMode_Duty.Text = Format(DutyCycle_Act, "0.0")        '23/08/23 Revised


        If AxDBDeviceManager3.Devices(124).ValueRead = 1 And Tbx_MWMode_Duty_edit = False Then 'MR6403
            Tbx_MWMode_Duty.BackColor = Color.Red
        Else
            If MWMode_Time = True Then
                Tbx_MWMode_Duty.BackColor = Color.Khaki                                     '23/03/01 Color.Khaki
                'Tbx_MWMode_Duty.BackColor = Color.LightGray                                       '23/08/23 Revised
            Else
                If Tbx_MWMode_Duty_edit = True Then
                    Tbx_MWMode_Duty.BackColor = Color.Yellow
                Else
                    Tbx_MWMode_Duty.BackColor = SystemColors.ControlLight

                End If
            End If
        End If


        '-----------------------------------------23/10/27 Revised
        Toff_Act = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1278")) / 100
        If MWMode_Time = False Then Tbx_MWMode_TOFF.Text = Format(Toff_Act, "0.00")

        '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 2023/12/06 Sone If pulse input Toff error is occured, cell's color is red
        If AxDBDeviceManager3.Devices(133).ValueRead = 1 Then 'And Tbx_MWMode_Freq_edit = False Then 'MR6405
            Tbx_MWMode_TOFF.BackColor = Color.Red
        Else
            If MWMode_Time = False Then
                Tbx_MWMode_TOFF.BackColor = Color.Khaki                                        '23/03/01 Color.Khaki
                'Else
                'If Tbx_MWMode_TON_edit = True Then
                '    Tbx_MWMode_TON.BackColor = Color.Yellow
                'Else
                '    Tbx_MWMode_TON.BackColor = SystemColors.ControlLight

                'End If
            End If
        End If

        '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        Ton_Act = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1280")) / 100
        If MWMode_Time = False Then Tbx_MWMode_TON.Text = Format(Ton_Act, "0.00")

        '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 2023/12/06 Sone If pulse input Toff error is occured, cell's color is red
        If AxDBDeviceManager3.Devices(132).ValueRead = 1 Then 'And Tbx_MWMode_Freq_edit = False Then 'MR6404
            Tbx_MWMode_TON.BackColor = Color.Red
        Else
            If MWMode_Time = False Then
                Tbx_MWMode_TON.BackColor = Color.Khaki                                        '23/03/01 Color.Khaki
                'Else
                'If Tbx_MWMode_TON_edit = True Then
                '    Tbx_MWMode_TON.BackColor = Color.Yellow
                'Else
                '    Tbx_MWMode_TON.BackColor = SystemColors.ControlLight

                'End If
            End If
        End If
        '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        Mw_Ave = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1233") 'AVERAGE POWER: 0000 W
        Txt_MWMode_AverPw.Text = Mw_Ave
        'MW AVE
        'Mw_Ave = CSng(AxDBDeviceManager2.Devices(107).ValueRead) / 1     'D1003 2023/09/28 Sone Pulseモード時Average出力値表示追加
        Mw_Ave = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1233")   'DM1233 2023/10/10 Sone Pulseモード時Average出力値表示追加

        If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6406") = 1 Then
            Txt_MWMode_AverPw.Text = Format(Mw_Ave, "0")
        Else

            Txt_MWMode_AverPw.Text = "--------"  '2023/11/07 Sone CWモード時Average出力値は"--------"となるよう追加
        End If

        'PYRO
        Label8.Text = CStr(AxDBDeviceManager2.Devices(24).ValueRead) 'DM1023

        'PYRO2 2024/05/28 Pyro2 measured add to Control
        Txt_PYRO2_MES.Text = CStr(AxDBDeviceManager2.Devices(25).ValueRead) 'DM1024

        'Label126.Text = CStr(AxDBDeviceManager2.Devices(24).ValueRead)

        'HEIGHT
        Height_mes = CSng(AxDBDeviceManager2.Devices(79).ValueRead)
        Height_actramp = CSng(AxDBDeviceManager2.Devices(84).ValueRead) / 100    'add 2017/5/23
        'Label159.Text = Format(Height_actramp, "0.00")                           'add 2017/5/23
        'Label158.Text = Format(Height_mes, "0.00")                               'add 2017/5/23

        'PIRANI
        PIRANI2 = CStr(AxDBDeviceManager2.Devices(95).ValueRead)
        PIRANI3 = CStr(AxDBDeviceManager2.Devices(96).ValueRead)
        PIRANI4 = CStr(AxDBDeviceManager2.Devices(97).ValueRead)

        If PIRANI3 = 0 Then  '-----
            If PIRANI4 = 0 Then '.***

                If PIRANI2 = 1 Then
                    PIRANI1 = CStr((AxDBDeviceManager2.Devices(94).ValueRead) + 50000) / 100
                Else
                    PIRANI1 = CStr(AxDBDeviceManager2.Devices(94).ValueRead) / 100
                End If
                'Label203.Text = Format(PIRANI1, "0.00") + "0"
                Label203.Text = Format(PIRANI1, "0.0E+00")
            Else

                'If PIRANI2 = 1 Then
                'PIRANI1 = CStr((AxDBDeviceManager2.Devices(94).ValueRead) + 50000) / 100
                'Else
                PIRANI1 = CStr(AxDBDeviceManager2.Devices(94).ValueRead) / 1000
                'End If
                'Label203.Text = Format(PIRANI1, "0.000")
                Label203.Text = Format(PIRANI1, "0.0E+00")

            End If

        Else
            Label203.Text = "--------"

        End If


        Dim Hei As Single

        If Height_mes >= 32768 Then
            Hei = Format((65536 - Height_mes) * (-1), "0")
        Else
            Hei = Format(Height_mes, "0")

        End If

        Hei = Hei / 1000

        'Label147.Text = Format(Hei, "0.00")

        'Label158.Text = Format(Hei, "0.00") 'SEMIRAMP


        'STEP No
        TextBox1.Text = CStr(AxDBDeviceManager2.Devices(33).ValueRead)

        'STEPTIME
        'StepT = CLng(AxDBDeviceManager2.Devices(36).ValueRead)
        StepT = CLng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, 4242))
        StepT_min = Math.DivRem(StepT, 60, Stept_sec)
        TextBox2.Text = Format(StepT_min, "0")
        TextBox3.Text = Format(Stept_sec, "00")

        'PROCESS TIMER
        Dim hh As Integer
        Dim mm As Integer
        Dim ss As Integer

        hh = AxDBDeviceManager2.Devices(51).ValueRead
        mm = AxDBDeviceManager2.Devices(52).ValueRead
        ss = AxDBDeviceManager2.Devices(53).ValueRead

        TextBox4.Text = CStr(hh)
        TextBox5.Text = CStr(mm)
        TextBox6.Text = CStr(ss)


        'GAS1
        Gas1_Act = CSng(AxDBDeviceManager2.Devices(7).ValueRead) / 10000 * GasPrm.Fs(0)    'D1006
        Gas1_Mes = CSng(AxDBDeviceManager2.Devices(8).ValueRead) / 10000 * GasPrm.Fs(0)    'D1007
        Gas1_Actramp = CSng(AxDBDeviceManager2.Devices(86).ValueRead) / 10000 * GasPrm.Fs(0)     'add 2017/5/23
        Label42.Text = Format(Gas1_Act, Dgstr(GasPrm.Decmals(0)))
        Label41.Text = Format(Gas1_Mes, Dgstr(GasPrm.Decmals(0)))
        'Label163.Text = Format(Gas1_Actramp, Dgstr(GasPrm.Decmals(0)))
        'Label162.Text = Format(Gas1_Mes, Dgstr(GasPrm.Decmals(0)))


        'GAS2
        Gas2_Act = CSng(AxDBDeviceManager2.Devices(9).ValueRead) / 10000 * GasPrm.Fs(1)    'D1008
        Gas2_Mes = CSng(AxDBDeviceManager2.Devices(10).ValueRead) / 10000 * GasPrm.Fs(1)   'D1009
        Gas2_Actramp = CSng(AxDBDeviceManager2.Devices(87).ValueRead) / 10000 * GasPrm.Fs(1)     'add 2017/5/23
        Label40.Text = Format(Gas2_Act, Dgstr(GasPrm.Decmals(1)))
        Label39.Text = Format(Gas2_Mes, Dgstr(GasPrm.Decmals(1)))
        'Label165.Text = Format(Gas2_Actramp, Dgstr(GasPrm.Decmals(1)))
        'Label164.Text = Format(Gas2_Mes, Dgstr(GasPrm.Decmals(1)))

        'GAS3
        Gas3_Act = CSng(AxDBDeviceManager2.Devices(11).ValueRead) / 10000 * GasPrm.Fs(2)   'D1010
        Gas3_Mes = CSng(AxDBDeviceManager2.Devices(12).ValueRead) / 10000 * GasPrm.Fs(2)   'D1011
        Gas3_Actramp = CSng(AxDBDeviceManager2.Devices(88).ValueRead) / 10000 * GasPrm.Fs(2)     'add 2017/5/23
        Label56.Text = Format(Gas3_Act, Dgstr(GasPrm.Decmals(2)))
        Label55.Text = Format(Gas3_Mes, Dgstr(GasPrm.Decmals(2)))
        'Label167.Text = Format(Gas3_Actramp, Dgstr(GasPrm.Decmals(2)))
        'Label166.Text = Format(Gas3_Mes, Dgstr(GasPrm.Decmals(2)))

        'GAS4
        Gas4_Act = CSng(AxDBDeviceManager2.Devices(13).ValueRead) / 10000 * GasPrm.Fs(3)    'D1012
        Gas4_Mes = CSng(AxDBDeviceManager2.Devices(14).ValueRead) / 10000 * GasPrm.Fs(3)    'D1013
        Gas4_Actramp = CSng(AxDBDeviceManager2.Devices(89).ValueRead) / 10000 * GasPrm.Fs(3)     'add 2017/5/23
        Label54.Text = Format(Gas4_Act, Dgstr(GasPrm.Decmals(3)))
        Label53.Text = Format(Gas4_Mes, Dgstr(GasPrm.Decmals(3)))
        'Label169.Text = Format(Gas4_Actramp, Dgstr(GasPrm.Decmals(3)))
        'Label168.Text = Format(Gas4_Mes, Dgstr(GasPrm.Decmals(3)))

        'GAS5
        Gas5_Act = CSng(AxDBDeviceManager2.Devices(15).ValueRead) / 10000 * GasPrm.Fs(4)    'D1014
        Gas5_Mes = CSng(AxDBDeviceManager2.Devices(16).ValueRead) / 10000 * GasPrm.Fs(4)    'D1015
        Gas5_Actramp = CSng(AxDBDeviceManager2.Devices(90).ValueRead) / 10000 * GasPrm.Fs(4)     'add 2017/5/23
        Label64.Text = Format(Gas5_Act, Dgstr(GasPrm.Decmals(4)))
        Label63.Text = Format(Gas5_Mes, Dgstr(GasPrm.Decmals(4)))
        'Label171.Text = Format(Gas5_Actramp, Dgstr(GasPrm.Decmals(4)))
        'Label170.Text = Format(Gas5_Mes, Dgstr(GasPrm.Decmals(4)))

        'GAS6
        Gas6_Act = CSng(AxDBDeviceManager2.Devices(17).ValueRead) / 10000 * GasPrm.Fs(5)    'D1016
        Gas6_Mes = CSng(AxDBDeviceManager2.Devices(18).ValueRead) / 10000 * GasPrm.Fs(5)    'D1017
        Gas6_Actramp = CSng(AxDBDeviceManager2.Devices(91).ValueRead) / 10000 * GasPrm.Fs(5)     'add 2017/5/23
        Label62.Text = Format(Gas6_Act, Dgstr(GasPrm.Decmals(5)))
        Label61.Text = Format(Gas6_Mes, Dgstr(GasPrm.Decmals(5)))
        'Label173.Text = Format(Gas6_Actramp, Dgstr(GasPrm.Decmals(5)))
        'Label172.Text = Format(Gas6_Mes, Dgstr(GasPrm.Decmals(5)))

        'ROUGH
        Rough_Act = CSng(AxDBDeviceManager2.Devices(23).ValueRead) / 10   'D1019
        Label20.Text = Format(Rough_Act, "0.0")

        'VENT
        VentCount = AxDBDeviceManager2.Devices(27).ValueRead              'D1023
        Label25.Text = Format(VentCount, "0")

        'WATER
        'FS1 CHAMBER
        FS1flow = CSng(AxDBDeviceManager2.Devices(55).ValueRead) / 10
        Label105.Text = Format(FS1flow, "0.0")

        FS1temp = CSng(AxDBDeviceManager2.Devices(58).ValueRead) / 10
        Label108.Text = Format(FS1temp, "0.0")

        'FS2 MW PS
        FS2flow = CSng(AxDBDeviceManager2.Devices(56).ValueRead) / 10
        Label106.Text = Format(FS2flow, "0.0")

        FS2temp = CSng(AxDBDeviceManager2.Devices(59).ValueRead) / 10
        Label109.Text = Format(FS2temp, "0.0")

        'FS3 STAGE
        FS3flow = CSng(AxDBDeviceManager2.Devices(57).ValueRead) / 10
        Label107.Text = Format(FS3flow, "0.0")

        FS3temp = CSng(AxDBDeviceManager2.Devices(60).ValueRead) / 10
        Label113.Text = Format(FS3temp, "0.0")

        'FS4,5追加 23/2/1 10kW-pulse
        'FS4 MW Head
        FS4flow = CSng(AxDBDeviceManager2.Devices(98).ValueRead) / 10 'DM1064
        Label140.Text = Format(FS4flow, "0.0")

        FS4temp = CSng(AxDBDeviceManager2.Devices(99).ValueRead) / 10 'DM1065
        Label139.Text = Format(FS4temp, "0.0")

        'FS5 Tuner
        FS5flow = CSng(AxDBDeviceManager2.Devices(100).ValueRead) / 10 'DM1066
        Label155.Text = Format(FS5flow, "0.0")

        FS5temp = CSng(AxDBDeviceManager2.Devices(101).ValueRead) / 10 'DM1067
        Label154.Text = Format(FS5temp, "0.0")
        'FS6追加 23/9/26 Sone
        'FS6 SPARE
        FS6flow = CSng(AxDBDeviceManager2.Devices(69).ValueRead) / 10 'DM1080
        Label145.Text = Format(FS6flow, "0.0")

        FS6temp = CSng(AxDBDeviceManager2.Devices(108).ValueRead) / 10 'DM1081
        Label148.Text = Format(FS6temp, "0.0")

        ''PyroAlarm
        'PyroAlarmL = CSng(AxDBDeviceManager2.Devices(66).ValueRead)
        'PyroAlarmH = CSng(AxDBDeviceManager2.Devices(67).ValueRead)


        'LoopControl
        Looptemp = CSng(AxDBDeviceManager2.Devices(69).ValueRead)
        Looplimit = CSng(AxDBDeviceManager2.Devices(70).ValueRead)
        Loopramp = CSng(AxDBDeviceManager2.Devices(71).ValueRead)
        Looptime = CSng(AxDBDeviceManager2.Devices(72).ValueRead)
        Looprange = CSng(AxDBDeviceManager2.Devices(73).ValueRead)

        Looptemp_act = CSng(AxDBDeviceManager2.Devices(74).ValueRead)
        Looplimit_act = CSng(AxDBDeviceManager2.Devices(75).ValueRead)
        Loopramp_act = CSng(AxDBDeviceManager2.Devices(76).ValueRead)
        Looptime_act = CSng(AxDBDeviceManager2.Devices(77).ValueRead) / 10
        Looprange_act = CSng(AxDBDeviceManager2.Devices(78).ValueRead)

        'Label125.Text = Format(Looptemp_act, "0")
        'Label120.Text = Format(Looplimit_act, "0")
        'Label127.Text = Format(Loopramp_act, "0")
        'Label128.Text = Format(Looptime_act, "0")
        'Label133.Text = Format(Looprange_act, "0")


        If AxDBDeviceManager2.Devices(82).ValueRead = 39321 Then  'PLC Lock
            Exit Sub
        Else
            Application.Exit()
        End If

    End Sub

    Private Sub AxDBDeviceManager3_AfterRead(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager3.AfterRead
        '------------------------------------------------------------
        '                      ALARM MONITOR
        '------------------------------------------------------------
        'ROUGH
        If AxDBDeviceManager3.Devices(35).ValueRead = 1 Then
            Label87.BackColor = AonCLR  'soft pumping
        Else
            Label87.BackColor = AofCLR
        End If

        If AxDBDeviceManager3.Devices(36).ValueRead = 1 Then
            Label88.BackColor = AonCLR  'rough
        Else
            Label88.BackColor = AofCLR
        End If

        'PLENUM
        If AxDBDeviceManager3.Devices(40).ValueRead = 1 Then
            Label71.BackColor = AonCLR  'div
        Else
            Label71.BackColor = AofCLR
        End If

        'MW
        If AxDBDeviceManager3.Devices(26).ValueRead = 1 Then
            Label69.BackColor = AonCLR  'div
        Else
            Label69.BackColor = AofCLR
        End If

        'CHAMBER WATER
        If AxDBDeviceManager3.Devices(32).ValueRead = 1 Then
            Label119.BackColor = AonCLR  'fs1 chamber
        Else
            Label119.BackColor = AofCLR
        End If

        'MW PS WATER
        If AxDBDeviceManager3.Devices(38).ValueRead = 1 Then
            Label121.BackColor = AonCLR  'fs2 Mw PS
        Else
            Label121.BackColor = AofCLR
        End If

        'STAGE WATER
        If AxDBDeviceManager3.Devices(39).ValueRead = 1 Then
            Label127.BackColor = AonCLR  'fs3 stage
        Else
            Label127.BackColor = AofCLR
        End If

        'FS4, 5追加     23/2/1 10kW-pulse
        'FS4 MW Head WATER
        If AxDBDeviceManager3.Devices(119).ValueRead = 1 Then 'MR600
            Label130.BackColor = AonCLR  'fs4 Mw head
        Else
            Label130.BackColor = AofCLR
        End If

        'FS5 Tuner WATER
        If AxDBDeviceManager3.Devices(120).ValueRead = 1 Then 'MR601
            Label131.BackColor = AonCLR  'fs5 tuner
        Else
            Label131.BackColor = AofCLR
        End If

        'FS6追加 23/9/26 Sone
        'FS6 SPARE
        If AxDBDeviceManager3.Devices(125).ValueRead = 1 Then 'MR602
            Label143.BackColor = AonCLR  'fs6 Spare
        Else
            Label143.BackColor = AofCLR
        End If

        'REF
        If AxDBDeviceManager3.Devices(28).ValueRead = 1 Then
            Label91.BackColor = AonCLR  'refrection
        Else
            Label91.BackColor = AofCLR
        End If

        'PRESS
        If AxDBDeviceManager3.Devices(33).ValueRead = 1 Then
            Label70.BackColor = AonCLR  'div
        Else
            Label70.BackColor = AofCLR
        End If

        'PROCESS LIMIT
        If AxDBDeviceManager3.Devices(42).ValueRead = 1 Then
            Label92.BackColor = AonCLR  'Process Limit
        Else
            Label92.BackColor = AofCLR
        End If

        'ATM
        If AxDBDeviceManager3.Devices(45).ValueRead = 1 Then
            Label93.BackColor = AonCLR  'atmosphere
        Else
            Label93.BackColor = AofCLR
        End If

        'GAS1
        If AxDBDeviceManager3.Devices(10).ValueRead = 1 Then
            Label72.BackColor = AonCLR  'div
        Else
            Label72.BackColor = AofCLR
        End If

        If AxDBDeviceManager3.Devices(22).ValueRead = 1 Then
            Label95.BackColor = AonCLR  'O2/H2
        Else
            Label95.BackColor = AofCLR
        End If

        If AxDBDeviceManager3.Devices(24).ValueRead = 1 Then
            Label94.BackColor = AonCLR  'manifold
        Else
            Label94.BackColor = AofCLR
        End If

        'GAS2
        If AxDBDeviceManager3.Devices(12).ValueRead = 1 Then
            Label73.BackColor = AonCLR  'div
        Else
            Label73.BackColor = AofCLR
        End If

        'GAS3
        If AxDBDeviceManager3.Devices(14).ValueRead = 1 Then
            Label74.BackColor = AonCLR  'div
        Else
            Label74.BackColor = AofCLR
        End If

        'GAS4
        If AxDBDeviceManager3.Devices(16).ValueRead = 1 Then
            Label75.BackColor = AonCLR  'div
        Else
            Label75.BackColor = AofCLR
        End If

        'GAS5
        If AxDBDeviceManager3.Devices(18).ValueRead = 1 Then
            Label76.BackColor = AonCLR  'div
        Else
            Label76.BackColor = AofCLR
        End If

        'GAS6
        If AxDBDeviceManager3.Devices(20).ValueRead = 1 Then
            Label78.BackColor = AonCLR  'div
        Else
            Label78.BackColor = AofCLR
        End If

        'PUMP
        If AxDBDeviceManager3.Devices(4).ValueRead = 1 Then
            Label86.BackColor = AonCLR  'pump abnormal
        Else
            Label86.BackColor = AofCLR
        End If

        'EMO
        If AxDBDeviceManager3.Devices(1).ValueRead = 1 Then
            Label97.BackColor = AonCLR  'emo
        Else
            Label97.BackColor = AofCLR
        End If

        'EXT
        If AxDBDeviceManager3.Devices(3).ValueRead = 1 Then
            Label98.BackColor = AonCLR  'ext interlock
        Else
            Label98.BackColor = AofCLR
        End If

        'CDA
        If AxDBDeviceManager3.Devices(2).ValueRead = 1 Then
            Label100.BackColor = AonCLR  'low cda
        Else
            Label100.BackColor = AofCLR
        End If

        'MW ABNORMAL
        If AxDBDeviceManager3.Devices(31).ValueRead = 1 Then
            Label90.BackColor = AonCLR  'Mw abnormal
        Else
            Label90.BackColor = AofCLR
        End If

        'PLC ABNORMAL
        If AxDBDeviceManager3.Devices(5).ValueRead = 1 Then
            Label99.BackColor = AonCLR  'plc abnormal
        Else
            Label99.BackColor = AofCLR
        End If

        'MW COMM ERR
        If AxDBDeviceManager3.Devices(23).ValueRead = 1 Then
            Label89.BackColor = AonCLR  'Mw comm err
        Else
            Label89.BackColor = AofCLR
        End If

        'N2 PURGE
        If AxDBDeviceManager3.Devices(7).ValueRead = 1 Then
            Label101.BackColor = AonCLR  'ATC LIMIT
        Else
            Label101.BackColor = AofCLR
        End If

        'TMP ABNORMAL
        If AxDBDeviceManager3.Devices(8).ValueRead = 1 Then
            Label96.BackColor = AonCLR  'tmp failure
        Else
            Label96.BackColor = AofCLR
        End If

        'TEMPREATURE 
        If AxDBDeviceManager3.Devices(51).ValueRead = 1 Then
            Label123.BackColor = AonCLR  'temperature failure
        Else
            Label123.BackColor = AofCLR
        End If

        'Autotuner Alarm Indicator追加 23/9/26 Sone
        'AUTOTUNER ABNORMAL
        If AxDBDeviceManager3.Devices(126).ValueRead = 1 Then
            Label142.BackColor = AonCLR  'Autotuner Abnormal
        Else
            Label142.BackColor = AofCLR
        End If

        'PYRO High Warning
        If AxDBDeviceManager3.Devices(127).ValueRead = 1 Then
            TextBox19.BackColor = AonCLR  'temperature failure
        End If

        'PYRO High Alarm
        If AxDBDeviceManager3.Devices(128).ValueRead = 1 Then
            TextBox20.BackColor = AonCLR
        End If

        'PYRO High Warning
        If AxDBDeviceManager3.Devices(129).ValueRead = 1 Then
            TextBox38.BackColor = AonCLR
        End If

        'PYRO High Alarm
        If AxDBDeviceManager3.Devices(130).ValueRead = 1 Then
            TextBox39.BackColor = AonCLR
        End If


        'Z AXIS MOTOR
        If AxDBDeviceManager3.Devices(52).ValueRead = 1 Then
            Label149.BackColor = AonCLR
        Else
            Label149.BackColor = AofCLR
        End If

        'WAVE LOGGER ABNORMAL
        If AxDBDeviceManager3.Devices(80).ValueRead = 1 Then
            Label204.BackColor = AonCLR  'Wave logger abnormal
        Else
            Label204.BackColor = AofCLR
        End If

        '////// ALARM RUN //////
        If AxDBDeviceManager3.Devices(48).ValueRead = 1 Then
            Dim ret As Integer
            ret = Shell("C:\SDS6K\SDS6K-10kEALRM.EXE") '2023/10/24 File name revised
            System.Threading.Thread.Sleep(2000)
        End If

        '////// 2023/11/30 Sone Harmonics Warning Display //////
        If AxDBDeviceManager3.Devices(131).ValueRead = 1 Then
            Pbx_Hrm_Warn.Visible = True
        Else
            Pbx_Hrm_Warn.Visible = False
        End If

    End Sub

    Private Function Check_isNumeric(ByVal txtbox As TextBox, ByVal devnum As String, ByVal rate As Single) As Boolean
        'CHECK SET DATA IS NUMERIC or NOT
        Dim inum As Integer
        Dim rzlt As Boolean

        rzlt = IsNumeric(txtbox.Text)
        If rzlt = False Then
            Try
                inum = CInt(txtbox.Text)
                If inum = 0 Then
                    Return True
                End If
            Catch ex As Exception

            End Try

            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devnum))
            If rate = 1 Then txtbox.Text = Format(rd / 1, "0")
            If rate = 10 Then txtbox.Text = Format(rd / 10, "0.0")
            If rate = 100 Then txtbox.Text = Format(rd / 100, "0.00")
            If rate = 10000 Then
                If devnum = "1006" Then txtbox.Text = Format(rd * GasPrm.Fs(0) / 10000, Dgstr(GasPrm.Decmals(0)))
                If devnum = "1008" Then txtbox.Text = Format(rd * GasPrm.Fs(1) / 10000, Dgstr(GasPrm.Decmals(1)))
                If devnum = "1010" Then txtbox.Text = Format(rd * GasPrm.Fs(2) / 10000, Dgstr(GasPrm.Decmals(2)))
                If devnum = "1012" Then txtbox.Text = Format(rd * GasPrm.Fs(3) / 10000, Dgstr(GasPrm.Decmals(3)))
                If devnum = "1014" Then txtbox.Text = Format(rd * GasPrm.Fs(4) / 10000, Dgstr(GasPrm.Decmals(4)))
                If devnum = "1016" Then txtbox.Text = Format(rd * GasPrm.Fs(5) / 10000, Dgstr(GasPrm.Decmals(5)))
            End If
            Return False
        Else
            Return True
        End If

    End Function


    Private Sub TextBox7_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox7.KeyPress
        'ROUGH SETPOINT
        If e.KeyChar = vbCr Then
            If Check_isNumeric(TextBox7, "1022", 10) = False Then Exit Sub
            Rough_Stp = Val(TextBox7.Text)
            If Rough_Stp > 1000 Then Rough_Stp = 1000
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1022", Rough_Stp * 10)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1022") / 10)
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3310", 1)
            TextBox7.BackColor = SystemColors.ControlLight
            TextBox7.Text = Format(rd, "0.0")
        End If
    End Sub
    Private Sub TextBox7_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox7.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3110", 1)
            TextBox7.BackColor = Color.Yellow
        End If

    End Sub

    Private Sub TextBox8_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox8.KeyPress
        'VENT vacuum
        If e.KeyChar = vbCr Then
            If Check_isNumeric(TextBox8, "1040", 1) = False Then Exit Sub
            VentCnt_Stp = CSng(TextBox8.Text)
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1040", VentCnt_Stp)
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1040"))      'Bug corrected 2023/10/21
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3408", 1)
            TextBox8.BackColor = SystemColors.ControlLight
            TextBox8.Text = Format(rd, "0")
        End If
    End Sub
    Private Sub TextBox8_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox8.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3208", 1)
            TextBox8.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox9_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox9.KeyPress
        'MW REF LIMIT SET

        If e.KeyChar = vbCr Then
            If Check_isNumeric(TextBox9, "1020", 1) = False Then Exit Sub
            If CSng(TextBox10.Text) > MwPrm.Max Then
                MwRef_Stp = MwPrm.Max
            Else
                MwRef_Stp = CSng(TextBox9.Text)
            End If
            TextBox9.Text = Format(MwRef_Stp, "0")
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1020", MwRef_Stp)

            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1020"))
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3314", 1)
            TextBox9.BackColor = SystemColors.ControlLight
            TextBox9.Text = Format(rd, "0")
        End If
    End Sub
    Private Sub TextBox9_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox9.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3114", 1)
            TextBox9.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox10_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox10.KeyPress
        'FWD POWER SETPOINT
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox10, "1002", 1) = False Then Exit Sub
                If CSng(TextBox10.Text) > MwPrm.Max Then
                    Mw_Stp = MwPrm.Max
                Else
                    Mw_Stp = CSng(TextBox10.Text)
                End If
                TextBox10.Text = Format(Mw_Stp, "0")
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1002", Mw_Stp)
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "5302", 1)
                Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1002"))
                'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3313", 1)
                TextBox10.BackColor = SystemColors.ControlLight
                TextBox10.Text = Format(rd, "0")
            End If
        End If
    End Sub
    Private Sub TextBox10_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox10.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3113", 1)
            TextBox10.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox12_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox12.KeyPress
        'PRES CHAMBER SETPOINT
        'TmpCtl busy > R5300
        Dim tmpctlbsy As Integer = 0
        tmpctlbsy = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "5300")
        If tmpctlbsy = 1 Then
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1004"))
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3311", 1)
            TextBox12.BackColor = SystemColors.ControlLight
            TextBox12.Text = Format(rd / 10, "0.0")
            Exit Sub
        End If

        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox12, "1004", 10) = False Then Exit Sub
                If CSng(TextBox12.Text) > 1000 Then
                    Pres_Stp = 1000
                Else
                    Pres_Stp = CSng(TextBox12.Text)
                End If
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1004", Pres_Stp * 10)
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "5303", 1)
                Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1004"))
                'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3311", 1)
                TextBox12.BackColor = SystemColors.ControlLight
                TextBox12.Text = Format(rd / 10, "0.0")
            End If
        End If
    End Sub
    Private Sub TextBox12_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox12.KeyDown 'key input yellow

        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3111", 1)
            TextBox12.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox11_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox11.KeyPress
        'PRES PLENUM SETPOINT
        'TmpCtl busy > R5300
        Dim tmpctlbsy As Integer = 0
        tmpctlbsy = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "5300")
        If tmpctlbsy = 1 Then
            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1000"))
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3311", 1)
            TextBox11.BackColor = SystemColors.ControlLight
            TextBox11.Text = Format(rd / 10, "0.0")
            Exit Sub
        End If
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox11, "1000", 10) = False Then Exit Sub
                If CSng(TextBox11.Text) > 1000 Then
                    Plnum_Stp = 1000
                Else
                    Plnum_Stp = CSng(TextBox11.Text)
                End If
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1000", Plnum_Stp * 10)
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "5304", 1)
                Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1000"))
                'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3312", 1)
                TextBox11.BackColor = SystemColors.ControlLight
                TextBox11.Text = Format(rd / 10, "0.0")
            End If
        End If
    End Sub
    Private Sub TextBox11_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox11.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3112", 1)
            TextBox11.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox14_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox14.KeyPress
        'GAS1 SETPOINT
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox14, "1006", 10000) = False Then Exit Sub
                If CSng(TextBox14.Text) > GasPrm.Fs(0) Then
                    Gas1_Stp = GasPrm.Fs(0)
                Else
                    Gas1_Stp = CSng(TextBox14.Text)
                End If
                If GasPrm.Fs(0) <> 0 Then
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1006", Gas1_Stp * 10000 / GasPrm.Fs(0))
                    Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1006"))
                    'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3401", 1)
                    TextBox14.BackColor = SystemColors.ControlLight
                    TextBox14.Text = Format(rd * GasPrm.Fs(0) / 10000, Dgstr(GasPrm.Decmals(0)))
                End If
            End If
        End If
    End Sub
    Private Sub TextBox14_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox14.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3201", 1)
            TextBox14.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox13_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox13.KeyPress
        'GAS2 SETPOINT
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox13, "1008", 10000) = False Then Exit Sub
                If CSng(TextBox13.Text) > GasPrm.Fs(1) Then
                    Gas2_Stp = GasPrm.Fs(1)
                Else
                    Gas2_Stp = CSng(TextBox13.Text)
                End If
                If GasPrm.Fs(1) <> 0 Then
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1008", Gas2_Stp * 10000 / GasPrm.Fs(1))
                    Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1008"))
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3402", 1)
                    TextBox13.BackColor = SystemColors.ControlLight
                    TextBox13.Text = Format(rd * GasPrm.Fs(1) / 10000, Dgstr(GasPrm.Decmals(1)))
                End If
            End If
        End If
    End Sub
    Private Sub TextBox13_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox13.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3202", 1)
            TextBox13.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox16_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox16.KeyPress
        'GAS3 SETPOINT
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox16, "1010", 10000) = False Then Exit Sub
                If CSng(TextBox16.Text) > GasPrm.Fs(2) Then
                    Gas3_Stp = GasPrm.Fs(2)
                Else
                    Gas3_Stp = CSng(TextBox16.Text)
                End If
                If GasPrm.Fs(2) <> 0 Then
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1010", Gas3_Stp * 10000 / GasPrm.Fs(2))
                    Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1010"))
                    'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3403", 1)
                    TextBox16.BackColor = SystemColors.ControlLight
                    TextBox16.Text = Format(rd * GasPrm.Fs(2) / 10000, Dgstr(GasPrm.Decmals(2)))
                End If
            End If
        End If
    End Sub
    Private Sub TextBox16_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox16.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3203", 1)
            TextBox16.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox15_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox15.KeyPress
        'GAS4 SETPOINT
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox15, "1012", 10000) = False Then Exit Sub
                If CSng(TextBox15.Text) > GasPrm.Fs(3) Then
                    Gas4_Stp = GasPrm.Fs(3)
                Else
                    Gas4_Stp = CSng(TextBox15.Text)
                End If
                If GasPrm.Fs(3) <> 0 Then
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1012", Gas4_Stp * 10000 / GasPrm.Fs(3))
                    Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1012"))
                    'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3404", 1)
                    TextBox15.BackColor = SystemColors.ControlLight
                    TextBox15.Text = Format(rd * GasPrm.Fs(3) / 10000, Dgstr(GasPrm.Decmals(3)))
                End If
            End If
        End If
    End Sub
    Private Sub TextBox15_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox15.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3204", 1)
            TextBox15.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox18_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox18.KeyPress
        'GAS5 SETPOINT
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox18, "1014", 10000) = False Then Exit Sub
                If CSng(TextBox18.Text) > GasPrm.Fs(4) Then
                    Gas5_Stp = GasPrm.Fs(4)
                Else
                    Gas5_Stp = CSng(TextBox18.Text)
                End If
                If GasPrm.Fs(4) <> 0 Then
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1014", Gas5_Stp * 10000 / GasPrm.Fs(4))
                    Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1014"))
                    'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3405", 1)
                    TextBox18.BackColor = SystemColors.ControlLight
                    TextBox18.Text = Format(rd * GasPrm.Fs(4) / 10000, Dgstr(GasPrm.Decmals(4)))
                End If
            End If
        End If
    End Sub
    Private Sub TextBox18_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox18.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3205", 1)
            TextBox18.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox17_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox17.KeyPress
        'GAS6 SETPOINT
        If AxDBDeviceManager3.Devices(55).ValueRead = 0 Then
            If e.KeyChar = vbCr Then
                If Check_isNumeric(TextBox17, "1016", 10000) = False Then Exit Sub
                If CSng(TextBox17.Text) > GasPrm.Fs(5) Then
                    Gas6_Stp = GasPrm.Fs(5)
                Else
                    Gas6_Stp = CSng(TextBox17.Text)
                End If
                If GasPrm.Fs(5) <> 0 Then
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1016", Gas6_Stp * 10000 / GasPrm.Fs(5))
                    Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1016"))
                    'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3406", 1)
                    TextBox17.BackColor = SystemColors.ControlLight
                    TextBox17.Text = Format(rd * GasPrm.Fs(5) / 10000, Dgstr(GasPrm.Decmals(5)))
                End If
            End If
        End If
    End Sub
    Private Sub TextBox17_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox17.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3206", 1)
            TextBox17.BackColor = Color.Yellow
        End If
    End Sub


    Private Sub TextBox19_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox19.KeyPress
        'PyroWarning(H)
        If e.KeyChar = vbCr Then
            If IsNumeric(TextBox19.Text) = False Then
            Else
                Dim sp As Integer = CInt(TextBox19.Text)
                If sp < 0 Then
                    sp = 0
                ElseIf sp > 9999 Then
                    sp = 9999
                End If
                'Case 66 : wrval = PyrAlm.HigWarSP
                'Case 67 : wrval = PyrAlm.HigAlmSP
                'Case 68 : wrval = PyrAlm.LowWarSP
                'Case 69 : wrval = PyrAlm.LowAlmSP
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "165", sp)
            End If

            Dim rd As Integer = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "165")
            PyroWarH = rd
            TextBox19.Text = rd
            TextBox19.BackColor = SystemColors.ControlLight
            'Timer1.Enabled = True  2022/10/20 PYRO入力時動作修正
            Timer1.Enabled = True '2023/11/6 PYRO入力時動作修正
            'TextBox20.Enabled = True
            'TextBox38.Enabled = True
            'TextBox39.Enabled = True
        End If

    End Sub

    Private Sub TextBox19_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox19.KeyDown 'key input yellow
        'PyroWarning(H)
        If e.KeyData = Keys.A Then
        Else
            TextBox19.BackColor = Color.Yellow
            'Timer1.Enabled = False 2022/10/20 PYRO入力時動作修正
            Timer1.Enabled = False '2023/11/6 PYRO入力時動作修正
        End If
    End Sub


    Private Sub TextBox20_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox20.KeyPress
        'PyroAlarm(H)
        If e.KeyChar = vbCr Then
            If IsNumeric(TextBox20.Text) = False Then
            Else
                Dim sp As Integer = CInt(TextBox20.Text)
                If sp < 0 Then
                    sp = 0
                ElseIf sp > 9999 Then
                    sp = 9999
                End If
                'Case 66 : wrval = PyrAlm.HigWarSP
                'Case 67 : wrval = PyrAlm.HigAlmSP
                'Case 68 : wrval = PyrAlm.LowWarSP
                'Case 69 : wrval = PyrAlm.LowAlmSP
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "166", sp)
            End If

            Dim rd As Integer = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "166")
            PyroAlarmH = rd
            TextBox20.Text = rd
            TextBox20.BackColor = SystemColors.ControlLight
            'Timer1.Enabled = True  2022/10/20 PYRO入力時動作修正
            Timer1.Enabled = True '2023/11/6 PYRO入力時動作修正
            'TextBox19.Enabled = True
            'TextBox38.Enabled = True
            'TextBox39.Enabled = True
        End If

    End Sub

    Private Sub TextBox20_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox20.KeyDown 'key input yellow
        'PyroAlarm(H)
        If e.KeyData = Keys.A Then
        Else
            TextBox20.BackColor = Color.Yellow
            'Timer1.Enabled = False  2022/10/20 PYRO入力時動作修正
            'TextBox19.Enabled = False
            'TextBox38.Enabled = False
            'TextBox39.Enabled = False
            Timer1.Enabled = False '2023/11/6 PYRO入力時動作修正
        End If
    End Sub

    Private Sub TextBox38_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox38.KeyPress
        'PyroWarning(Lo)
        If e.KeyChar = vbCr Then
            If IsNumeric(TextBox38.Text) = False Then
            Else
                Dim sp As Integer = CInt(TextBox38.Text)
                If sp < 0 Then
                    sp = 0
                ElseIf sp > 9999 Then
                    sp = 9999
                End If
                'Case 66 : wrval = PyrAlm.HigWarSP
                'Case 67 : wrval = PyrAlm.HigAlmSP
                'Case 68 : wrval = PyrAlm.LowWarSP
                'Case 69 : wrval = PyrAlm.LowAlmSP
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "167", sp)
            End If

            Dim rd As Integer = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "167")
            PyroWarL = rd
            TextBox38.Text = rd
            TextBox38.BackColor = SystemColors.ControlLight
            Timer1.Enabled = True
        End If

    End Sub

    Private Sub TextBox38_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox38.KeyDown 'key input yellow
        'PyroWarning(Lo)
        If e.KeyData = Keys.A Then
        Else
            TextBox38.BackColor = Color.Yellow
            Timer1.Enabled = False
        End If
    End Sub

    Private Sub TextBox39_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox39.KeyPress
        'PyroAlarm(Lo)
        If e.KeyChar = vbCr Then
            If IsNumeric(TextBox39.Text) = False Then
            Else
                Dim sp As Integer = CInt(TextBox39.Text)
                If sp < 0 Then
                    sp = 0
                ElseIf sp > 9999 Then
                    sp = 9999
                End If
                'Case 66 : wrval = PyrAlm.HigWarSP
                'Case 67 : wrval = PyrAlm.HigAlmSP
                'Case 68 : wrval = PyrAlm.LowWarSP
                'Case 69 : wrval = PyrAlm.LowAlmSP
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "168", sp)
            End If

            Dim rd As Integer = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "168")
            PyroAlarmL = rd
            TextBox39.Text = rd
            TextBox39.BackColor = SystemColors.ControlLight
            Timer1.Enabled = True
        End If

    End Sub

    Private Sub TextBox39_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles TextBox39.KeyDown 'key input yellow
        'PyroAlarm(Lo)
        If e.KeyData = Keys.A Then
        Else
            TextBox39.BackColor = Color.Yellow
            Timer1.Enabled = False
        End If
    End Sub

    'Private Sub TextBox23_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'LOOP SET RATE
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox23, "1084", 1) = False Then Exit Sub
    '        If CSng(TextBox23.Text) > 1000 Then
    '            Loopramp = 1000
    '        Else
    '            Loopramp = CSng(TextBox23.Text)
    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1084", Loopramp)
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3411", 1)
    '        TextBox23.BackColor = SystemColors.ControlLight
    '        TextBox23.Text = Format(Loopramp, "0")
    '        'Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1080"))
    '        'TextBox21.Text = Format(rd, "0")
    '    End If
    'End Sub
    'Private Sub TextBox23_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3211", 1)
    '        TextBox23.BackColor = Color.Yellow
    '    End If
    'End Sub


    'Private Sub TextBox24_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'LOOP SET TIME
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox24, "1086", 1) = False Then Exit Sub
    '        If CSng(TextBox24.Text) <= 0 Then
    '            Looptime = 1
    '        ElseIf CSng(TextBox24.Text) > 1000 Then
    '            Looptime = 1000
    '        Else
    '            Looptime = CSng(TextBox24.Text)
    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1086", Looptime)
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3412", 1)
    '        TextBox24.BackColor = SystemColors.ControlLight
    '        TextBox24.Text = Format(Looptime, "0")
    '        'Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1080"))
    '        'TextBox21.Text = Format(rd, "0")
    '    End If
    'End Sub
    'Private Sub TextBox24_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3212", 1)
    '        TextBox24.BackColor = Color.Yellow
    '    End If
    'End Sub


    'Private Sub TextBox25_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'LOOP SET TIME
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox25, "1088", 1) = False Then Exit Sub
    '        If CSng(TextBox25.Text) > 1000 Then
    '            Looprange = 1000
    '        ElseIf CSng(TextBox25.Text) < 0 Then
    '            Looprange = 0
    '        Else
    '            Looprange = CSng(TextBox25.Text)
    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1088", Looprange)
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3413", 1)
    '        TextBox25.BackColor = SystemColors.ControlLight
    '        TextBox25.Text = Format(Looprange, "0")
    '        'Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1080"))
    '        'TextBox21.Text = Format(rd, "0")
    '    End If
    'End Sub
    'Private Sub TextBox25_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3213", 1)
    '        TextBox25.BackColor = Color.Yellow
    '    End If
    'End Sub


    'Private Sub TextBox27_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'HEIGHTSET
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox27, "6000", 1) = False Then Exit Sub
    '        If CSng(TextBox27.Text) < 0 Then
    '            Height_act = 0
    '        ElseIf (TextBox27.Text) > 25 Then
    '            Height_act = 25
    '        Else
    '            Height_act = CSng(TextBox27.Text)
    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6000", Height_act * 100)
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3315", 1)
    '        TextBox27.BackColor = SystemColors.ControlLight
    '        TextBox27.Text = Format(Height_act, "0.00")
    '    End If
    'End Sub
    'Private Sub TextBox27_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3115", 1)
    '        TextBox27.BackColor = Color.Yellow
    '    End If
    'End Sub



    'Private Sub TextBox26_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'HEIGHT time
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox26, "6010", 1) = False Then Exit Sub
    '        If CSng(TextBox26.Text) > 10910 Then
    '            Height_time = 10900
    '        Else
    '            Height_time = CSng(TextBox26.Text)
    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6010", Height_time * 10)
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3400", 1)
    '        TextBox26.BackColor = SystemColors.ControlLight
    '        TextBox26.Text = Format(Height_time, "0.0")
    '    End If
    'End Sub
    'Private Sub TextBox26_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3200", 1)
    '        TextBox26.BackColor = Color.Yellow
    '    End If
    'End Sub


    'Private Sub TextBox28_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Jump
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox28, "6050", 1) = False Then Exit Sub
    '        If CSng(TextBox28.Text) > 6000 Then
    '            Ramp_time = 6000

    '        Else
    '            Ramp_time = CSng(TextBox28.Text)


    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6050", Ramp_time * 10)
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3300", 1)
    '        TextBox28.Text = Format(Ramp_time, "0.0")


    '    End If
    'End Sub
    Private Sub TextBox28_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
        If e.KeyData = Keys.A Then
        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3100", 1)
        End If
    End Sub



    'Private Sub TextBox29_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp Press
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox29, "6060", 1) = False Then Exit Sub
    '        If CSng(TextBox29.Text) > 1000 Then
    '            Ramp_pres = 10000
    '        Else
    '            Ramp_pres = CSng(TextBox29.Text)

    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6060", Ramp_pres * 10)
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3301", 1)
    '        TextBox29.Text = Format(Ramp_pres, "0.0")
    '    End If
    'End Sub
    'Private Sub TextBox29_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3101", 1)
    '    End If
    'End Sub




    'Private Sub TextBox30_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp height
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox30, "6080", 1) = False Then Exit Sub
    '        If CSng(TextBox30.Text) > 25 Then
    '            Ramp_height = 25
    '        Else
    '            Ramp_height = CSng(TextBox30.Text)
    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6080", Ramp_height * 100)
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3302", 1)
    '        TextBox30.Text = Format(Ramp_height, "0.00")

    '    End If
    'End Sub
    'Private Sub TextBox30_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3102", 1)
    '    End If
    'End Sub



    'Private Sub TextBox31_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp MW
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox31, "6100", 1) = False Then Exit Sub
    '        If CSng(TextBox31.Text) > 6000 Then
    '            Ramp_MW = 6000
    '        Else
    '            Ramp_MW = CSng(TextBox31.Text)
    '        End If
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6100", Ramp_MW)
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3303", 1)
    '        TextBox31.Text = Format(Ramp_MW, "0")
    '    End If
    'End Sub
    'Private Sub TextBox31_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3103", 1)
    '    End If
    'End Sub


    'Private Sub TextBox32_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp MFC1
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox32, "6120", 10000) = False Then Exit Sub
    '        If CSng(TextBox32.Text) > GasPrm.Fs(0) Then
    '            Ramp_MFC1 = GasPrm.Fs(0)
    '        Else
    '            Ramp_MFC1 = CSng(TextBox32.Text)
    '        End If
    '        If GasPrm.Fs(0) <> 0 Then
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6120", Ramp_MFC1 * 10000 / GasPrm.Fs(0))
    '            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6120"))
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3304", 1)
    '            TextBox32.Text = Format(rd * GasPrm.Fs(0) / 10000, Dgstr(GasPrm.Decmals(0)))
    '        End If
    '    End If
    'End Sub
    Private Sub TextBox32_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
        If e.KeyData = Keys.A Then
        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3104", 1)
        End If
    End Sub



    'Private Sub TextBox33_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp MFC2
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox33, "6140", 10000) = False Then Exit Sub
    '        If CSng(TextBox33.Text) > GasPrm.Fs(1) Then
    '            Ramp_MFC2 = GasPrm.Fs(1)
    '        Else
    '            Ramp_MFC2 = CSng(TextBox33.Text)
    '        End If
    '        If GasPrm.Fs(1) <> 0 Then
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6140", Ramp_MFC2 * 10000 / GasPrm.Fs(1))
    '            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6140"))
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3305", 1)
    '            TextBox33.Text = Format(rd * GasPrm.Fs(1) / 10000, Dgstr(GasPrm.Decmals(1)))
    '        End If
    '    End If
    'End Sub
    Private Sub TextBox33_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
        If e.KeyData = Keys.A Then
        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3105", 1)
        End If
    End Sub


    'Private Sub TextBox34_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp MFC3
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox34, "6160", 10000) = False Then Exit Sub
    '        If CSng(TextBox34.Text) > GasPrm.Fs(2) Then
    '            Ramp_MFC3 = GasPrm.Fs(2)
    '        Else
    '            Ramp_MFC3 = CSng(TextBox34.Text)
    '        End If
    '        If GasPrm.Fs(2) <> 0 Then
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6160", Ramp_MFC3 * 10000 / GasPrm.Fs(2))
    '            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6160"))
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3306", 1)
    '            TextBox34.Text = Format(rd * GasPrm.Fs(2) / 10000, Dgstr(GasPrm.Decmals(2)))
    '        End If
    '    End If
    'End Sub
    'Private Sub TextBox34_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3106", 1)
    '    End If
    'End Sub


    'Private Sub TextBox35_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp MFC4
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox35, "6180", 10000) = False Then Exit Sub
    '        If CSng(TextBox35.Text) > GasPrm.Fs(3) Then
    '            Ramp_MFC4 = GasPrm.Fs(3)
    '        Else
    '            Ramp_MFC4 = CSng(TextBox35.Text)
    '        End If
    '        If GasPrm.Fs(3) <> 0 Then
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6180", Ramp_MFC4 * 10000 / GasPrm.Fs(3))
    '            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6180"))
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3307", 1)
    '            TextBox35.Text = Format(rd * GasPrm.Fs(3) / 10000, Dgstr(GasPrm.Decmals(3)))
    '        End If
    '    End If
    'End Sub
    Private Sub TextBox35_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
        If e.KeyData = Keys.A Then
        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3107", 1)
        End If
    End Sub


    'Private Sub TextBox36_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp MFC5
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox36, "6200", 10000) = False Then Exit Sub
    '        If CSng(TextBox36.Text) > GasPrm.Fs(4) Then
    '            Ramp_MFC5 = GasPrm.Fs(4)
    '        Else
    '            Ramp_MFC5 = CSng(TextBox36.Text)
    '        End If
    '        If GasPrm.Fs(4) <> 0 Then
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6200", Ramp_MFC5 * 10000 / GasPrm.Fs(4))
    '            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6200"))
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3308", 1)
    '            TextBox36.Text = Format(rd * GasPrm.Fs(4) / 10000, Dgstr(GasPrm.Decmals(4)))
    '        End If
    '    End If
    'End Sub
    'Private Sub TextBox36_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
    '    If e.KeyData = Keys.A Then
    '    Else
    '        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3108", 1)
    '    End If
    'End Sub


    'Private Sub TextBox37_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs)
    '    'Ramp MFC6
    '    If e.KeyChar = vbCr Then
    '        If Check_isNumeric(TextBox37, "6220", 10000) = False Then Exit Sub
    '        If CSng(TextBox37.Text) > GasPrm.Fs(5) Then
    '            Ramp_MFC6 = GasPrm.Fs(5)
    '        Else
    '            Ramp_MFC6 = CSng(TextBox37.Text)
    '        End If
    '        If GasPrm.Fs(5) <> 0 Then
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "6220", Ramp_MFC6 * 10000 / GasPrm.Fs(5))
    '            Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "6220"))
    '            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3309", 1)
    '            TextBox37.Text = Format(rd * GasPrm.Fs(5) / 10000, Dgstr(GasPrm.Decmals(5)))
    '        End If
    '    End If
    'End Sub
    Private Sub TextBox37_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs)  'key input yellow
        If e.KeyData = Keys.A Then
        Else
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3109", 1)
        End If
    End Sub

    'Pulse setting 23/2/1 10kW-pulse 23/2/28 Input bug fix
    Private Sub Tbx_MWMode_TON_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles Tbx_MWMode_TON.KeyPress
        'Pulse mode T.ON[ms] 0.51-4999.50ms 4/4 Minimum change 0.5 → 0.51
        If e.KeyChar = vbCr Then
            'If Check_isNumeric(TextBox21, "1280", 100) = False Then Exit Sub  
            '小数点以下2桁を切り捨てるコードに変更 梶野
            'Ton_Stp = Val(Tbx_MWMode_TON.Text) * 100 '2023/3/30 Sone Ton_StpはSingle型に変更
            Ton_Stp = CInt(Val(Tbx_MWMode_TON.Text) * 100) '2023/11/10 Kajino ここでIntに変換し、小数点以下を切り捨てる
            If Ton_Stp > 499950 Then Ton_Stp = 499950
            If Ton_Stp < 51 Then Ton_Stp = 51
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1280", Ton_Stp) '23/2/28 Input bug fix
            'Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1280") / 100) '23/2/28 Input bug fix
            WrSigned32(CStr(Ton_Stp), DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1280") '2023/3/30 Sone Ton_StpをDouble型に変更してPLCに書き込み
            Dim rd As Single = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1280")) / 100
            Tbx_MWMode_TON.BackColor = SystemColors.ControlLight
            Tbx_MWMode_TON.Text = Format(rd, "0.00")
        End If
    End Sub
    Private Sub Tbx_MWMode_TON_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles Tbx_MWMode_TON.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3201", 1)
            Tbx_MWMode_TON.BackColor = Color.Yellow
        End If
    End Sub
    'Pulse setting 23/2/1 10kW-pulse
    Private Sub Tbx_MWMode_TOFF_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles Tbx_MWMode_TOFF.KeyPress
        'Pulse mode T.OFF[ms] 0.50-4999.50ms  4/4 Minimum change 0.5 → 0.51
        If e.KeyChar = vbCr Then
            'If Check_isNumeric(TextBox22, "1278", 100) = False Then Exit Sub
            '小数点以下2桁を切り捨てるコードをに変更 梶野
            'Toff_Stp = Val(Tbx_MWMode_TOFFa.Text) * 100 '2023/3/30 Sone Toff_StpはSingle型に変更
            Toff_Stp = CInt(Val(Tbx_MWMode_TOFF.Text) * 100) '2023/11/10 Kajino ここでIntに変換し、小数点以下を切り捨てる

            If Toff_Stp > 499950 Then Toff_Stp = 499950
            If Toff_Stp < 51 Then Toff_Stp = 51


            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1278", Toff_Stp) '23/2/28 Input bug fix
            'Dim rd As Single = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1278") / 100) '23/3/28 Input bug fix
            WrSigned32(CStr(Toff_Stp), DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1278") '2023/3/30 Sone Toff_StpをDouble型に変更してPLCに書き込み
            Dim rd As Single = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1278")) / 100

            Tbx_MWMode_TOFF.BackColor = SystemColors.ControlLight
            Tbx_MWMode_TOFF.Text = Format(rd, "0.00")
        End If
    End Sub
    Private Sub Tbx_MWMode_TOFF_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles Tbx_MWMode_TOFF.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3201", 1)
            Tbx_MWMode_TOFF.BackColor = Color.Yellow
        End If
    End Sub


    '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    '2023/10/27 Add 
    '2023/10/30 Handles Add 
    Private Sub Tbx_MWMode_Duty_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles Tbx_MWMode_Duty.KeyPress
        'Max is 100.0%, min is 0.0%
        If e.KeyChar = vbCr Then
            If Check_isNumeric(Tbx_MWMode_Duty, "1247", 100) = False Then Exit Sub
            DutyCycle_Stp = Val(Tbx_MWMode_Duty.Text) * 100
            'If DutyCycle_Stp > 9950 Then DutyCycle_Stp = 9950
            'If DutyCycle_Stp > 10000 Then DutyCycle_Stp = 10000 '2023/10/30
            If DutyCycle_Stp > 10000 - Freq_Stp * 5 Then DutyCycle_Stp = 10000 - Freq_Stp * 5 '2023/12/5 Sone DutyCycle High Limit is changed

            'If DutyCycle_Stp < 50 Then DutyCycle_Stp = 50
            If DutyCycle_Stp < Freq_Stp * 0.05 * 100 Then DutyCycle_Stp = Freq_Stp * 0.05 * 100 '2023/12/5 Sone DutyCycle Low Limit is changed

            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1247", DutyCycle_Stp)

            Dim rd As Single = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1217") / 100
            Tbx_MWMode_Duty.BackColor = SystemColors.ControlLight
            Tbx_MWMode_Duty.Text = Format(rd, "0.0")
            Tbx_MWMode_Duty_edit = False

        End If
    End Sub

    Private Sub Tbx_MWMode_Duty_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles Tbx_MWMode_Duty.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            Tbx_MWMode_Duty_edit = True
            Tbx_MWMode_Duty.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub Tbx_MWMode_Freq_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles Tbx_MWMode_Freq.KeyPress
        'Max is 1000Hz, min is 10Hz
        If e.KeyChar = vbCr Then
            If Check_isNumeric(Tbx_MWMode_Freq, "1246", 1) = False Then Exit Sub
            Freq_Stp = Val(Tbx_MWMode_Freq.Text) * 1
            If Freq_Stp > 1000 Then Freq_Stp = 1000
            If Freq_Stp < 10 Then Freq_Stp = 10

            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1246", Freq_Stp)

            Dim rd As Single = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1216") / 1
            Tbx_MWMode_Freq.BackColor = SystemColors.ControlLight
            Tbx_MWMode_Freq.Text = Format(rd, "0")
            Tbx_MWMode_Freq_edit = False

        End If
    End Sub

    Private Sub Tbx_MWMode_Freq_Keydown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles Tbx_MWMode_Freq.KeyDown 'key input yellow
        If e.KeyData = Keys.A Then
        Else
            Tbx_MWMode_Freq_edit = True
            Tbx_MWMode_Freq.BackColor = Color.Yellow
        End If
    End Sub
    '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    ''CHAMBER
    'Private Sub TextBox29_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox29.ContextMenuStrip = ContextMenuStrip1
    'End Sub

    Private Sub ToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem1.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6204", 1)
    End Sub

    Private Sub ToolStripMenuItem2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem2.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6204", 0)
    End Sub

    'HEIGHT
    'Private Sub TextBox30_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox30.ContextMenuStrip = ContextMenuStrip2
    'End Sub

    Private Sub ToolStripMenuItem3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem3.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6206", 1)
    End Sub

    Private Sub ToolStripMenuItem4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem4.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6206", 0)
    End Sub


    ''MW
    'Private Sub TextBox31_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox31.ContextMenuStrip = ContextMenuStrip3
    'End Sub

    Private Sub ToolStripMenuItem5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem5.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6207", 1)
    End Sub

    Private Sub ToolStripMenuItem6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem6.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6207", 0)
    End Sub


    'MFC1
    'Private Sub TextBox32_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox32.ContextMenuStrip = ContextMenuStrip4
    'End Sub

    Private Sub ToolStripMenuItem7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem7.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6208", 1)
    End Sub

    Private Sub ToolStripMenuItem8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem8.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6208", 0)
    End Sub


    'MFC2
    'Private Sub TextBox33_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox33.ContextMenuStrip = ContextMenuStrip5
    'End Sub

    Private Sub ToolStripMenuItem9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem9.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6209", 1)
    End Sub

    Private Sub ToolStripMenuItem10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem10.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6209", 0)
    End Sub


    'MFC3
    'Private Sub TextBox34_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox34.ContextMenuStrip = ContextMenuStrip6
    'End Sub

    Private Sub ToolStripMenuItem11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem11.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6210", 1)
    End Sub

    Private Sub ToolStripMenuItem12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem12.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6210", 0)
    End Sub


    'MFC4
    'Private Sub TextBox35_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox35.ContextMenuStrip = ContextMenuStrip7
    'End Sub

    Private Sub ToolStripMenuItem13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem13.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6211", 1)
    End Sub

    Private Sub ToolStripMenuItem14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem14.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6211", 0)
    End Sub

    'MFC5
    'Private Sub TextBox36_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox36.ContextMenuStrip = ContextMenuStrip8
    'End Sub

    Private Sub ToolStripMenuItem15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem15.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6212", 1)
    End Sub

    Private Sub ToolStripMenuItem16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem16.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6212", 0)
    End Sub

    'MFC6
    'Private Sub TextBox37_TextChanged(sender As System.Object, e As System.EventArgs)
    '    TextBox37.ContextMenuStrip = ContextMenuStrip9
    'End Sub

    Private Sub ToolStripMenuItem17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem17.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6213", 1)
    End Sub

    Private Sub ToolStripMenuItem18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem18.Click
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6213", 0)
    End Sub


    Private Sub ContextMenuStrip2_Opening(ByVal sender As System.Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles ContextMenuStrip2.Opening

    End Sub

    Private Sub Button29_Click_1(sender As Object, e As EventArgs) Handles Button29.Click
        'SUPERVISOR/LOG OUT btton
        If LoginMode = SUPRVISR Then
            'LOGOUT-> OPERATOR
            LoginMode = OPRATR
            AxDBCommManager1.WriteDevice(DBPlcDevice.DKV5000XYM_D, LGMD_PLC, LoginMode)
            RgsW("0")

            CtlVsble_Operation(LoginMode)
            Button39.Visible = False
        Else
            'LOGIN-> SUPERVISOR or Operator
            frmLogIn.ShowDialog()
        End If


    End Sub

    Public Sub RgsW(ByVal wd As String)
        'キー（HKEY_CURRENT_USER\Software\SDS\Unlock）を開く
        Dim regkey As Microsoft.Win32.RegistryKey =
            Microsoft.Win32.Registry.CurrentUser.CreateSubKey("Software\SDS\Unlock")

        'REG_EXPAND_SZで書き込む
        regkey.SetValue("String", wd,
            Microsoft.Win32.RegistryValueKind.String)
        'regkey.SetValue("ExpandString", "%windir%",
        '    Microsoft.Win32.RegistryValueKind.ExpandString)
        ''REG_QWORDで書き込む
        'regkey.SetValue("QWord", 1000, Microsoft.Win32.RegistryValueKind.QWord)

        '閉じる
        regkey.Close()
    End Sub

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

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick  '2022/10/20 PYRO入力時動作修正

        'Pyro Alarm check
        Dim pyrwh As Integer = AxDBCommManager1.ReadDevice(DBPlcDevice.KV5000XYM_M, "20201")
        Dim pyrah As Integer = AxDBCommManager1.ReadDevice(DBPlcDevice.KV5000XYM_M, "20202")
        Dim pyrwL As Integer = AxDBCommManager1.ReadDevice(DBPlcDevice.KV5000XYM_M, "20203")
        Dim pyraL As Integer = AxDBCommManager1.ReadDevice(DBPlcDevice.KV5000XYM_M, "20204")

        If pyrwh = 1 Then
            TextBox19.BackColor = AonCLR
        ElseIf TextBox19.BackColor = Color.Yellow Then
            TextBox19.BackColor = Color.Yellow
        Else
            TextBox19.BackColor = SystemColors.ControlLight
        End If
        If pyrah = 1 Then
            TextBox20.BackColor = AonCLR
        ElseIf TextBox20.BackColor = Color.Yellow Then
            TextBox20.BackColor = Color.Yellow
        Else
            TextBox20.BackColor = SystemColors.ControlLight
        End If

        If pyrwL = 1 Then
            TextBox38.BackColor = AonCLR
        ElseIf TextBox38.BackColor = Color.Yellow Then
            TextBox38.BackColor = Color.Yellow
        Else
            TextBox38.BackColor = SystemColors.ControlLight
        End If

        If pyraL = 1 Then
            TextBox39.BackColor = AonCLR
        ElseIf TextBox39.BackColor = Color.Yellow Then
            TextBox39.BackColor = Color.Yellow
        Else
            TextBox39.BackColor = SystemColors.ControlLight
        End If

    End Sub

    Private Sub Button38_Click(sender As Object, e As EventArgs) Handles Button38.Click
        'go to "TEMP CONTROL"
        frmTmpCTL.Show()
    End Sub

    Private Sub WrSigned32(sinedText As String, dev As DATABUILDERAXLibLB.DBPlcDevice, devNum As Integer)
        Dim cv As DATABUILDERAXLibLB.DBValueConverterEx
        Dim signedValL As Long
        Dim signedValH As Long
        cv = New DATABUILDERAXLibLB.DBValueConverterEx

        cv.SignedTextToWValue(sinedText, signedValL, signedValH)

        AxDBCommManager1.WriteDevice(dev, CStr(devNum), signedValL)
        AxDBCommManager1.WriteDevice(dev, CStr(devNum + 1), signedValH)

    End Sub

    Private Function RdSigned32(dev As DATABUILDERAXLibLB.DBPlcDevice, devNum As Integer) As String
        Dim valL As Long
        Dim valH As Long
        Dim strText As String = ""
        Dim cv As DATABUILDERAXLibLB.DBValueConverterEx

        valL = AxDBCommManager1.ReadDevice(dev, CStr(devNum))
        valH = AxDBCommManager1.ReadDevice(dev, CStr(devNum + 1))
        cv = New DATABUILDERAXLibLB.DBValueConverterEx
        cv.WValueToSignedText(valL, valH, strText)
        Return strText

    End Function

    Private Sub Button39_Click(sender As Object, e As EventArgs) Handles Button39.Click
        'OPEN SDS6NETSYS FOLDER
        Const PATH As String = "C:\SDS6NETSYS\"

        Try
            Expler = System.Diagnostics.Process.Start("EXPLORER.EXE", PATH)
            'Expler = System.Diagnostics.Process.Start("Notepad.EXE")
        Catch ex As Exception
            MessageBox.Show(ex.Message)
        End Try
    End Sub

    'Pulse modeとCW modeの切り替え選択  23/02/01 10kW-pulse
    'Revised 23/03/01
    ' RadioButton1 -> Pulse mode, RadioButton2 -> CW mode
    ' PLC MR6406 is Pulse mode = ON
    'Revised 23/05/26 When MW is ON(MR706), operate mode can't change
    Private Sub Rbn_Mode_Pulse_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Rbn_Mode_Pulse.CheckedChanged
        'If AxDBDeviceManager1.Devices(7).ValueRead = 0 Then 'MR706
        If Rbn_Mode_Pulse.Checked = True Then 'And Frm_Loaded = True 
            'AxDBDeviceManager3.Devices(121).ValueWrite = 1                  'AxDBDeviceManager3.Devices(121) is MR6406
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6406", 1)
            MessageBox.Show("The safety area is disabled because pulse mode has been selected.", "", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            Lbl_SA_Msg.Visible = True
            '%%%%%%%%%%%%%%%%%%%%%%%%%%2023/11/28 Sone Added Invisible SA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            Label133.Visible = False 'Safety Area Pres name
            Label135.Visible = False 'Safety Area Pres Lamp
            Label198.Visible = False 'Safety Area Power name
            Label199.Visible = False 'Safety Area Power Lamp
        Else
            'AxDBDeviceManager3.Devices(121).ValueWrite = 0
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6406", 0)
            Lbl_SA_Msg.Visible = False
            '%%%%%%%%%%%%%%%%%%%%%%%%%%2023/11/28 Sone Added Invisible SA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            Label133.Visible = True 'Safety Area Pres name
            Label135.Visible = True 'Safety Area Pres Lamp
            Label198.Visible = True 'Safety Area Power name
            Label199.Visible = True 'Safety Area Power Lamp
        End If
        'End If
    End Sub
    'プラズマサイズによるセーフティエリアの切り替え選択  23/04/18 10kW Safety area change by plasma size
    Dim rv As Integer
    Dim wrval As Integer

    Private Sub Rbn_PUCK_2in_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Rbn_PUCK_2in.CheckedChanged '2"Plasma Safety area Select
        Dim rv As Integer
        Dim wrval As Integer
        'Revised 2023/06/16
        If Frm_Loaded = True Then
            rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "119")
            If Rbn_PUCK_2in.Checked = True Then
                'wrval = rv Or &H4
                'wrval = wrval And Not &H8
                'wrval = wrval And Not &H10
                wrval = SetBitValue(rv, 2, True)
                Label135.Visible = False '2023/09/28 Sone When 2' selected, Pres Safety area is invisible
                Label133.Visible = False
            Else
                wrval = SetBitValue(rv, 2, False)
            End If
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "119", wrval) '2インチプラズマ DM119.2

        End If
    End Sub

    Private Sub Rbn_PUCK_3in_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Rbn_PUCK_3in.CheckedChanged '3"Plasma Safety area Select
        Dim rv As Integer
        Dim wrval As Integer
        'Revised 2023/06/16
        If Frm_Loaded = True Then
            rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "119")
            If Rbn_PUCK_3in.Checked = True Then
                'wrval = rv And Not &H4
                'wrval = wrval Or &H8
                'wrval = wrval And Not &H10
                wrval = SetBitValue(rv, 3, True)
                Label135.Visible = True '2023/09/28 Sone When 3' and 4' selected, Pres Safety area is visible
                Label133.Visible = True
            Else
                wrval = SetBitValue(rv, 3, False)
            End If
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "119", wrval) '3インチプラズマ DM119.3
        End If
    End Sub

    Private Sub Rbn_PUCK_4in_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Rbn_PUCK_4in.CheckedChanged '4"Plasma Safety area Select
        Dim rv As Integer
        Dim wrval As Integer
        'Revised 2023/06/16
        If Frm_Loaded = True Then
            rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "119")
            If Rbn_PUCK_4in.Checked = True Then
                'wrval = rv And Not &H4
                'wrval = wrval And Not &H8
                'wrval = wrval Or &H10
                wrval = SetBitValue(rv, 4, True)
                Label135.Visible = True '2023/09/28 Sone When 3' and 4' selected, Pres Safety area is visible
                Label133.Visible = True

            Else
                wrval = SetBitValue(rv, 4, False)
            End If
            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "119", wrval) '4インチプラズマ DM119.4
        End If

    End Sub


    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick
        'Check Wave Logger status
        Dim stte As Long = 0
        If WLok = 0 Then
            stte = WaveLogger.GetState()
            If stte <> 5 Then
                Timer2.Enabled = False
                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 1)
                frmWLalrm.Show()
                'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 0)
                Button10.Visible = True
                'WaveLogger.Quit()
                'WaveLogger = Nothing
                'WLok = 1
            End If
        End If
    End Sub

    Public Function GetBitArray(value As Integer) As Boolean()
        Dim bits(15) As Boolean                                 ' Array to hold the bits
        Dim mask As Integer = &HFFFF                            ' Mask to extract the lower 16 bits

        value = value And mask                                  ' Extract the lower 16 bits

        For i As Integer = 0 To 15                              ' Iterate over each bit position
            bits(i) = (value And (1 << i)) <> 0                 ' Check if bit is set (1) or not (0)
        Next

        Return bits                                             ' Return the array of bits
    End Function

    Public Function SetBitValue(value As Integer, position As Integer, bitValue As Boolean) As Integer
        Dim mask As Integer = 1 << position ' Create a mask with the bit at the specified position

        If bitValue Then
            ' Set the bit to 1
            value = value Or mask
        Else
            ' Set the bit to 0
            value = value And (Not mask)
        End If

        ' Consider only the lower 16 bits
        value = value And &HFFFF

        Return value
    End Function

    Private Sub AxDBTriggerManager2_Fire(sender As Object, e As _IDBTriggerEvents_FireEvent) Handles AxDBTriggerManager2.Fire
        '-------- WaveLogger stop-------
        Try
            WaveLogger.Stop()
            Timer2.Enabled = False
        Catch ex As Exception

        End Try

    End Sub

    Private Sub AxDBTriggerManager3_Fire(sender As Object, e As _IDBTriggerEvents_FireEvent) Handles AxDBTriggerManager3.Fire
        'if Tmp control is busy then show frmTmpCTL

        frmTmpCTL.Show()

    End Sub

    Private Sub RadioButton_MWMode_Time_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton_MWMode_Time.CheckedChanged
        '-----------2023/10/27 Add the MW mode select function
        If RadioButton_MWMode_Time.Checked = True Then MWMode_Time = True Else MWMode_Time = False

        If Frm_Loaded = True Then
            If MWMode_Time = True Then

                Tbx_MWMode_TON.BackColor = SystemColors.ControlLight
                Tbx_MWMode_TOFF.BackColor = SystemColors.ControlLight
                Tbx_MWMode_Duty.BackColor = Color.Khaki
                Tbx_MWMode_Freq.BackColor = Color.Khaki

                Tbx_MWMode_TON.ForeColor = SystemColors.WindowText
                Tbx_MWMode_TOFF.ForeColor = SystemColors.WindowText
                Tbx_MWMode_Duty.ForeColor = Color.SaddleBrown
                Tbx_MWMode_Freq.ForeColor = Color.SaddleBrown

                Tbx_MWMode_TON.Enabled = True
                Tbx_MWMode_TOFF.Enabled = True
                Tbx_MWMode_Duty.Enabled = False
                Tbx_MWMode_Freq.Enabled = False

                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6407", 1)

            Else

                Tbx_MWMode_TON.BackColor = Color.Khaki
                Tbx_MWMode_TOFF.BackColor = Color.Khaki
                Tbx_MWMode_Duty.BackColor = SystemColors.ControlLight
                Tbx_MWMode_Freq.BackColor = SystemColors.ControlLight

                Tbx_MWMode_TON.ForeColor = Color.SaddleBrown
                Tbx_MWMode_TOFF.ForeColor = Color.SaddleBrown
                Tbx_MWMode_Duty.ForeColor = SystemColors.WindowText
                Tbx_MWMode_Freq.ForeColor = SystemColors.WindowText

                Tbx_MWMode_TON.Enabled = False
                Tbx_MWMode_TOFF.Enabled = False
                Tbx_MWMode_Duty.Enabled = True
                Tbx_MWMode_Freq.Enabled = True

                AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "6407", 0)

            End If


        End If
    End Sub

End Class
