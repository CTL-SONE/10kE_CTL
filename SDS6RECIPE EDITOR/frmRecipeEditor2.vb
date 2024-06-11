Public Class FrmRecipeEditor
    '**************************************************************
    '              SDS6K-10kE RECIPE EDITOR Ver1.00.1
    '                        2023/10/24
    ' Ver4.04ベース 
    ' Ver1.00.0.1 2023/10/21 - Alex - Bug of MW input value <>0
    ' Ver1.00.0.2 2023/11/29 - Kajino - Now step become bold
    '**************************************************************




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
        Dim Min As Long           '[W]      -> added 23/10/21
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
    End Structure

    'Plenum Parameter
    Structure PlnmParameter
        Dim Tolerance As Single     '[Torr]
    End Structure

    'PyroMeter Parameter
    Structure PyroParameter
        Dim RangeL As Long        '[DegC]
        Dim RangeH As Long        '[DegC]
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

    'RECIPE PARAMETER
    Structure Recipe
        Dim Fname As String
        Dim StepNum As Integer
        Dim StepT() As Single
        Dim Pres() As Single
        Dim PresRamp() As Integer     'Append 20160725
        Dim Plnm() As Single
        Dim PlnmRamp() As Integer
        Dim Hght() As Single          'No use Append 20160831
        Dim HghtRamp() As Integer     'No use Append 20160831
        Dim Temp() As Single          'Append 20160725
        Dim Power() As Single
        Dim MWrmp() As Integer        'Append 20160725
        Dim TmpCtl() As Integer       'Append 20160725
        Dim Gas1() As Single
        Dim MFC1rmp() As Integer      'Append 20160725
        Dim Gas2() As Single
        Dim MFC2rmp() As Integer      'Append 20160725
        Dim Gas3() As Single
        Dim MFC3rmp() As Integer      'Append 20160725
        Dim Gas4() As Single
        Dim MFC4rmp() As Integer      'Append 20160725
        Dim Gas5() As Single
        Dim MFC5rmp() As Integer      'Append 20160725
        Dim Gas6() As Single
        Dim MFC6rmp() As Integer      'Append 20160725
        Dim RghF() As Integer
        Dim VntF() As Integer
        Dim TmrF() As Integer
        Dim SkpF() As Integer
        Dim temp1() As Integer      'No use
        Dim temp2() As Integer      'No use
        Dim temp3() As Integer      'No use


    End Structure

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
        Dim WL_Screen As Boolean
    End Structure



    Const PATH As String = "C:\SDS6NETSYS\"
    Const PARAMFILE_NAM As String = "Param.txt"
    Const RCP_LENG As Integer = 100

    Dim RdComp As Boolean
    Dim WrComp As Boolean
    Dim VonCLR As System.Drawing.Color = Color.Red          'Valve ON color
    Dim VofCLR As System.Drawing.Color = Color.Black        'Valve OFF color
    Dim DonCLR As System.Drawing.Color = Color.Red          'Device ON color
    Dim DofCLR As System.Drawing.Color = Color.Yellow       'Device OFF color

    Dim GasName(6) As String
    Dim SysType As Integer = 0    '0:Normal/1:TMP
    Dim MwF As Single
    Dim MwR As Single

    Dim Dgstr(3) As String

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
    Dim WtrPrm As WaterParameter

    'RECIPE
    Dim Rcp As Recipe
    Dim Step_Ctr As Integer
    Dim AutoMode As Integer


    Dim Jump As Integer         'Append 20210506
    Dim Ctlvsbl As ControlVisible



    Friend GrdRow As Integer    'Append 20160728
    Friend GrdClm As Integer    'Append 20160728

    Public LodEnd As Boolean = False

    Public LoginMode As Integer = OPRATR 'Supervisor:1/ Operator:0
    Public Const SUPRVISR = 1
    Public Const OPRATR = 0
    Public Const LGMD_PLC As String = "47000"

    Public Kyctr As Integer = 0

    'Append 2022/10/20
    Public PowRmp_inaction As Integer
    Public PlnRmp_inaction As Integer
    Public ChmbRmp_inaction As Integer




    Private Sub FrmRecipeEditor_Load(sender As Object, e As System.EventArgs) Handles Me.Load
        '// Initialize
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim k As Integer = 0

        '-------- Rev.2022/02
        'IP and Version of PLC are stored in HKEY_CURRENT_USER\SoftWare\SDS\PLC
        AxDBCommManager1.Peer = rgsplcip() & ":8500"    'default 192.168.3.13 & port 8500
        AxDBCommManager1.PLC = CInt(RgsPlcSr())         'default 515 = KV-5500/5000/3000

        Try
            AxDBCommManager1.Connect()
            'AxDBTriggerManager1.Active = True
            AxDBTriggerManager2.Active = True
        Catch ex As Exception
            MessageBox.Show(ex.Message)
            Exit Sub
        End Try

        ReadCtlvsblPrm()

        LoginMode = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000XYM_D, LGMD_PLC)
        'LoginMode = 1  'To debugg
        CtlVsble_Operation(LoginMode)

        Jump = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "2240")
        TextBox1.Text = Format(Jump, "0")

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

        With Rcp
            ReDim .StepT(RCP_LENG)
            ReDim .Pres(RCP_LENG)
            ReDim .Plnm(RCP_LENG)   'Append 20210522
            ReDim .Hght(RCP_LENG)   'No use Append 20160831
            ReDim .Temp(RCP_LENG)   'Append 20160725
            ReDim .Power(RCP_LENG)
            ReDim .Gas1(RCP_LENG)
            ReDim .Gas2(RCP_LENG)
            ReDim .Gas3(RCP_LENG)
            ReDim .Gas4(RCP_LENG)
            ReDim .Gas5(RCP_LENG)
            ReDim .Gas6(RCP_LENG)
            ReDim .RghF(RCP_LENG)
            ReDim .VntF(RCP_LENG)
            ReDim .TmrF(RCP_LENG)
            ReDim .SkpF(RCP_LENG)
            ReDim .PresRamp(RCP_LENG)
            ReDim .PlnmRamp(RCP_LENG)
            ReDim .HghtRamp(RCP_LENG)   'No use Append 20160831
            ReDim .MWrmp(RCP_LENG)
            ReDim .TmpCtl(RCP_LENG)
            ReDim .MFC1rmp(RCP_LENG)
            ReDim .MFC2rmp(RCP_LENG)
            ReDim .MFC3rmp(RCP_LENG)
            ReDim .MFC4rmp(RCP_LENG)
            ReDim .MFC5rmp(RCP_LENG)
            ReDim .MFC6rmp(RCP_LENG)
            ReDim .temp1(RCP_LENG)
            ReDim .temp2(RCP_LENG)
            ReDim .temp3(RCP_LENG)

        End With

        With DataGridView1
            For i = 0 To RCP_LENG - 1
                .RowCount = RCP_LENG
                .Rows(i).Cells(0).Value = i + 1 'STEP No set
            Next
            .RowCount = RCP_LENG + 1
            .Rows(100).SetValues("")
            .Rows(100).ReadOnly = True
            .Rows(100).MinimumHeight = 2
            .Rows(100).Height = 0

        End With

        Dim device(1600) As DATABUILDERAXLibLB.DBDevice
        'Dim device(1300) As DATABUILDERAXLibLB.DBDevice
        'Dim device(2600) As DATABUILDERAXLibLB.DBDevice
        RdComp = False
        AxDBDeviceManager2.ReadAll()
        Do
            My.Application.DoEvents()
        Loop Until RdComp = True

        For i = 1 To 1600   '1300
            device(i) = AxDBDeviceManager2.Devices.Item(i)
        Next

        For i = 1 To RCP_LENG
            k = (i - 1) * 16    '13
            'Rcp.StepT(i - 1) = CSng(device(k + 1).ValueRead / 60)
            Rcp.StepT(i - 1) = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, 20000 + k + 14)) / 60
            Rcp.Pres(i - 1) = CSng(device(k + 2).ValueRead / 10)
            Rcp.Plnm(i - 1) = CSng(device(k + 3).ValueRead / 10)
            Rcp.Hght(i - 1) = CSng(device(k + 14).ValueRead / 10)
            Rcp.Power(i - 1) = CSng(device(k + 4).ValueRead)
            Rcp.Temp(i - 1) = CSng(device(k + 11).ValueRead)
            Rcp.Gas1(i - 1) = CSng(device(k + 5).ValueRead / 10000 * GasPrm.Fs(0))
            Rcp.Gas2(i - 1) = CSng(device(k + 6).ValueRead / 10000 * GasPrm.Fs(1))
            Rcp.Gas3(i - 1) = CSng(device(k + 7).ValueRead / 10000 * GasPrm.Fs(2))
            Rcp.Gas4(i - 1) = CSng(device(k + 8).ValueRead / 10000 * GasPrm.Fs(3))
            Rcp.Gas5(i - 1) = CSng(device(k + 9).ValueRead / 10000 * GasPrm.Fs(4))
            Rcp.Gas6(i - 1) = CSng(device(k + 10).ValueRead / 10000 * GasPrm.Fs(5))

            With DataGridView1
                '---------6:Pres-------
                If (device(k + 12).ValueRead And &H1) = &H1 Then
                    Rcp.PresRamp(i - 1) = 1
                    .Rows(i - 1).Cells(6).Style.BackColor = Color.Pink
                Else
                    Rcp.PresRamp(i - 1) = 0
                    .Rows(i - 1).Cells(6).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                '---------7:Plnm--------
                If (device(k + 12).ValueRead And &H800) = &H800 Then
                    Rcp.PlnmRamp(i - 1) = 1
                    .Rows(i - 1).Cells(7).Style.BackColor = Color.Pink
                Else
                    Rcp.PlnmRamp(i - 1) = 0
                    .Rows(i - 1).Cells(7).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                ''----------8:Temp 9:Power--------
                'If (device(k + 12).ValueRead And &H2) = &H2 Then
                '    Rcp.MWrmp(i - 1) = 1
                '    .Rows(i - 1).Cells(9).Style.BackColor = Color.Pink
                '    .Rows(i - 1).Cells(8).Style.BackColor = .DefaultCellStyle.BackColor
                'ElseIf (device(k + 12).ValueRead And &H4) = &H4 Then
                '    Rcp.MWrmp(i - 1) = 0
                '    Rcp.TmpCtl(i - 1) = 1
                '    .Rows(i - 1).Cells(8).Style.BackColor = Color.GreenYellow
                '    .Rows(i - 1).Cells(9).Style.BackColor = Color.GreenYellow
                'Else
                '    Rcp.TmpCtl(i - 1) = 0
                '    .Rows(i - 1).Cells(8).Style.BackColor = .DefaultCellStyle.BackColor
                '    .Rows(i - 1).Cells(9).Style.BackColor = .DefaultCellStyle.BackColor
                'End If
                '2022/11/09 modify Kajino
                '----------8:Temp--------
                If (device(k + 12).ValueRead And &H4) = &H4 Then
                    Rcp.TmpCtl(i - 1) = 1
                    .Rows(i - 1).Cells(8).Style.BackColor = Color.GreenYellow
                Else
                    Rcp.TmpCtl(i - 1) = 0
                    .Rows(i - 1).Cells(8).Style.BackColor = .DefaultCellStyle.BackColor
                End If
                '----------9:Power--------
                If (device(k + 12).ValueRead And &H2) = &H2 Then
                    Rcp.MWrmp(i - 1) = 1
                    .Rows(i - 1).Cells(9).Style.BackColor = Color.Pink
                Else
                    Rcp.MWrmp(i - 1) = 0
                    .Rows(i - 1).Cells(9).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                '---------MFCrmp-------
                If (device(k + 12).ValueRead And &H8) = &H8 Then
                    Rcp.MFC1rmp(i - 1) = 1
                    .Rows(i - 1).Cells(10).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC1rmp(i - 1) = 0
                    .Rows(i - 1).Cells(10).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H10) = &H10 Then
                    Rcp.MFC2rmp(i - 1) = 1
                    .Rows(i - 1).Cells(11).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC2rmp(i - 1) = 0
                    .Rows(i - 1).Cells(11).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H20) = &H20 Then
                    Rcp.MFC3rmp(i - 1) = 1
                    .Rows(i - 1).Cells(12).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC3rmp(i - 1) = 0
                    .Rows(i - 1).Cells(12).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H40) = &H40 Then
                    Rcp.MFC4rmp(i - 1) = 1
                    .Rows(i - 1).Cells(13).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC4rmp(i - 1) = 0
                    .Rows(i - 1).Cells(13).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H80) = &H80 Then
                    Rcp.MFC5rmp(i - 1) = 1
                    .Rows(i - 1).Cells(14).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC5rmp(i - 1) = 0
                    .Rows(i - 1).Cells(14).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H100) = &H100 Then
                    Rcp.MFC6rmp(i - 1) = 1
                    .Rows(i - 1).Cells(15).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC6rmp(i - 1) = 0
                    .Rows(i - 1).Cells(15).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H200) = &H200 Then
                    Rcp.HghtRamp(i - 1) = 1
                    .Rows(i - 1).Cells(16).Style.BackColor = Color.Pink
                Else
                    Rcp.HghtRamp(i - 1) = 0
                    .Rows(i - 1).Cells(16).Style.BackColor = .DefaultCellStyle.BackColor
                End If

            End With

            If device(k + 13).ValueRead = 1 Then Rcp.RghF(i - 1) = 1 Else Rcp.RghF(i - 1) = 0
            If device(k + 12).ValueRead >= 4096 Then Rcp.VntF(i - 1) = 1 Else Rcp.VntF(i - 1) = 0
            If device(k + 13).ValueRead = 4 Then Rcp.TmrF(i - 1) = 1 Else Rcp.TmrF(i - 1) = 0
            If device(k + 13).ValueRead = 8 Then Rcp.SkpF(i - 1) = 1 Else Rcp.SkpF(i - 1) = 0
            With DataGridView1
                .Rows(i - 1).Cells(1).Value = Rcp.RghF(i - 1)
                .Rows(i - 1).Cells(2).Value = Rcp.VntF(i - 1)
                .Rows(i - 1).Cells(3).Value = Rcp.TmrF(i - 1)
                .Rows(i - 1).Cells(4).Value = Rcp.SkpF(i - 1)
                For j = 1 To 4
                    If .Rows(i - 1).Cells(j).Value = 1 Then
                        .Rows(i - 1).Cells(j).Style.BackColor = Color.Red
                    ElseIf .Rows(i - 1).Cells(j).Value = 0 Then
                        .Rows(i - 1).Cells(j).Style.BackColor = SystemColors.Control
                    End If
                Next
                .Rows(i - 1).Cells(5).Value = Format(Rcp.StepT(i - 1), "0.0")  'StepT set
                .Rows(i - 1).Cells(6).Value = Format(Rcp.Pres(i - 1), "0.0")   'Pres set
                .Rows(i - 1).Cells(7).Value = Format(Rcp.Plnm(i - 1), "0.0")   'Plnm set
                .Rows(i - 1).Cells(8).Value = Format(Rcp.Temp(i - 1), "0")    'Temp set 
                .Rows(i - 1).Cells(9).Value = Format(Rcp.Power(i - 1), "0")   'Power set
                .Rows(i - 1).Cells(10).Value = Format(Rcp.Gas1(i - 1), Dgstr(GasPrm.Decmals(0)))  'MFC1 set
                .Rows(i - 1).Cells(11).Value = Format(Rcp.Gas2(i - 1), Dgstr(GasPrm.Decmals(1)))  'MFC2 set
                .Rows(i - 1).Cells(12).Value = Format(Rcp.Gas3(i - 1), Dgstr(GasPrm.Decmals(2)))  'MFC3 set
                .Rows(i - 1).Cells(13).Value = Format(Rcp.Gas4(i - 1), Dgstr(GasPrm.Decmals(3)))  'MFC4 set
                .Rows(i - 1).Cells(14).Value = Format(Rcp.Gas5(i - 1), Dgstr(GasPrm.Decmals(4)))  'MFC5 set
                .Rows(i - 1).Cells(15).Value = Format(Rcp.Gas6(i - 1), Dgstr(GasPrm.Decmals(5)))  'MFC6 set
                .Rows(i - 1).Cells(16).Value = Format(Rcp.Hght(i - 1), "0.0")   'Hght set *Append 2021/
            End With
        Next

        'MW power limits   - added 2023/10/21
        MwPrm.Max = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "118")
        MwPrm.Min = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "150")

        'SCREEN INITIALIZE
        Label9.Text = Format(MwPrm.Max, "0")    'Pmax
        Label3.Text = GasPrm.Name(0)            'Gas1
        Label4.Text = GasPrm.Name(1)            'Gas2
        Label5.Text = GasPrm.Name(2)            'Gas3
        Label6.Text = GasPrm.Name(3)            'Gas4
        Label7.Text = GasPrm.Name(4)            'Gas5
        Label8.Text = GasPrm.Name(5)            'Gas6

        Rcp.Fname = "CURRENT DATA"
        Label11.Text = Rcp.Fname

        Step_Ctr = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "1032")
        AutoMode = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_MR, "907")




        LodEnd = True



        'Pres
        'DataGridView1.Columns(7).ContextMenuStrip = Nothing
        DataGridView1.Columns(6).ContextMenuStrip = Me.ContextMenuStrip1    'Pres
        DataGridView1.Columns(7).ContextMenuStrip = Me.ContextMenuStrip1    'Plnm
        DataGridView1.Columns(16).ContextMenuStrip = Me.ContextMenuStrip1   'Height
        DataGridView1.Columns(10).ContextMenuStrip = Me.ContextMenuStrip1   'MFC1
        DataGridView1.Columns(11).ContextMenuStrip = Me.ContextMenuStrip1   'MFC2
        DataGridView1.Columns(12).ContextMenuStrip = Me.ContextMenuStrip1   'MFC3
        DataGridView1.Columns(13).ContextMenuStrip = Me.ContextMenuStrip1   'MFC4
        DataGridView1.Columns(14).ContextMenuStrip = Me.ContextMenuStrip1   'MFC5
        DataGridView1.Columns(15).ContextMenuStrip = Me.ContextMenuStrip1   'MFC6
        DataGridView1.Columns(8).ContextMenuStrip = Me.ContextMenuStrip2    'Temp
        DataGridView1.Columns(9).ContextMenuStrip = Me.ContextMenuStrip1    'Power

        Timer1.Enabled = True

        If AxDBDeviceManager2.Devices(2601).ValueRead = 39321 Then  'Append 20161114
            Exit Sub
        Else
            Application.Exit()
        End If

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

    Public Sub CtlVsble_Operation(login_mode As Integer)

        Dim w As Integer = DataGridView1.Columns(5).Width
        Dim cw(10) As Integer
        Dim dgvw As Integer = DataGridView1.Width + 4   'Tmp.C 40->44 (+4)
        Dim frmw As Integer = 1118

        If login_mode = OPRATR Then
            With Ctlvsbl
                Me.Text = "SDS6K-10kE RECIPE LOADER Ver1.00" '23/10/05 Ver1.00に更新
                Button2.Visible = False 'File Save
                Button4.Visible = .Recp_jmpto
                TextBox1.Visible = .Recp_jmpto
                DataGridView1.Columns(5).Visible = .Recp_Step
                DataGridView1.Columns(6).Visible = .Recp_Pres
                DataGridView1.Columns(7).Visible = .Recp_Plen
                DataGridView1.Columns(8).Visible = .Recp_Temp
                DataGridView1.Columns(9).Visible = .Recp_Power
                Label9.Visible = .Recp_Power
                Label3.Visible = .Recp_MFC  'Gas 1
                Label4.Visible = .Recp_MFC  'Gas 2
                Label5.Visible = .Recp_MFC  'Gas 3
                Label6.Visible = .Recp_MFC  'Gas 4
                Label7.Visible = .Recp_MFC  'Gas 5
                Label8.Visible = .Recp_MFC  'Gas 6


                For i = 10 To 15
                    DataGridView1.Columns(i).Visible = .Recp_MFC
                Next
                cw(0) = w * (.Recp_Step + 1)
                cw(1) = w * (.Recp_Pres + 1)
                cw(2) = w * (.Recp_Plen + 1)
                cw(3) = (w + 4) * (.Recp_Temp + 1)
                cw(4) = w * (.Recp_Power + 1)
                cw(5) = 6 * w * (.Recp_MFC + 1)
                DataGridView1.Width = dgvw - cw(0) - cw(1) - cw(2) - cw(3) - cw(4) - cw(5)

                Dim x As Integer = Panel1.Location.X - cw(0) - cw(1) - cw(2) - cw(3) - cw(4)    ' - cw(5)
                Dim y As Integer = Panel1.Location.Y
                Panel1.Location = New Point(x, y)
                If (DataGridView1.Width + 41) < 400 Then
                    Me.Width = 400
                Else
                    Me.Width = DataGridView1.Width + 41
                End If

                Label11.Width = DataGridView1.Width / 2    'file name display

            End With
        Else
            Me.Text = "SDS6K-10kE RECIPE EDITOR Ver1.00" '23/10/05 Ver1.00に更新
            Button2.Visible = True 'File Save
            Button4.Visible = True 'Jump to
            TextBox1.Visible = True
            DataGridView1.Columns(5).Visible = True
            DataGridView1.Columns(6).Visible = True
            DataGridView1.Columns(7).Visible = True
            DataGridView1.Columns(8).Visible = True
            DataGridView1.Columns(9).Visible = True
            Label3.Visible = True  'Gas 1
            Label4.Visible = True  'Gas 2
            Label5.Visible = True  'Gas 3
            Label6.Visible = True  'Gas 4
            Label7.Visible = True  'Gas 5
            Label8.Visible = True  'Gas 6
            For i = 10 To 15
                DataGridView1.Columns(i).Visible = True
            Next

            DataGridView1.Width = dgvw
            Me.Width = frmw
            Label11.Width = 808    'file name display
            'DataGridView1.Width = 992
            'Me.Width = 1033
        End If

    End Sub

    Private Sub DataGridView1_CellContentClick(sender As System.Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellContentClick
        '// Change color of button's cell back color
        Dim i As Integer = e.ColumnIndex
        Dim j As Integer = e.RowIndex
        Dim v As Integer
        Dim flgstt_DM As Integer = 20012       'DM20012
        Dim flgsttTmpC_DM As Integer = 20011   'DM20011 '*Append 2021/09/08
        Dim devno As String
        Dim wv As Integer
        Dim wv_tc As Integer    '*Append 2021/09/08
        Dim rv As Integer       '*Append 2021/09/08

        If j < 0 Then Exit Sub
        With DataGridView1
            If i > 0 And i < 5 Then
                v = .Rows(j).Cells(i).Value
                If v = 0 Then
                    .Rows(j).Cells(i).Value = 1
                End If
                If v = 1 Then
                    .Rows(j).Cells(i).Value = 0
                End If
                'If i = 1 Then Rcp.RghF(j) = .Rows(j).Cells(i).Value : wv = 1 * Rcp.RghF(j) + 2 * Rcp.VntF(j) + 4 * Rcp.TmrF(j) + 8 * Rcp.SkpF(j)
                'If i = 2 Then Rcp.VntF(j) = .Rows(j).Cells(i).Value : wv = 2 * Rcp.VntF(j) + 1 * Rcp.RghF(j) + 4 * Rcp.TmrF(j) + 8 * Rcp.SkpF(j)
                'If i = 3 Then Rcp.TmrF(j) = .Rows(j).Cells(i).Value : wv = 4 * Rcp.TmrF(j) + 1 * Rcp.RghF(j) + 2 * Rcp.VntF(j) + 8 * Rcp.SkpF(j)
                'If i = 4 Then Rcp.SkpF(j) = .Rows(j).Cells(i).Value : wv = 8 * Rcp.SkpF(j) + 1 * Rcp.RghF(j) + 2 * Rcp.VntF(j) + 4 * Rcp.TmrF(j)
                If i = 1 Then
                    Rcp.RghF(j) = .Rows(j).Cells(i).Value : wv = 1 * Rcp.RghF(j)
                    'Rcp.VntF(j) = 0 : .Rows(j).Cells(i + 1).Value = 0
                    Rcp.TmrF(j) = 0 : .Rows(j).Cells(i + 2).Value = 0
                    Rcp.SkpF(j) = 0 : .Rows(j).Cells(i + 3).Value = 0
                    If .Rows(j).Cells(i).Value = 1 Then
                        .Rows(j).Cells(i).Style.BackColor = Color.Red
                    ElseIf .Rows(j).Cells(i).Value = 0 Then
                        .Rows(j).Cells(i).Style.BackColor = SystemColors.Control
                    End If
                    '.Rows(j).Cells(i + 1).Style.BackColor = SystemColors.Control
                    .Rows(j).Cells(i + 2).Style.BackColor = SystemColors.Control
                    .Rows(j).Cells(i + 3).Style.BackColor = SystemColors.Control
                End If

                '-----------------------------------------------------------------
                '                     Rcp.Tmp.C = Rcp.VntF
                '                   This flg is independent
                '-----------------------------------------------------------------
                If i = 2 Then
                    'Rcp.RghF(j) = 0 : .Rows(j).Cells(i - 1).Value = 0
                    Rcp.VntF(j) = .Rows(j).Cells(i).Value : wv_tc = (2 ^ 12) * Rcp.VntF(j)
                    'Rcp.TmrF(j) = 0 : .Rows(j).Cells(i + 1).Value = 0
                    'Rcp.SkpF(j) = 0 : .Rows(j).Cells(i + 2).Value = 0
                    If .Rows(j).Cells(i).Value = 1 Then
                        .Rows(j).Cells(i).Style.BackColor = Color.Red
                    ElseIf .Rows(j).Cells(i).Value = 0 Then
                        .Rows(j).Cells(i).Style.BackColor = SystemColors.Control
                    End If
                    '.Rows(j).Cells(i - 1).Style.BackColor = SystemColors.Control
                    '.Rows(j).Cells(i + 1).Style.BackColor = SystemColors.Control
                    '.Rows(j).Cells(i + 2).Style.BackColor = SystemColors.Control
                End If
                '------------------------------------------------------------------

                If i = 3 Then
                    Rcp.RghF(j) = 0 : .Rows(j).Cells(i - 2).Value = 0
                    'Rcp.VntF(j) = 0 : .Rows(j).Cells(i - 1).Value = 0
                    Rcp.TmrF(j) = .Rows(j).Cells(i).Value : wv = 4 * Rcp.TmrF(j)
                    Rcp.SkpF(j) = 0 : .Rows(j).Cells(i + 1).Value = 0
                    If .Rows(j).Cells(i).Value = 1 Then
                        .Rows(j).Cells(i).Style.BackColor = Color.Red
                    ElseIf .Rows(j).Cells(i).Value = 0 Then
                        .Rows(j).Cells(i).Style.BackColor = SystemColors.Control
                    End If
                    .Rows(j).Cells(i - 2).Style.BackColor = SystemColors.Control
                    '.Rows(j).Cells(i - 1).Style.BackColor = SystemColors.Control
                    .Rows(j).Cells(i + 1).Style.BackColor = SystemColors.Control
                End If
                If i = 4 Then
                    Rcp.RghF(j) = 0 : .Rows(j).Cells(i - 3).Value = 0
                    'Rcp.VntF(j) = 0 : .Rows(j).Cells(i - 2).Value = 0
                    Rcp.TmrF(j) = 0 : .Rows(j).Cells(i - 1).Value = 0
                    Rcp.SkpF(j) = .Rows(j).Cells(i).Value : wv = 8 * Rcp.SkpF(j)
                    If .Rows(j).Cells(i).Value = 1 Then
                        .Rows(j).Cells(i).Style.BackColor = Color.Red
                    ElseIf .Rows(j).Cells(i).Value = 0 Then
                        .Rows(j).Cells(i).Style.BackColor = SystemColors.Control
                    End If
                    .Rows(j).Cells(i - 3).Style.BackColor = SystemColors.Control
                    '.Rows(j).Cells(i - 2).Style.BackColor = SystemColors.Control
                    .Rows(j).Cells(i - 1).Style.BackColor = SystemColors.Control
                End If

                If i = 1 Or i = 3 Or i = 4 Then
                    devno = Format(flgstt_DM + (j * 16))
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                End If

                If i = 2 Then
                    devno = Format(flgsttTmpC_DM + (j * 16))
                    rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
                    If wv_tc = 0 Then
                        wv_tc = &HEFFF And rv
                    Else
                        wv_tc = wv_tc Or rv
                    End If
                    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv_tc)
                End If

            End If
        End With

    End Sub

    Private Sub Timer1_Tick(sender As System.Object, e As System.EventArgs) Handles Timer1.Tick
        'Cell color control
        Dim i As Integer
        Dim j As Integer
        'Exit Sub
        For i = 1 To RCP_LENG
            j = i - 1
            'Temp cell color check


            'If DataGridView1.Rows(GrdRow).Cells(8).Style.BackColor = Color.GreenYellow Then
            If Rcp.TmpCtl(j) = 1 Then
                'Pres
                If ChmbRmp_inaction = 0 Then
                    If Rcp.PresRamp(j) = 1 Then
                        DataGridView1.Rows(j).Cells(6).Style.BackColor = Color.Pink
                    Else
                        DataGridView1.Rows(j).Cells(6).Style.BackColor = DataGridView1.DefaultCellStyle.BackColor
                    End If
                Else
                    DataGridView1.Rows(j).Cells(6).Style.BackColor = Color.GreenYellow
                End If

                'Plenum
                If PlnRmp_inaction = 0 Then
                    If Rcp.PlnmRamp(j) = 1 Then
                        DataGridView1.Rows(j).Cells(7).Style.BackColor = Color.Pink
                    Else
                        DataGridView1.Rows(j).Cells(7).Style.BackColor = DataGridView1.DefaultCellStyle.BackColor
                    End If
                Else
                    DataGridView1.Rows(j).Cells(7).Style.BackColor = Color.GreenYellow
                End If

                'Power
                If PowRmp_inaction = 0 Then
                    If Rcp.MWrmp(j) = 1 Then
                        DataGridView1.Rows(j).Cells(9).Style.BackColor = Color.Pink
                    Else
                        DataGridView1.Rows(j).Cells(9).Style.BackColor = DataGridView1.DefaultCellStyle.BackColor
                    End If
                Else
                    DataGridView1.Rows(j).Cells(9).Style.BackColor = Color.GreenYellow
                End If
            Else
                'Pres
                If Rcp.PresRamp(j) = 1 Then
                    DataGridView1.Rows(j).Cells(6).Style.BackColor = Color.Pink
                Else
                    DataGridView1.Rows(j).Cells(6).Style.BackColor = DataGridView1.DefaultCellStyle.BackColor
                End If

                'Plenum
                If Rcp.PlnmRamp(j) = 1 Then
                    DataGridView1.Rows(j).Cells(7).Style.BackColor = Color.Pink
                Else
                    DataGridView1.Rows(j).Cells(7).Style.BackColor = DataGridView1.DefaultCellStyle.BackColor
                End If

                'Power
                If Rcp.MWrmp(j) = 1 Then
                    DataGridView1.Rows(j).Cells(9).Style.BackColor = Color.Pink
                Else
                    DataGridView1.Rows(j).Cells(9).Style.BackColor = DataGridView1.DefaultCellStyle.BackColor
                End If
            End If

        Next

    End Sub

    Private Sub DataGridView1_CellMouseDown(sender As Object, e As System.Windows.Forms.DataGridViewCellMouseEventArgs) Handles DataGridView1.CellMouseDown
        'Exit Sub
        Dim i As Integer = e.ColumnIndex
        Dim j As Integer = e.RowIndex
        'Dim ramp_g As New ToolStripMenuItem("Ramp", Nothing, AddressOf ToolStripMenuItem1_Click)
        'Dim non_g As New ToolStripMenuItem("Non", Nothing, AddressOf ToolStripMenuItem1_Click)

        If e.Button = Windows.Forms.MouseButtons.Right Then
            GrdClm = i
            GrdRow = j
            ''-----Append 2022/07/22-----
            ''Cells(9)-> Power
            'If DataGridView1.Rows(GrdRow).Cells(9).Style.BackColor = Color.GreenYellow Then
            '    If GrdClm <= 9 Then
            '        DataGridView1.Columns(6).ContextMenuStrip = Nothing    'Pres
            '        DataGridView1.Columns(7).ContextMenuStrip = Nothing    'Plnm
            '    Else
            '        DataGridView1.Columns(6).ContextMenuStrip = Me.ContextMenuStrip1    'Pres
            '        DataGridView1.Columns(7).ContextMenuStrip = Me.ContextMenuStrip1    'Plnm
            '    End If
            'Else
            '    DataGridView1.Columns(6).ContextMenuStrip = Me.ContextMenuStrip1    'Pres
            '    DataGridView1.Columns(7).ContextMenuStrip = Me.ContextMenuStrip1    'Plnm
            'End If
            ''---------------------------
        Else
            Exit Sub
        End If

    End Sub


    Private Sub ToolStripMenuItem1_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem1.Click
        'Pres,Plnm,Power,GAS Ramp menu
        'Dim tmpctl As Boolean = False
        'With DataGridView1
        '    If .Rows(GrdRow).Cells(9).Style.BackColor = Color.GreenYellow Then
        '        tmpctl = True
        '    End If
        'End With

        With DataGridView1

            .Rows(GrdRow).Cells(GrdClm).Style.BackColor = Color.Pink

        End With

        Dim stt_DM As Integer = 20011    'DM20011"
        Dim devno As String
        Dim rv As Integer
        Dim wv As Integer
        Dim i As Integer = GrdClm
        Dim j As Integer = GrdRow
        'devno = Format(stt_DM + (j * 13))
        devno = Format(stt_DM + (j * 16))
        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
        If i = 6 Then
            wv = rv Or &H1
            Rcp.PresRamp(j) = 1
        ElseIf i = 7 Then
            wv = rv Or &H800
            Rcp.PlnmRamp(j) = 1
        ElseIf i = 9 Then
            wv = rv Or &H2
            Rcp.MWrmp(j) = 1
        ElseIf i = 16 Then
            wv = rv Or &H200
            Rcp.HghtRamp(j) = 1
        ElseIf i = 10 Then
            wv = rv Or &H8
            Rcp.MFC1rmp(j) = 1
        ElseIf i = 11 Then
            wv = rv Or &H10
            Rcp.MFC2rmp(j) = 1
        ElseIf i = 12 Then
            wv = rv Or &H20
            Rcp.MFC3rmp(j) = 1
        ElseIf i = 13 Then
            wv = rv Or &H40
            Rcp.MFC4rmp(j) = 1
        ElseIf i = 14 Then
            wv = rv Or &H80
            Rcp.MFC5rmp(j) = 1
        ElseIf i = 15 Then
            wv = rv Or &H100
            Rcp.MFC6rmp(j) = 1
        End If
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)

    End Sub

    Private Sub ToolStripMenuItem2_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem2.Click
        'Pres,Plnm,Power,GAS Non menu
        With DataGridView1
            .Rows(GrdRow).Cells(GrdClm).Style.BackColor = .DefaultCellStyle.BackColor
        End With

        Dim stt_DM As Integer = 20011    'DM20011"
        Dim devno As String
        Dim rv As Integer
        Dim wv As Integer
        Dim i As Integer = GrdClm
        Dim j As Integer = GrdRow
        devno = Format(stt_DM + (j * 16))
        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
        If i = 6 Then
            wv = rv And Not &H1
            Rcp.PresRamp(j) = 0
        ElseIf i = 7 Then
            wv = rv And Not &H800
            Rcp.PlnmRamp(j) = 0
        ElseIf i = 9 Then
            wv = rv And Not &H2
            Rcp.MWrmp(j) = 0
        ElseIf i = 16 Then
            wv = rv And Not &H200
            Rcp.HghtRamp(j) = 0
        ElseIf i = 10 Then
            wv = rv And Not &H8
            Rcp.MFC1rmp(j) = 0
        ElseIf i = 11 Then
            wv = rv And Not &H10
            Rcp.MFC2rmp(j) = 0
        ElseIf i = 12 Then
            wv = rv And Not &H20
            Rcp.MFC3rmp(j) = 0
        ElseIf i = 13 Then
            wv = rv And Not &H40
            Rcp.MFC4rmp(j) = 0
        ElseIf i = 14 Then
            wv = rv And Not &H80
            Rcp.MFC5rmp(j) = 0
        ElseIf i = 15 Then
            wv = rv And Not &H100
            Rcp.MFC6rmp(j) = 0
        End If
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)

    End Sub

    '2022/11/09 Kajino
    'Private Sub ToolStripMenuItem3_Click(ByVal sender As Object, ByVal e As System.EventArgs)
    '    'Nouse 2022/10/20
    '    'Power Ramp menu
    '    With DataGridView1
    '        .Rows(GrdRow).Cells(GrdClm).Style.BackColor = Color.Pink
    '        .Rows(GrdRow).Cells(GrdClm + 1).Style.BackColor = .DefaultCellStyle.BackColor
    '    End With

    '    Dim stt_DM As Integer = 20011    'DM20011"
    '    Dim devno As String
    '    Dim rv As Integer
    '    Dim wv As Integer
    '    Dim i As Integer = GrdClm
    '    Dim j As Integer = GrdRow
    '    devno = Format(stt_DM + (j * 16))
    '    rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
    '    If i = 9 Then
    '        rv = rv And Not &H4 'TempCtl reset
    '        wv = rv Or &H2
    '        Rcp.MWrmp(j) = 1
    '        Rcp.TmpCtl(j) = 0
    '    End If
    '    AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)

    'End Sub

    Private Sub ToolStripMenuItem4_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem4.Click
        'Temp TempCtl menu
        Timer1.Enabled = False

        With DataGridView1
            .Rows(GrdRow).Cells(GrdClm).Style.BackColor = Color.GreenYellow
            '.Rows(GrdRow).Cells(GrdClm + 1).Style.BackColor = Color.GreenYellow
        End With

        Dim stt_DM As Integer = 20011    'DM20011"
        Dim devno As String
        Dim rv As Integer
        Dim wv As Integer
        Dim i As Integer = GrdClm
        Dim j As Integer = GrdRow
        devno = Format(stt_DM + (j * 16))
        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
        '2022/11/09 Kajino
        If i = 8 Then
            'rv = rv And Not &H2 'Ramp reset
            'wv = rv And Not &H2 'Ramp reset

            'modify the bug 2022/11/23 Kajino
            wv = rv Or &H4 'Tmp Ctl set
            'wv = rv Or &H4
            Rcp.TmpCtl(j) = 1
            'Rcp.MWrmp(j) = 0
        End If
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
        Timer1.Enabled = True

    End Sub

    Private Sub ToolStripMenuItem5_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem5.Click
        'Temp Non menu
        Timer1.Enabled = False

        With DataGridView1
            .Rows(GrdRow).Cells(GrdClm).Style.BackColor = .DefaultCellStyle.BackColor
            '.Rows(GrdRow).Cells(GrdClm + 1).Style.BackColor = .DefaultCellStyle.BackColor
        End With

        Dim stt_DM As Integer = 20011    'DM20011"
        Dim devno As String
        Dim rv As Integer
        Dim wv As Integer
        Dim i As Integer = GrdClm
        Dim j As Integer = GrdRow
        devno = Format(stt_DM + (j * 16))
        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
        '2022/11/09 Kajino
        If i = 8 Then
            'wv = rv And Not &H6

            'modify the bug 2022/11/23 Kajino
            'wv = rv And Not &H2
            wv = rv And Not &H4
            'Rcp.MWrmp(j) = 0
            Rcp.TmpCtl(j) = 0
        End If
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
        Timer1.Enabled = True

    End Sub


    Private Sub DataGridView1_CellValidated(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellValidated
        Dim i As Integer = e.ColumnIndex
        Dim j As Integer = e.RowIndex
        'flag set 
        With DataGridView1
            'If i > 0 And i < 5 Then '
            If i = 1 Then
                If .Rows(j).Cells(i).Value = 1 Then
                    .Rows(j).Cells(i).Style.BackColor = Color.Red
                ElseIf .Rows(j).Cells(i).Value = 0 Then
                    .Rows(j).Cells(i).Style.BackColor = SystemColors.Control
                End If
                '.Rows(j).Cells(i + 1).Style.BackColor = SystemColors.Control
                .Rows(j).Cells(i + 2).Style.BackColor = SystemColors.Control
                .Rows(j).Cells(i + 3).Style.BackColor = SystemColors.Control
            End If
            If i = 2 Then
                If .Rows(j).Cells(i).Value = 1 Then
                    .Rows(j).Cells(i).Style.BackColor = Color.Red
                ElseIf .Rows(j).Cells(i).Value = 0 Then
                    .Rows(j).Cells(i).Style.BackColor = SystemColors.Control
                End If
                '.Rows(j).Cells(i - 1).Style.BackColor = SystemColors.Control
                '.Rows(j).Cells(i + 1).Style.BackColor = SystemColors.Control
                '.Rows(j).Cells(i + 2).Style.BackColor = SystemColors.Control
            End If
            If i = 3 Then
                If .Rows(j).Cells(i).Value = 1 Then
                    .Rows(j).Cells(i).Style.BackColor = Color.Red
                ElseIf .Rows(j).Cells(i).Value = 0 Then
                    .Rows(j).Cells(i).Style.BackColor = SystemColors.Control
                End If
                .Rows(j).Cells(i - 2).Style.BackColor = SystemColors.Control
                '.Rows(j).Cells(i - 1).Style.BackColor = SystemColors.Control
                .Rows(j).Cells(i + 1).Style.BackColor = SystemColors.Control
            End If
            If i = 4 Then
                If .Rows(j).Cells(i).Value = 1 Then
                    .Rows(j).Cells(i).Style.BackColor = Color.Red
                ElseIf .Rows(j).Cells(i).Value = 0 Then
                    .Rows(j).Cells(i).Style.BackColor = SystemColors.Control
                End If
                .Rows(j).Cells(i - 3).Style.BackColor = SystemColors.Control
                '.Rows(j).Cells(i - 2).Style.BackColor = SystemColors.Control
                .Rows(j).Cells(i - 1).Style.BackColor = SystemColors.Control
            End If
            'End If
        End With

    End Sub

    Private Sub LinePaint()
        Static r As Boolean = False
        r = Not r
        With DataGridView1
            If r = True Then
                .Rows(0).DefaultCellStyle.BackColor = Color.LightSalmon
            Else
                .Rows(0).DefaultCellStyle.BackColor = SystemColors.Window
            End If
        End With
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        '// File Load
        If AxDBDeviceManager1.Devices(3).ValueRead = 1 Then
            Exit Sub
        End If
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim fno As Integer = FreeFile()


        If LoginMode = SUPRVISR Then
            Panel2.Enabled = True
            'superviser mode
            With OpenFileDialog1
                .InitialDirectory = ""
                .Title = "RECIPE OPEN"
                .FileName = "*.apr"
                .DefaultExt = "apr"
                If .ShowDialog() = DialogResult.OK Then
                    Rcp.Fname = .FileName
                    RecipeFile_read()
                Else
                    Exit Sub  'キャンセル
                End If

            End With
        Else
            'Operater mode
            'Display Recipe file name in ListBox
            If ListBox1.Visible = True Then
                ListBox1.Items.Clear()
                ListBox1.Visible = False
                Kyctr = 0
                DataGridView1.CausesValidation = True
                DataGridView1.Enabled = True    'Append 2021/08/29
                Panel2.Enabled = True
                Exit Sub
            End If
            Const PATH = "C:\SDS6NETSYS\RECIPE LOAD"
            Dim files As String() = System.IO.Directory.GetFiles(PATH, "*.apr", System.IO.SearchOption.AllDirectories)
            Dim n As Integer = files.Length
            Dim f(n - 1) As String

            For i = 0 To n - 1
                f(i) = Mid(files(i), Len(PATH) + 2) 'PATH delete
                f(i) = Mid(f(i), 1, Len(f(i)) - 4)  '.apr delete
            Next
            Rcp.Fname = ""
            ListBox1.Items.Clear()
            ListBox1.Items.AddRange(f)
            ListBox1.Visible = True
            ListBox1.Select()
            Kyctr = 1
            ListBox1.SelectedIndex = 0
            DataGridView1.CausesValidation = False
            DataGridView1.Enabled = False    'Append 2021/08/29
            Panel2.Enabled = False
        End If

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        '// File Save
        RecipeFile_write()
    End Sub
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        'SKIP
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1206", 1)
    End Sub
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        'JUMP
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1207", 1)
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

            Return False    '*Append 2021/5/20
        Else
            Return True
        End If

    End Function



    Private Sub TextBox1_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox1.KeyPress
        If e.KeyChar = vbCr Then
            If Check_isNumeric(TextBox1, "2240", 1) = False Then
                TextBox1.Text = ""
                TextBox1.BackColor = SystemColors.ControlLight
                Exit Sub
            End If
            If CSng(TextBox1.Text) > 100 Then
                TextBox1.Text = 100
                Jump = 100
            ElseIf CSng(TextBox1.Text) < 1 Then
                TextBox1.Text = 1
                Jump = 1
            Else
                TextBox1.Text = CInt(TextBox1.Text)
                Jump = CInt(TextBox1.Text)
            End If

            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, "2240", Jump)
            'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "3300", 1)
            TextBox1.Text = Format(Jump, "0")
            TextBox1.BackColor = SystemColors.ControlLight
        End If
    End Sub

    Private Sub TextBox1_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox1.KeyDown
        If e.KeyCode = Keys.A Then
        Else
            TextBox1.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub DataGridView1_KeyUp(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles DataGridView1.KeyUp
        Dim i, j As Integer
        Dim stt_DM As Integer = 20000    'DM20000"
        Dim devno As String
        Dim wv As Integer
        Dim rv As Single
        Dim cellval As String = ""

        '*****************************************
        If (e.Modifiers And Keys.Control) = Keys.Control Then
            If e.KeyCode = Keys.V Then
                ClipToGrid()
            End If
        End If
        '*****************************************

        If e.KeyCode = Keys.Enter Or e.KeyCode = Keys.Down Or e.KeyCode = Keys.Up Or e.KeyCode = Keys.Right Or e.KeyCode = Keys.Left Then
            With DataGridView1
                If e.KeyCode = Keys.Enter Then
                    i = .CurrentCell.RowIndex - 1
                    j = .CurrentCell.ColumnIndex

                ElseIf e.KeyCode = Keys.Up Then
                    i = .CurrentCell.RowIndex + 1
                    j = .CurrentCell.ColumnIndex

                ElseIf e.KeyCode = Keys.Down Then
                    i = .CurrentCell.RowIndex - 1
                    j = .CurrentCell.ColumnIndex
                ElseIf e.KeyCode = Keys.Left Then
                    i = .CurrentCell.RowIndex
                    j = .CurrentCell.ColumnIndex + 1

                ElseIf e.KeyCode = Keys.Right Then
                    i = .CurrentCell.RowIndex
                    j = .CurrentCell.ColumnIndex - 1
                End If

                Dim tmpctl As Boolean = False
                '2022/11/09 modify Kajino
                'With DataGridView1
                '    If .Rows(i).Cells(9).Style.BackColor = Color.GreenYellow Then
                '        tmpctl = True
                '    End If
                'End With
                With DataGridView1
                    If .Rows(i).Cells(8).Style.BackColor = Color.GreenYellow Then
                        tmpctl = True
                    End If
                End With

                Label2.Text = i

                'Cell's value "rv" is kept. Cells return to be orignal
                If j > 8 Then
                    devno = Format(stt_DM + (i * 16) + j - 6)       'Power,MFC1.....MFC6
                    If j = 16 Then
                        devno = Format(stt_DM + (i * 16) + j - 3)   'Height
                    End If
                    rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
                ElseIf j = 8 Then
                    devno = Format(stt_DM + (i * 16) + j + 2)       'Temp
                    rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
                Else
                    If j = 5 Then
                        devno = Format(stt_DM + (i * 16) + j + 9)   'StepT(32bit)
                        rv = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno))
                        '------2022/10/16 comment out
                        'ElseIf j = 6 And tmpctl = True Then
                        '    devno = Format(stt_DM + (i * 16) + j - 5)
                        '    rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
                        '    cellval = Format(rv / 10, "0.0")
                        '    .Rows(i).Cells(j).Value = cellval
                        '    Exit Sub
                        'ElseIf j = 7 And tmpctl = True Then
                        '    devno = Format(stt_DM + (i * 16) + j - 5)
                        '    rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
                        '    cellval = Format(rv / 10, "0.0")
                        '    .Rows(i).Cells(j).Value = cellval
                        '    Exit Sub
                    Else
                        devno = Format(stt_DM + (i * 16) + j - 5)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
                    End If
                End If


                '// Check that input text is numeric or not
                Dim k As Integer
                If IsNumeric(.Rows(i).Cells(j).Value) = False Then
                    Try
                        k = CInt(.Rows(i).Cells(j).Value)
                        If k = 0 Then GoTo wrtoplc
                    Catch ex As Exception

                    End Try

                    'Cell's value return to original
                    Select Case j
                        Case 0
                            Exit Sub
                        Case 5  'StepT
                            cellval = Format(rv / 60, "0.0")
                        Case 6  'Pres
                            cellval = Format(rv / 10, "0.0")
                        Case 7  'Plen
                            cellval = Format(rv / 10, "0.0")
                        Case 8  'Temp
                            cellval = Format(rv, "0")
                        Case 9  'Power
                            cellval = Format(rv, "0")
                        Case 10 'MFC1
                            cellval = Format(rv / 10000 * GasPrm.Fs(0), Dgstr(GasPrm.Decmals(0)))
                        Case 11 'MFC2
                            cellval = Format(rv / 10000 * GasPrm.Fs(1), Dgstr(GasPrm.Decmals(1)))
                        Case 12 'MFC3
                            cellval = Format(rv / 10000 * GasPrm.Fs(2), Dgstr(GasPrm.Decmals(2)))
                        Case 13 'MFC4
                            cellval = Format(rv / 10000 * GasPrm.Fs(3), Dgstr(GasPrm.Decmals(3)))
                        Case 14 'MFC5
                            cellval = Format(rv / 10000 * GasPrm.Fs(4), Dgstr(GasPrm.Decmals(4)))
                        Case 15 'MFC6
                            cellval = Format(rv / 10000 * GasPrm.Fs(5), Dgstr(GasPrm.Decmals(5)))
                        Case 16 'Height
                            cellval = Format(rv / 100, "0.0")
                    End Select
                    .Rows(i).Cells(j).Value = cellval
                    Exit Sub
                End If
wrtoplc:


                Select Case j
                    Case 0
                        Exit Sub
                    Case 5  'StepT
                        Dim ws As String
                        Rcp.StepT(i) = .Rows(i).Cells(j).Value
                        If Rcp.StepT(i) > 99999 Then
                            Rcp.StepT(i) = 99999
                        ElseIf Rcp.StepT(i) < 0 Then
                            Rcp.StepT(i) = 0
                        End If
                        ws = CStr(Rcp.StepT(i) * 60)

                        'AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        WrSigned32(ws, DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)

                        'rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 60
                        rv = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)) / 60
                        .Rows(i).Cells(j).Value = Format(rv, "0.0")

                    Case 6  'Pres
                        Rcp.Pres(i) = .Rows(i).Cells(j).Value
                        If Rcp.Pres(i) > 1000 Then Rcp.Pres(i) = 1000
                        wv = Rcp.Pres(i) * 10
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10
                        .Rows(i).Cells(j).Value = Format(rv, "0.0")
                        '---------- Append 2022/05/18 ---------
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_RLY_B, "5303", 1)


                    Case 7  'Plen
                        Rcp.Plnm(i) = .Rows(i).Cells(j).Value
                        If Rcp.Plnm(i) > 1000 Then Rcp.Plnm(i) = 1000
                        wv = Rcp.Plnm(i) * 10
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10
                        .Rows(i).Cells(j).Value = Format(rv, "0.0")

                    Case 16  'Height
                        Rcp.Hght(i) = .Rows(i).Cells(j).Value
                        If Rcp.Hght(i) > 1000 Then
                            Rcp.Hght(i) = 1000
                            'ElseIf Rcp.Hght(i) < 0 Then
                            '    Rcp.Hght(i) = 0
                        End If

                        wv = Rcp.Hght(i) * 10
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10
                        .Rows(i).Cells(j).Value = Format(rv, "0.0")

                    Case 8  'Temp
                        Rcp.Temp(i) = .Rows(i).Cells(j).Value
                        wv = Rcp.Temp(i)

                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 1
                        .Rows(i).Cells(j).Value = Format(rv, "0")

                    Case 9  'Power
                        'MwPrm.Max = 1000
                        Rcp.Power(i) = .Rows(i).Cells(j).Value
                        If Rcp.Power(i) > MwPrm.Max Then Rcp.Power(i) = MwPrm.Max
                        'If Rcp.Power(i) < 1000 Then Rcp.Power(i) = 1000                                    '2023/10/05 Sone SDS6K-10kE's minimum power is 1000W ///Need "0" for MW off
                        If Rcp.Power(i) < MwPrm.Min And Rcp.Power(i) <> 0 Then Rcp.Power(i) = MwPrm.Min     '2023/10/21 Min power (DM150) or "0"

                        wv = Rcp.Power(i) * 1
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 1
                        .Rows(i).Cells(j).Value = Format(rv, "0")

                    Case 10  'MFC1
                        Rcp.Gas1(i) = .Rows(i).Cells(j).Value
                        If Rcp.Gas1(i) > GasPrm.Fs(0) Then Rcp.Gas1(i) = GasPrm.Fs(0)
                        wv = Rcp.Gas1(i) * 10000 / GasPrm.Fs(0)
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10000 * GasPrm.Fs(0)
                        .Rows(i).Cells(j).Value = Format(rv, Dgstr(GasPrm.Decmals(0)))

                    Case 11 'MFC2
                        Rcp.Gas2(i) = .Rows(i).Cells(j).Value
                        If Rcp.Gas2(i) > GasPrm.Fs(1) Then Rcp.Gas2(i) = GasPrm.Fs(1)
                        wv = Rcp.Gas2(i) * 10000 / GasPrm.Fs(1)
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10000 * GasPrm.Fs(1)
                        .Rows(i).Cells(j).Value = Format(rv, Dgstr(GasPrm.Decmals(1)))

                    Case 12 'MFC3
                        Rcp.Gas3(i) = .Rows(i).Cells(j).Value
                        If Rcp.Gas3(i) > GasPrm.Fs(2) Then Rcp.Gas3(i) = GasPrm.Fs(2)
                        wv = Rcp.Gas3(i) * 10000 / GasPrm.Fs(2)
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10000 * GasPrm.Fs(2)
                        .Rows(i).Cells(j).Value = Format(rv, Dgstr(GasPrm.Decmals(2)))

                    Case 13 'MFC4
                        Rcp.Gas4(i) = .Rows(i).Cells(j).Value
                        If Rcp.Gas4(i) > GasPrm.Fs(3) Then Rcp.Gas4(i) = GasPrm.Fs(3)
                        wv = Rcp.Gas4(i) * 10000 / GasPrm.Fs(3)
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10000 * GasPrm.Fs(3)
                        .Rows(i).Cells(j).Value = Format(rv, Dgstr(GasPrm.Decmals(3)))

                    Case 14 'MFC5
                        Rcp.Gas5(i) = .Rows(i).Cells(j).Value
                        If Rcp.Gas5(i) > GasPrm.Fs(4) Then Rcp.Gas5(i) = GasPrm.Fs(4)
                        wv = Rcp.Gas5(i) * 10000 / GasPrm.Fs(4)
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10000 * GasPrm.Fs(4)
                        .Rows(i).Cells(j).Value = Format(rv, Dgstr(GasPrm.Decmals(4)))

                    Case 15 'MFC6
                        Rcp.Gas6(i) = .Rows(i).Cells(j).Value
                        If Rcp.Gas6(i) > GasPrm.Fs(5) Then Rcp.Gas6(i) = GasPrm.Fs(5)
                        wv = Rcp.Gas6(i) * 10000 / GasPrm.Fs(5)
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno, wv)
                        rv = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno) / 10000 * GasPrm.Fs(5)
                        .Rows(i).Cells(j).Value = Format(rv, Dgstr(GasPrm.Decmals(5)))
                End Select

                If i = RCP_LENG - 1 And (e.KeyCode = Keys.Enter Or e.KeyCode = Keys.Down) Then
                    ' .return to last row
                    .Rows(i).Cells(j).Selected = True

                End If
            End With
            'Else
            '    With DataGridView1
            '        If e.KeyCode = Keys.Up Then
            '            i = .CurrentCell.RowIndex + 1
            '        ElseIf e.KeyCode = Keys.Down Then
            '            i = .CurrentCell.RowIndex - 1
            '        End If
            '        j = .CurrentCell.ColumnIndex
            '        If j = 0 Then Exit Sub
            '        If j = 1 Then

            '        End If
            '        If j = 2 Then

            '        End If
            '        devno = Format(stt_DM + (i * 13) + j - 5)
            '        .Rows(i).Cells(j).Value = AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devno)
            '    End With

        End If
    End Sub

    Private Sub DataGridView1_UserAddedRow(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewRowEventArgs) Handles DataGridView1.UserAddedRow
        Dim i As Integer = e.Row.Index
        DataGridView1.Rows(i).Cells(0).Value = i + 1    'STEP +1
    End Sub

    Private Sub SysPrm_read()
        'Read Parameters frome System File
        Dim fnam As String = PATH & PARAMFILE_NAM
        Dim fno As Integer = FreeFile()

        Try
            FileOpen(fno, fnam, OpenMode.Input)

            Input(fno, VerInfo)
            With GasPrm
                Dim i As Integer
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
            With WtrPrm
                Input(fno, .FlowFs1)
                Input(fno, .FlowFs2)
                Input(fno, .FlowFs3)
                Input(fno, .FlowFs4)
                Input(fno, .FlowFs5)
                Input(fno, .FlowFs6)
                Input(fno, .TempFs1)
                Input(fno, .TempFs2)
                Input(fno, .TempFs3)
                Input(fno, .TempFs4)
                Input(fno, .TempFs5)
                Input(fno, .TempFs6)
            End With
            FileClose()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
        End Try

    End Sub

    'クリップボードの中身をDataGridViewに貼り付ける
    Private Sub ClipToGrid()

        Dim arStr As Array
        Dim tmpGrd As DataGridView
        Dim pasteText As String = ""
        Dim tmpStr As String = ""
        Dim i As Integer = 0
        Dim itm As Object

        Try
            tmpGrd = ActiveControl
        Catch e As Exception
            Exit Sub
        End Try

        pasteText = Clipboard.GetText()

        If (TypeOf pasteText Is System.String) = False Then Exit Sub
        If pasteText.ToString.Trim.Length <= 0 Then Exit Sub
        tmpStr = pasteText
        arStr = Split(tmpStr, vbTab)
        If arStr.Length <> tmpGrd.Columns.Count Then Exit Sub

        Dim iCRow As Integer = tmpGrd.CurrentCell.RowIndex
        Dim rw As DataGridViewRow = tmpGrd.CurrentRow

        If tmpGrd.CurrentRow.IsNewRow = True Then
            For Each itm In rw.Cells
                rw.Cells(i).Value = arStr(i)
                i += 1
            Next
        Else
            tmpGrd.Rows.Insert(iCRow, New DataGridViewRow)
            For Each itm In rw.Cells
                tmpGrd.Rows(iCRow).Cells(i).Value = arStr(i)
                i += 1
            Next
        End If
    End Sub


    Private Sub AxDBTriggerManager2_Fire(ByVal sender As Object, ByVal e As AxDATABUILDERAXLibLB._IDBTriggerEvents_FireEvent) Handles AxDBTriggerManager2.Fire
        'AUTO /STEP COUNTER/ monitor
        Dim i As Integer
        Dim j As Integer 'add kajino 2023/11/29

        AutoMode = AxDBDeviceManager1.Devices(1).ValueRead
        Step_Ctr = AxDBDeviceManager1.Devices(2).ValueRead
        'Active Line's color is changed
        If AutoMode = 1 And Step_Ctr > 0 Then
            For i = 0 To RCP_LENG - 1
                If i = Step_Ctr - 1 Then
                    DataGridView1.Rows(i).Cells(0).Style.BackColor = Color.LightSalmon

                    'add kajino 2023/11/29 レシピの実行行の値を大きくして太字にする
                    For j = 5 To 15
                        DataGridView1.Rows(i).Cells(j).Style.Font = New Font("", 10, FontStyle.Bold)
                    Next

                Else
                    DataGridView1.Rows(i).Cells(0).Style.BackColor = Color.White

                    For j = 5 To 15
                        DataGridView1.Rows(i).Cells(j).Style.Font = New Font("", 10, FontStyle.Regular)
                    Next

                End If
            Next
        Else
            For i = 0 To RCP_LENG - 1
                DataGridView1.Rows(i).Cells(0).Style.BackColor = Color.White
                For j = 5 To 15
                    DataGridView1.Rows(i).Cells(j).Style.Font = New Font("", 10, FontStyle.Regular)
                Next
            Next
        End If

    End Sub

    Private Sub RecipeFile_write()
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim fno As Integer = FreeFile()


        With DataGridView1
            For i = 0 To RCP_LENG - 1
                Rcp.RghF(i) = .Rows(i).Cells(1).Value
                Rcp.VntF(i) = .Rows(i).Cells(2).Value
                Rcp.TmrF(i) = .Rows(i).Cells(3).Value
                Rcp.SkpF(i) = .Rows(i).Cells(4).Value

                Rcp.StepT(i) = .Rows(i).Cells(5).Value
                Rcp.Pres(i) = .Rows(i).Cells(6).Value
                Rcp.Plnm(i) = .Rows(i).Cells(7).Value
                Rcp.Temp(i) = .Rows(i).Cells(8).Value
                Rcp.Power(i) = .Rows(i).Cells(9).Value

                Rcp.Gas1(i) = .Rows(i).Cells(10).Value
                Rcp.Gas2(i) = .Rows(i).Cells(11).Value
                Rcp.Gas3(i) = .Rows(i).Cells(12).Value
                Rcp.Gas4(i) = .Rows(i).Cells(13).Value
                Rcp.Gas5(i) = .Rows(i).Cells(14).Value
                Rcp.Gas6(i) = .Rows(i).Cells(15).Value
                Rcp.Hght(i) = .Rows(i).Cells(16).Value
            Next
        End With
        With SaveFileDialog1
            .Title = "RECIPE SAVE"
            .FileName = "*.apr"
            .DefaultExt = "apr"
            If .ShowDialog() = DialogResult.OK Then
                Rcp.Fname = .FileName
            Else
                Exit Sub  'キャンセル
            End If
        End With

        FileOpen(fno, Rcp.Fname, OpenMode.Output)
        With Rcp
            For i = 0 To RCP_LENG - 1
                WriteLine(fno, .RghF(i), .VntF(i), .TmrF(i), .SkpF(i), .StepT(i), .Pres(i), .Plnm(i), .Power(i) _
                                                   , .Gas1(i), .Gas2(i), .Gas3(i), .Gas4(i), .Gas5(i), .Gas6(i))
            Next
            'Append 20160729
            For i = 0 To RCP_LENG - 1
                WriteLine(fno, .Temp(i), .PresRamp(i), .PlnmRamp(i), .MWrmp(i), .TmpCtl(i), .MFC1rmp(i), .MFC2rmp(i), .MFC3rmp(i) _
                          , .MFC4rmp(i), .MFC5rmp(i), .MFC6rmp(i), .temp1(i), .temp2(i), .temp3(i))
            Next
        End With
        FileClose()
        Label11.Text = Rcp.Fname

    End Sub

    Private Sub RecipeFile_read()
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim fno As Integer = FreeFile()

        Try
            FileOpen(fno, Rcp.Fname, OpenMode.Input)
        Catch ex As Exception
            MessageBox.Show("File can't open")
            Exit Sub
        End Try


        Me.Cursor = Cursors.WaitCursor
        With Rcp
            For i = 0 To RCP_LENG - 1
                If EOF(fno) Then Exit For
                Input(fno, .RghF(i))
                Input(fno, .VntF(i))
                Input(fno, .TmrF(i))
                Input(fno, .SkpF(i))

                Input(fno, .StepT(i))
                Input(fno, .Pres(i))
                Input(fno, .Plnm(i))
                Input(fno, .Power(i))

                Input(fno, .Gas1(i))
                Input(fno, .Gas2(i))
                Input(fno, .Gas3(i))
                Input(fno, .Gas4(i))
                Input(fno, .Gas5(i))
                Input(fno, .Gas6(i))
            Next

            'Append 20160729
            For i = 0 To RCP_LENG - 1
                If EOF(fno) Then Exit For
                Try
                    Input(fno, .Temp(i))
                    Input(fno, .PresRamp(i))
                    Input(fno, .PlnmRamp(i))    '.HghtRamp(i))
                    Input(fno, .MWrmp(i))
                    Input(fno, .TmpCtl(i))
                    Input(fno, .MFC1rmp(i))
                    Input(fno, .MFC2rmp(i))
                    Input(fno, .MFC3rmp(i))
                    Input(fno, .MFC4rmp(i))
                    Input(fno, .MFC5rmp(i))
                    Input(fno, .MFC6rmp(i))
                    Input(fno, .temp1(i))
                    Input(fno, .temp2(i))
                    Input(fno, .temp3(i))
                Catch ex As Exception
                    'MessageBox.Show(ex.Message)
                End Try
            Next
        End With
        FileClose()
        If LoginMode = OPRATR Then
            Dim fnm As String
            Dim s As String
            Const PATH As String = "C:\SDS6NETSYS\RECIPE LOAD\"
            s = Mid(Rcp.Fname, Len(PATH) + 1)             'PATH delete
            fnm = Mid(s, 1, Len(s) - 4)     '.apr delete
            Label11.Text = fnm
        Else
            Label11.Text = Rcp.Fname
        End If


        'With DataGridView1
        '    For i = 0 To RCP_LENG - 1
        '        .Rows(i).Cells(1).Value = Rcp.RghF(i)
        '        .Rows(i).Cells(2).Value = Rcp.VntF(i)
        '        .Rows(i).Cells(3).Value = Rcp.TmrF(i)
        '        .Rows(i).Cells(4).Value = Rcp.SkpF(i)
        '        .Rows(i).Cells(5).Value = Rcp.StepT(i)
        '        .Rows(i).Cells(6).Value = Rcp.Pres(i)
        '        .Rows(i).Cells(7).Value = Rcp.Plnm(i)
        '        .Rows(i).Cells(8).Value = Rcp.Power(i)
        '        .Rows(i).Cells(9).Value = Rcp.Gas1(i)
        '        .Rows(i).Cells(10).Value = Rcp.Gas2(i)
        '        .Rows(i).Cells(11).Value = Rcp.Gas3(i)
        '        .Rows(i).Cells(12).Value = Rcp.Gas4(i)
        '        .Rows(i).Cells(13).Value = Rcp.Gas5(i)
        '        .Rows(i).Cells(14).Value = Rcp.Gas6(i)
        '    Next
        'End With

        Dim device(1600) As DATABUILDERAXLibLB.DBDevice
        'Dim device(2600) As DATABUILDERAXLibLB.DBDevice
        Dim k As Integer
        Dim ctlwrd As Integer = 0

        For i = 1 To 1600   '1300
            device(i) = AxDBDeviceManager2.Devices.Item(i)
        Next
        For i = 1 To RCP_LENG
            k = (i - 1) * 16
            If Rcp.RghF(i - 1) = 1 Then device(k + 13).ValueWrite = 1
            'If Rcp.VntF(i - 1) = 1 Then device(k + 13).ValueWrite = 2  *Comment out 2021/09/08
            If Rcp.TmrF(i - 1) = 1 Then device(k + 13).ValueWrite = 4
            If Rcp.SkpF(i - 1) = 1 Then device(k + 13).ValueWrite = 8

            'If Rcp.RghF(i - 1) = 0 And Rcp.VntF(i - 1) = 0 And Rcp.TmrF(i - 1) = 0 And Rcp.SkpF(i - 1) = 0 Then
            '    device(k + 13).ValueWrite = 0
            'End If

            If Rcp.RghF(i - 1) = 0 And Rcp.TmrF(i - 1) = 0 And Rcp.SkpF(i - 1) = 0 Then
                device(k + 13).ValueWrite = 0
            End If

            'device(k + 14).ValueWrite = Rcp.StepT(i - 1) * 60  'Comment out Rcp.StepT 16bit->32bit
            device(k + 2).ValueWrite = Rcp.Pres(i - 1) * 10
            device(k + 3).ValueWrite = Rcp.Plnm(i - 1) * 10
            device(k + 4).ValueWrite = Rcp.Power(i - 1)
            device(k + 5).ValueWrite = Rcp.Gas1(i - 1) * 10000 / GasPrm.Fs(0)
            device(k + 6).ValueWrite = Rcp.Gas2(i - 1) * 10000 / GasPrm.Fs(1)
            device(k + 7).ValueWrite = Rcp.Gas3(i - 1) * 10000 / GasPrm.Fs(2)
            device(k + 8).ValueWrite = Rcp.Gas4(i - 1) * 10000 / GasPrm.Fs(3)
            device(k + 9).ValueWrite = Rcp.Gas5(i - 1) * 10000 / GasPrm.Fs(4)
            device(k + 10).ValueWrite = Rcp.Gas6(i - 1) * 10000 / GasPrm.Fs(5)
            device(k + 11).ValueWrite = Rcp.Temp(i - 1)

            ctlwrd = 0
            If Rcp.PresRamp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H1
            If Rcp.MWrmp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H2
            If Rcp.TmpCtl(i - 1) = 1 Then ctlwrd = ctlwrd Or &H4
            If Rcp.MFC1rmp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H8
            If Rcp.MFC2rmp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H10
            If Rcp.MFC3rmp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H20
            If Rcp.MFC4rmp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H40
            If Rcp.MFC5rmp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H80
            If Rcp.MFC6rmp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H100
            If Rcp.HghtRamp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H200
            If Rcp.PlnmRamp(i - 1) = 1 Then ctlwrd = ctlwrd Or &H800
            If Rcp.VntF(i - 1) = 1 Then ctlwrd = ctlwrd Or &H1000   '4096   *Append 2021/09/08

            device(k + 12).ValueWrite = ctlwrd
        Next
        'device(i) = AxDBDeviceManager2.Devices.Item(i)
        WrComp = False
        AxDBDeviceManager2.WriteAll()
        Do
            My.Application.DoEvents()
        Loop Until WrComp = True

        '*Append 20210523
        Dim wstr As String
        For i = 1 To RCP_LENG
            k = (i - 1) * 16
            wstr = CStr(Rcp.StepT(i - 1) * 60)
            WrSigned32(wstr, DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, 20000 + k + 14)
        Next

        RdComp = False
        AxDBDeviceManager2.ReadAll()
        Do
            My.Application.DoEvents()
        Loop Until RdComp = True

        For i = 1 To RCP_LENG
            k = (i - 1) * 16
            Rcp.StepT(i - 1) = CSng(RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, 20000 + k + 14)) / 60  '*Change 20210523
            Rcp.Pres(i - 1) = CSng(device(k + 2).ValueRead / 10)
            Rcp.Plnm(i - 1) = CSng(device(k + 3).ValueRead / 10)
            Rcp.Power(i - 1) = CSng(device(k + 4).ValueRead)
            Rcp.Temp(i - 1) = device(k + 11).ValueRead
            Rcp.Gas1(i - 1) = CSng(device(k + 5).ValueRead / 10000 * GasPrm.Fs(0))
            Rcp.Gas2(i - 1) = CSng(device(k + 6).ValueRead / 10000 * GasPrm.Fs(1))
            Rcp.Gas3(i - 1) = CSng(device(k + 7).ValueRead / 10000 * GasPrm.Fs(2))
            Rcp.Gas4(i - 1) = CSng(device(k + 8).ValueRead / 10000 * GasPrm.Fs(3))
            Rcp.Gas5(i - 1) = CSng(device(k + 9).ValueRead / 10000 * GasPrm.Fs(4))
            Rcp.Gas6(i - 1) = CSng(device(k + 10).ValueRead / 10000 * GasPrm.Fs(5))

            If device(k + 13).ValueRead = 1 Then Rcp.RghF(i - 1) = 1 Else Rcp.RghF(i - 1) = 0
            If device(k + 12).ValueRead >= 4096 Then Rcp.VntF(i - 1) = 1 Else Rcp.VntF(i - 1) = 0
            If device(k + 13).ValueRead = 4 Then Rcp.TmrF(i - 1) = 1 Else Rcp.TmrF(i - 1) = 0
            If device(k + 13).ValueRead = 8 Then Rcp.SkpF(i - 1) = 1 Else Rcp.SkpF(i - 1) = 0
            With DataGridView1
                .Rows(i - 1).Cells(1).Value = Rcp.RghF(i - 1)
                .Rows(i - 1).Cells(2).Value = Rcp.VntF(i - 1)
                .Rows(i - 1).Cells(3).Value = Rcp.TmrF(i - 1)
                .Rows(i - 1).Cells(4).Value = Rcp.SkpF(i - 1)
                For j = 1 To 4
                    If .Rows(i - 1).Cells(j).Value = 1 Then
                        .Rows(i - 1).Cells(j).Style.BackColor = Color.Red
                    ElseIf .Rows(i - 1).Cells(j).Value = 0 Then
                        .Rows(i - 1).Cells(j).Style.BackColor = SystemColors.Control
                    End If
                Next
                .Rows(i - 1).Cells(5).Value = Format(Rcp.StepT(i - 1), "0.0")  'StepT set
                .Rows(i - 1).Cells(6).Value = Format(Rcp.Pres(i - 1), "0.0")   'Pres set
                .Rows(i - 1).Cells(7).Value = Format(Rcp.Plnm(i - 1), "0.0")   'Plnm set
                .Rows(i - 1).Cells(8).Value = Format(Rcp.Temp(i - 1), "0")     'Temp set
                .Rows(i - 1).Cells(9).Value = Format(Rcp.Power(i - 1), "0")    'Power set
                .Rows(i - 1).Cells(10).Value = Format(Rcp.Gas1(i - 1), Dgstr(GasPrm.Decmals(0)))   'MFC1 set
                .Rows(i - 1).Cells(11).Value = Format(Rcp.Gas2(i - 1), Dgstr(GasPrm.Decmals(1)))  'MFC2 set
                .Rows(i - 1).Cells(12).Value = Format(Rcp.Gas3(i - 1), Dgstr(GasPrm.Decmals(2)))  'MFC3 set
                .Rows(i - 1).Cells(13).Value = Format(Rcp.Gas4(i - 1), Dgstr(GasPrm.Decmals(3)))  'MFC4 set
                .Rows(i - 1).Cells(14).Value = Format(Rcp.Gas5(i - 1), Dgstr(GasPrm.Decmals(4)))  'MFC5 set
                .Rows(i - 1).Cells(15).Value = Format(Rcp.Gas6(i - 1), Dgstr(GasPrm.Decmals(5)))  'MFC6 set

                If (device(k + 12).ValueRead And &H1) = &H1 Then
                    Rcp.PresRamp(i - 1) = 1
                    .Rows(i - 1).Cells(6).Style.BackColor = Color.Pink
                Else
                    Rcp.PresRamp(i - 1) = 0
                    .Rows(i - 1).Cells(6).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                '*2022/02/04 Append
                If (device(k + 12).ValueRead And &H800) = &H800 Then
                    Rcp.PlnmRamp(i - 1) = 1
                    .Rows(i - 1).Cells(7).Style.BackColor = Color.Pink
                Else
                    Rcp.PlnmRamp(i - 1) = 0
                    .Rows(i - 1).Cells(7).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                'If (device(k + 12).ValueRead And &H2) = &H2 Then
                '    Rcp.MWrmp(i - 1) = 1
                '    .Rows(i - 1).Cells(9).Style.BackColor = Color.Pink
                '    .Rows(i - 1).Cells(8).Style.BackColor = .DefaultCellStyle.BackColor
                'ElseIf (device(k + 12).ValueRead And &H4) = &H4 Then
                '    Rcp.MWrmp(i - 1) = 0
                '    Rcp.TmpCtl(i - 1) = 1
                '    .Rows(i - 1).Cells(8).Style.BackColor = Color.GreenYellow
                '    .Rows(i - 1).Cells(9).Style.BackColor = Color.GreenYellow
                'Else
                '    Rcp.TmpCtl(i - 1) = 0
                '    .Rows(i - 1).Cells(8).Style.BackColor = .DefaultCellStyle.BackColor
                '    .Rows(i - 1).Cells(9).Style.BackColor = .DefaultCellStyle.BackColor
                'End If
                '2022/11/09 modify Kajino
                If (device(k + 12).ValueRead And &H4) = &H4 Then
                    Rcp.TmpCtl(i - 1) = 1
                    .Rows(i - 1).Cells(8).Style.BackColor = Color.GreenYellow
                Else
                    Rcp.TmpCtl(i - 1) = 0
                    .Rows(i - 1).Cells(8).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H2) = &H2 Then
                    Rcp.MWrmp(i - 1) = 1
                    .Rows(i - 1).Cells(9).Style.BackColor = Color.Pink
                Else
                    Rcp.MWrmp(i - 1) = 0
                    .Rows(i - 1).Cells(9).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H8) = &H8 Then
                    Rcp.MFC1rmp(i - 1) = 1
                    .Rows(i - 1).Cells(10).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC1rmp(i - 1) = 0
                    .Rows(i - 1).Cells(10).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H10) = &H10 Then
                    Rcp.MFC2rmp(i - 1) = 1
                    .Rows(i - 1).Cells(11).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC2rmp(i - 1) = 0
                    .Rows(i - 1).Cells(11).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H20) = &H20 Then
                    Rcp.MFC3rmp(i - 1) = 1
                    .Rows(i - 1).Cells(12).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC3rmp(i - 1) = 0
                    .Rows(i - 1).Cells(12).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H40) = &H40 Then
                    Rcp.MFC4rmp(i - 1) = 1
                    .Rows(i - 1).Cells(13).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC4rmp(i - 1) = 0
                    .Rows(i - 1).Cells(13).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H80) = &H80 Then
                    Rcp.MFC5rmp(i - 1) = 1
                    .Rows(i - 1).Cells(14).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC5rmp(i - 1) = 0
                    .Rows(i - 1).Cells(14).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                If (device(k + 12).ValueRead And &H100) = &H100 Then
                    Rcp.MFC6rmp(i - 1) = 1
                    .Rows(i - 1).Cells(15).Style.BackColor = Color.Pink
                Else
                    Rcp.MFC6rmp(i - 1) = 0
                    .Rows(i - 1).Cells(15).Style.BackColor = .DefaultCellStyle.BackColor
                End If

                '*2022/02/04 Comment out
                'If (device(k + 12).ValueRead And &H200) = &H200 Then
                '    Rcp.HghtRamp(i - 1) = 1
                '    .Rows(i - 1).Cells(7).Style.BackColor = Color.Pink
                'Else
                '    Rcp.HghtRamp(i - 1) = 0
                '    .Rows(i - 1).Cells(7).Style.BackColor = .DefaultCellStyle.BackColor
                'End If

            End With
        Next

        Me.Cursor = Cursors.Default

    End Sub


    Private Sub AxDBDeviceManager2_AfterRead(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager2.AfterRead
        RdComp = True
    End Sub

    Private Sub AxDBDeviceManager2_AfterWrite(ByVal sender As Object, ByVal e As System.EventArgs) Handles AxDBDeviceManager2.AfterWrite
        WrComp = True
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

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        'for debugg
        'Dim wv As Integer
        Label2.Text = 2 ^ 12
        'wv = CInt(TextBox2.Text)
        'WrSigned32(wv, DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, 20013)
        'Label10.Text = RdSigned32(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, 20013)
    End Sub


    Private Sub ListBox1_MouseClick(sender As Object, e As MouseEventArgs) Handles ListBox1.MouseClick
        Const PATH As String = "C:\SDS6NETSYS\RECIPE LOAD\"

        Rcp.Fname = PATH & ListBox1.SelectedItem & ".apr"
        ListBox1.Visible = False
        ListBox1.Items.Clear()
        RecipeFile_read()
        DataGridView1.CausesValidation = True
        DataGridView1.Enabled = True    'Append 2021/08/29
        Panel2.Enabled = True

    End Sub

    Private Sub ReadCtlvsblPrm()
        'Read Parameters of controls visible
        Dim fnam As String = PATH & "CtlVsPrm.txt"
        Dim fno As Integer = FreeFile()

        Try
            FileOpen(fno, fnam, OpenMode.Input)

            Input(fno, VerInfo)
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
                'Input(fno, .WL_Screen)
            End With

            FileClose()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
            FileClose()
        End Try

    End Sub

    Private Sub ListBox1_MouseMove(sender As Object, e As MouseEventArgs) Handles ListBox1.MouseMove
        Dim mousY As Integer = e.Y
        Dim i As Integer
        Dim k As Integer = ListBox1.Items.Count
        Label2.Text = mousY

        With ListBox1
            i = (mousY / .ItemHeight) + 1
            If i <= .Items.Count Then
                .SelectedIndex = i - 1
            End If
        End With
    End Sub

    Private Sub ListBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles ListBox1.KeyPress
        If e.KeyChar = vbCr Then
            Const PATH As String = "C:\SDS6NETSYS\RECIPE LOAD\"

            Rcp.Fname = PATH & ListBox1.SelectedItem & ".apr"
            ListBox1.Visible = False
            ListBox1.Items.Clear()
            RecipeFile_read()
            DataGridView1.CausesValidation = True
            DataGridView1.Enabled = True
            Kyctr = 0
            Panel2.Enabled = True
        End If
    End Sub

    Private Sub ListBox1_KeyUp(sender As Object, e As KeyEventArgs) Handles ListBox1.KeyUp
        If e.KeyCode = Keys.Down Then
            Kyctr = Kyctr + 1
            If Kyctr > ListBox1.Items.Count Then
                Kyctr = ListBox1.Items.Count
            End If
            If Kyctr <= ListBox1.Items.Count Then
                ListBox1.SelectedIndex = Kyctr - 1
            End If
        End If
        If e.KeyCode = Keys.Up Then
            Kyctr = Kyctr - 1
            If Kyctr <= 0 Then
                Kyctr = 1
            End If
            If Kyctr <= ListBox1.Items.Count Then
                ListBox1.SelectedIndex = Kyctr - 1
            End If
        End If
        Label2.Text = Kyctr
    End Sub

    Private Sub AxDBDeviceManager3_AfterRead(sender As Object, e As EventArgs) Handles AxDBDeviceManager3.AfterRead
        'Check the current value in action  Append 2022/10/20
        PowRmp_inaction = AxDBDeviceManager3.Devices(5).ValueRead
        PlnRmp_inaction = AxDBDeviceManager3.Devices(9).ValueRead
        ChmbRmp_inaction = AxDBDeviceManager3.Devices(14).ValueRead
    End Sub
End Class
