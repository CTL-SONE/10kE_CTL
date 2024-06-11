Imports System.ComponentModel

Public Class frmTmpCTL
    '*******************************************
    '       TEMP CONTROL Ver1.01
    '           2022/07/22
    '       TEMP CONTROL Ver1.02
    '           2022/08/08
    '           Indicator追加
    '       TEMP CONTROL Ver1.03
    '           2023/02/24
    '           ISROUGHからの要望によりPLEN最低値を5Torrに変更
    '       TEMP CONTROL Ver2.0
    '           2023/03/13
    '           ATC RAMPはSLOPEに変更
    '           レシピ動作時はTemp.Spはグレーアウトに変更
    '       TEMP CONTROL Ver2.1
    '           2023/11/07
    '           Safety area 3' or 4' is selected, ATC ramp is 0
    '*******************************************
    Structure TempControl_parameter
        Dim Tmp_setval As Single
        Dim Tmp_tolrnc As Single
        Dim Pow_LmtHp As Single
        Dim Pow_LmtLm As Single
        Dim Pow_Rmp As Single
        Dim Pow_smpT As Integer
        Dim Pln_LmtH As Single
        Dim Pln_LmtL As Single
        Dim Pln_Rmp As Single
        Dim Pln_Gap As Single
        Dim Pln_smpT As Single
        Dim Chmb_LmtH As Single
        Dim Chmb_LmtL As Single
        Dim Chmb_Rmp As Single
        Dim Chmb_smpT As Integer
    End Structure

    Structure Mesured
        Dim SetVal As Single    'degC
        Dim PowRefVal As Single 'W
        Dim PowLmtH As Single   'W
        Dim PowLmtL As Single   'W
        Dim Pres As Single   'Torr
        Dim MW As Single     'W
        Dim Plen As Single   'Torr
    End Structure

    Const PATH As String = "C:\SDS6NETSYS\"
    Const PARAMFILE_NAM As String = "Param.txt"

    Dim RdComp() As Boolean = {False, False, False, False}
    Dim WrComp() As Boolean = {False, False, False, False}

    Dim VonCLR As System.Drawing.Color = Color.Red          'Valve ON color
    Dim VofCLR As System.Drawing.Color = Color.Black        'Valve OFF color
    Dim DonCLR As System.Drawing.Color = Color.Red          'Device ON color
    Dim DofCLR As System.Drawing.Color = Color.Yellow       'Device OFF color

    Dim LonCLR As System.Drawing.Color = Color.Lime         'Lmp ON color
    Dim LofCLR As System.Drawing.Color = Color.Silver       'Lmp OFF color
    Dim SonCLR As System.Drawing.Color = Color.Coral        'Sw ON color
    Dim SofCLR As System.Drawing.Color = Color.Transparent  'Sw OFF color
    Dim AonCLR As System.Drawing.Color = Color.Red          'Alm ON color
    Dim AofCLR As System.Drawing.Color = Color.DimGray      'Alm OFF color
    Dim Aontext As System.Drawing.Color = Color.DarkSalmon
    Dim Aon2text As System.Drawing.Color = Color.OrangeRed
    Dim Aofftext As System.Drawing.Color = Color.Gainsboro

    Dim Dgstr(3) As String      '"#.###"
    Dim Dpntpos(6) As Integer

    Public TmpCtl_set As TempControl_parameter
    Public TmpCtl_inAct As TempControl_parameter
    Public Mes As Mesured

    '-----------2022/08/08 Add----------   ATC ICONs
    Dim AtcIconNumb_MW As Single
    Dim AtcIconNumb_PL As Single
    Dim AtcIconNumb_CH As Single

    Dim PLC_Connected As Boolean    'Append 2022/05/29

    Private Sub frmTmpCTL_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        '===============================================
        '                   Initialize
        '===============================================
        'Forbid Multi Execution
        If Diagnostics.Process.GetProcessesByName(Diagnostics.Process.GetCurrentProcess.ProcessName).Length > 1 Then
            Application.Exit()
        End If

        '-------- Rev.2022/02
        'IP and Version of PLC are stored in HKEY_CURRENT_USER\SoftWare\SDS\PLC
        AxDBCommManager1.Peer = RgsPlcIp() & ":8500"    'default 192.168.3.13 & port 8500
        AxDBCommManager1.PLC = CInt(RgsPlcSr())         'default 515 = KV-5500/5000/3000

        'Try
        '    AxDBCommManager1.Connect()
        '    AxDBTriggerManager1.Active = True
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

        Dgstr(0) = "0"
        Dgstr(1) = "0.0"
        Dgstr(2) = "0.00"
        Dgstr(3) = "0.000"


        If frmControl.LoginMode = frmControl.OPRATR Then
            With frmControl.Ctlvsbl

                Button32.Visible = .TmpCtl_START
                Panel1.Visible = .TmpCtl_SetPoint
                Panel2.Visible = .TmpCtl_SetPoint
                Panel3.Visible = .TmpCtl_SetPoint
                Panel4.Visible = .TmpCtl_SetPoint

                Panel5.Visible = .TmpCtl_InAction
                Panel6.Visible = .TmpCtl_InAction
                Panel7.Visible = .TmpCtl_InAction
                Panel8.Visible = .TmpCtl_InAction

                Panel9.Visible = .TmpCtl_Measured
                Panel10.Visible = .TmpCtl_Measured
            End With
        Else
            Button32.Visible = True
            Panel1.Visible = True
            Panel2.Visible = True
            Panel3.Visible = True
            Panel4.Visible = True

            Panel5.Visible = True
            Panel6.Visible = True
            Panel7.Visible = True
            Panel8.Visible = True

            Panel9.Visible = True
            Panel10.Visible = True
        End If

        Do
            Application.DoEvents()
        Loop Until RdComp(1) = True 'Read out set values

        Dim rd(16) As Integer

        '-----------2023/11/07 Sone Form Load時に、SAFTY areaの値読み込み追加-------------
        Dim rdval As Long

        Dim deviceSA As DATABUILDERAXLibLB.DBDevice
        deviceSA = AxDBDeviceManager2.Devices.Item(16)
        rdval = deviceSA.ValueRead

        '-------------------------------------

        Dim i As Integer
        For i = 1 To 16
            rd(i) = AxDBDeviceManager2.Devices(i).ValueRead
        Next
        TmpCtl_set.Tmp_setval = rd(1)
        TmpCtl_set.Tmp_tolrnc = rd(2)

        TmpCtl_set.Pow_LmtHp = rd(3)
        TmpCtl_set.Pow_LmtLm = rd(4)
        TmpCtl_set.Pow_Rmp = rd(5)
        TmpCtl_set.Pow_smpT = rd(6)

        TmpCtl_set.Pln_LmtH = rd(7)
        TmpCtl_set.Pln_LmtL = rd(8)
        TmpCtl_set.Pln_Rmp = rd(9)
        TmpCtl_set.Pln_Gap = rd(10)
        TmpCtl_set.Pln_smpT = rd(11)

        TmpCtl_set.Chmb_LmtH = rd(12)
        TmpCtl_set.Chmb_LmtL = rd(13)
        TmpCtl_set.Chmb_Rmp = rd(14)
        TmpCtl_set.Chmb_smpT = rd(15)

        TextBox21.Text = Format(TmpCtl_set.Tmp_setval, "0.0")
        TextBox25.Text = TmpCtl_set.Tmp_tolrnc

        TextBox2.Text = TmpCtl_set.Pow_LmtHp
        TextBox1.Text = TmpCtl_set.Pow_LmtLm
        TextBox4.Text = TmpCtl_set.Pow_Rmp
        TextBox3.Text = TmpCtl_set.Pow_smpT

        TextBox8.Text = Format(TmpCtl_set.Pln_LmtH / 10, "0.0")
        TextBox7.Text = Format(TmpCtl_set.Pln_LmtL / 10, "0.0")
        TextBox6.Text = Format(TmpCtl_set.Pln_Rmp / 10, "0.0")
        TextBox9.Text = Format(TmpCtl_set.Pln_Gap / 10, "0.0")
        TextBox5.Text = Format(TmpCtl_set.Pln_smpT / 10, "0.0")

        TextBox14.Text = Format(TmpCtl_set.Chmb_LmtH / 10, "0.0")
        TextBox13.Text = Format(TmpCtl_set.Chmb_LmtL / 10, "0.0")
        TextBox12.Text = Format(TmpCtl_set.Chmb_Rmp / 10, "0.0")
        TextBox11.Text = TmpCtl_set.Chmb_smpT

        If rdval = &H4 Then '2023/11/07 Sone 2'のセーフティエリア選択時は、適用 10進数では4、16進数でも4
            TextBox4.Text = TmpCtl_set.Pow_Rmp
            TextBox6.Text = Format(TmpCtl_set.Pln_Rmp / 10, "0.0")
            TextBox12.Text = Format(TmpCtl_set.Chmb_Rmp / 10, "0.0")
        Else '2023/11/07 Sone 3' or 4'のセーフティエリア選択時は、Rampパラメータを0に変更
            TextBox4.Text = 0 'Pwr Ramp SP
            TextBox6.Text = 0 'PLEN Ramp SP
            TextBox12.Text = 0 'CHMB Ramp SP
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


    Private Sub frmTmpCTL_Closed(sender As Object, e As EventArgs) Handles Me.Closed
        Try
            AxDBCommManager1.Disconnect()
            AxDBTriggerManager1.Active = False
            AxDBCommManager1.Dispose()
        Catch ex As Exception
            MessageBox.Show(ex.Message, "COM+")
            Exit Sub
        End Try
    End Sub

    Private Sub AxDBDeviceManager1_AfterRead(sender As Object, e As EventArgs) Handles AxDBDeviceManager1.AfterRead
        'Read out datas of Indicator
        RdComp(0) = True
    End Sub



    Private Sub AxDBDeviceManager2_AfterRead(sender As Object, e As EventArgs) Handles AxDBDeviceManager2.AfterRead
        'read out set point
        RdComp(1) = True
    End Sub

    Private Sub AxDBDeviceManager3_AfterRead(sender As Object, e As EventArgs) Handles AxDBDeviceManager3.AfterRead
        'read out inAction data
        RdComp(2) = True
    End Sub

    Private Sub AxDBDeviceManager4_AfterRead(sender As Object, e As EventArgs) Handles AxDBDeviceManager4.AfterRead
        'read out measured data
        RdComp(3) = True
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'Disp each data
        Dim i As Integer
        Dim rd As Integer

        If RdComp(0) = True Then
            'TEMP Lamp M7500
            If AxDBDeviceManager1.Devices(1).ValueRead = 1 Then
                Label1.BackColor = LonCLR
            Else
                Label1.BackColor = LofCLR
            End If
            ''POWER Lamp M7501
            'If AxDBDeviceManager1.Devices(2).ValueRead = 1 Then
            '    Label182.BackColor = LonCLR
            'Else
            '    Label182.BackColor = LofCLR
            'End If
            ''PLEN Lamp M7502
            'If AxDBDeviceManager1.Devices(3).ValueRead = 1 Then
            '    Label191.BackColor = LonCLR
            'Else
            '    Label191.BackColor = LofCLR
            'End If
            ''CHMB Lamp M7503
            'If AxDBDeviceManager1.Devices(4).ValueRead = 1 Then
            '    Label51.BackColor = LonCLR
            'Else
            '    Label51.BackColor = LofCLR
            'End If

            If AxDBDeviceManager1.Devices(5).ValueRead = 1 Then    'M1410
                Button32.Text = "STOP"
                Button32.BackColor = SonCLR
            Else
                Button32.Text = "START"
                Button32.BackColor = SofCLR
            End If

            'If AxDBDeviceManager1.Devices(6).ValueRead = 1 Then    'M1411 不要
            '    Label129.BackColor = LonCLR
            'Else
            '    Label129.BackColor = LofCLR
            'End If

            '23/3/13 Sone レシピ動作中はTemp.Spをグレー表示に変更
            If AxDBDeviceManager1.Devices(7).ValueRead = 1 Then    'M3001
                TextBox21.Enabled = False
            Else
                TextBox21.Enabled = True
            End If

            RdComp(0) = False
        End If

        If RdComp(2) = True Then
            With TmpCtl_inAct
                For i = 1 To 15
                    rd = AxDBDeviceManager3.Devices(i).ValueRead

                    Select Case i
                        Case 1 : .Tmp_setval = rd / 1
                        Case 2 : .Tmp_tolrnc = rd / 1
                        Case 3 : .Pow_LmtHp = rd / 1
                        Case 4 : .Pow_LmtLm = rd / 1
                        Case 5 : .Pow_Rmp = rd / 1
                        Case 6 : .Pow_smpT = rd / 1
                        Case 7 : .Pln_LmtH = rd / 10
                        Case 8 : .Pln_LmtL = rd / 10
                        Case 9 : .Pln_Rmp = rd / 10
                        Case 10 : .Pln_Gap = rd / 10
                        Case 11 : .Pln_smpT = rd / 10
                        Case 12 : .Chmb_LmtH = rd / 10
                        Case 13 : .Chmb_LmtL = rd / 10
                        Case 14 : .Chmb_Rmp = rd / 10
                        Case 15 : .Chmb_smpT = rd / 1
                    End Select
                Next
                Label125.Text = Format(.Tmp_setval, "0")
                Label133.Text = Format(.Tmp_tolrnc, "0")

                Label3.Text = Format(.Pow_LmtHp, "0")
                Label7.Text = Format(.Pow_LmtLm, "0")
                Label13.Text = Format(.Pow_Rmp, "0")
                Label15.Text = Format(.Pow_smpT, "0")

                Label36.Text = Format(.Pln_LmtH, "0.0")
                Label40.Text = Format(.Pln_LmtL, "0.0")
                Label31.Text = Format(.Pln_Rmp, "0.0")
                Label25.Text = Format(.Pln_Gap, "0.0")
                Label32.Text = Format(.Pln_smpT, "0.0")

                Label52.Text = Format(.Chmb_LmtH, "0.0")
                Label55.Text = Format(.Chmb_LmtL, "0.0")
                Label47.Text = Format(.Chmb_Rmp, "0.0")
                Label48.Text = Format(.Chmb_smpT, "0")
            End With
            RdComp(2) = False

        End If

        'Pres, MW, Plenのmeasuredの値を追加する 2022/10/27 Kajino
        If RdComp(3) = True Then
            With Mes
                'For i = 1 To 4
                For i = 1 To 7
                    rd = AxDBDeviceManager4.Devices(i).ValueRead


                    Select Case i
                        Case 1 : .SetVal = rd / 1
                        Case 2 : .PowRefVal = rd / 1
                        Case 3 : .PowLmtH = rd / 1
                        Case 4 : .PowLmtL = rd / 1
                            '追加
                        Case 5 : .Pres = rd / 10
                        Case 6 : .MW = rd / 10
                        Case 7 : .Plen = rd / 10
                    End Select
                Next
                Label126.Text = Format(.SetVal, "0")

                Label11.Text = Format(.PowRefVal, "0")
                Label17.Text = Format(.PowLmtH, "0")
                Label18.Text = Format(.PowLmtL, "0")

            End With
            RdComp(3) = False
        End If

        '------------ ADD ICON indicators   ----  2022/08/08
        AtcIconNumb_MW = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1640"))  'MW ATC icon code number
        AtcIconNumb_PL = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1650"))  'Plenum ATC icon code number
        AtcIconNumb_CH = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, "1660"))  'Chamb. press. ATC icon code number

        '------------ ICON indicators 移動  ----  2022/08/12
        Select Case AtcIconNumb_MW
            Case 0
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Waiting
                Label187.Text = "Waiting"
                GroupBox1.Text = "POWER : " & Label187.Text

            Case 1
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Up
                Label187.Text = "Increase"
                GroupBox1.Text = "POWER : " & Label187.Text

            Case 2
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Down
                Label187.Text = "Decrease"
                GroupBox1.Text = "POWER : " & Label187.Text

            Case 3
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Top
                Label187.Text = "High Limit"
                GroupBox1.Text = "POWER : " & Label187.Text

            Case 4
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Bottom
                Label187.Text = "Low Limit"
                GroupBox1.Text = "POWER : " & Label187.Text

            Case 5
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Gap
                Label187.Text = "Gap Limit"
                GroupBox1.Text = "POWER : " & Label187.Text

            Case 6
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Warning
                Label187.Text = "Out of Range"
                GroupBox1.Text = "POWER : " & Label187.Text

            Case 7
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Alarm
                Label187.Text = "Out of Control"
                GroupBox1.Text = "POWER : " & Label187.Text

            Case 8
                PictureBox1.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Standby
                Label187.Text = "Standby"
                GroupBox1.Text = "POWER : " & Label187.Text

        End Select

        Select Case AtcIconNumb_PL
            Case 0
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Waiting
                Label182.Text = "Waiting"
                GroupBox2.Text = "PLEN : " & Label182.Text
            Case 1
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Up
                Label182.Text = "Increase"
                GroupBox2.Text = "PLEN : " & Label182.Text

            Case 2
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Down
                Label182.Text = "Decrease"
                GroupBox2.Text = "PLEN : " & Label182.Text

            Case 3
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Top
                Label182.Text = "High Limit"
                GroupBox2.Text = "PLEN : " & Label182.Text

            Case 4
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Bottom
                Label182.Text = "Low Limit"
                GroupBox2.Text = "PLEN : " & Label182.Text

            Case 5
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Gap
                Label182.Text = "Gap Limit"
                GroupBox2.Text = "PLEN : " & Label182.Text

            Case 6
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Warning
                Label182.Text = "Out of Range"
                GroupBox2.Text = "PLEN : " & Label182.Text

            Case 7
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Alarm
                Label182.Text = "Out of Control"
                GroupBox2.Text = "PLEN : " & Label182.Text

            Case 8
                PictureBox2.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Standby
                Label182.Text = "Standby"
                GroupBox2.Text = "PLEN : " & Label182.Text

        End Select

        Select Case AtcIconNumb_CH
            Case 0
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Waiting
                Label191.Text = "Waiting"
                GroupBox3.Text = "CHMB : " & Label191.Text

            Case 1
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Up
                Label191.Text = "Increase"
                GroupBox3.Text = "CHMB : " & Label191.Text

            Case 2
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Down
                Label191.Text = "Decrease"
                GroupBox3.Text = "CHMB : " & Label191.Text

            Case 3
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Top
                Label191.Text = "High Limit"
                GroupBox3.Text = "CHMB : " & Label191.Text

            Case 4
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Bottom
                Label191.Text = "Low Limit"
                GroupBox3.Text = "CHMB : " & Label191.Text

            Case 5
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Gap
                Label191.Text = "Gap Limit"
                GroupBox3.Text = "CHMB : " & Label191.Text

            Case 6
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Warning
                Label191.Text = "Out of Range"
                GroupBox3.Text = "CHMB : " & Label191.Text

            Case 7
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Alarm
                Label191.Text = "Out of Control"
                GroupBox3.Text = "CHMB : " & Label191.Text

            Case 8
                PictureBox3.Image = SDS6KCTL.My.Resources.Resources.Old_Icon_Standby
                Label191.Text = "Standby"
                GroupBox3.Text = "CHMB : " & Label191.Text
        End Select


    End Sub

    Private Sub ChksetVal_PLCwrite(ByVal keychr As String, ByVal txtbox As TextBox, ByRef setval As Single,
                                   ByVal upprlmt As Single, ByVal lowerlmt As Single, ByVal devnum As String)

        'Check setvalue and value write to PLC
        Dim rd As Single
        Dim bfrVal As Single
        Dim alm As Boolean = False
        Dim msg As String
        'Dim lmtok As Boolean = False '22/10/28 Delete kajino
        Dim atcbusy As Boolean      '22/10/28 Add ATC busy flag Kajino

        If keychr = vbCr Then
            If IsNumeric(txtbox.Text) = True Then
                '2022/10/25 RAMP items also allow 0 to be entered. Kajino 
                'If CSng(txtbox.Text) > upprlmt Then
                '    setval = upprlmt
                'ElseIf CSng(txtbox.Text) < lowerlmt Then
                '    setval = lowerlmt
                'Else
                '    setval = CSng(txtbox.Text)
                'End If

                If CSng(txtbox.Text) > upprlmt Then
                    setval = upprlmt
                ElseIf CSng(txtbox.Text) < lowerlmt Then
                    If txtbox Is TextBox4 Or txtbox Is TextBox6 Or txtbox Is TextBox12 Then  'Each RAMP
                        If CSng(txtbox.Text) = 0 Then
                            setval = 0
                        Else
                            setval = lowerlmt
                        End If
                    Else
                        setval = lowerlmt
                    End If
                Else
                    setval = CSng(txtbox.Text)
                End If


                rd = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devnum))
                If txtbox Is TextBox8 Or txtbox Is TextBox7 Or txtbox Is TextBox6 Or txtbox Is TextBox9 Or
                    txtbox Is TextBox14 Or txtbox Is TextBox13 Or txtbox Is TextBox12 Or txtbox Is TextBox5 Then

                    bfrVal = rd / 10
                Else
                    bfrVal = rd
                End If

                '2022/10/28 不要のため削除 Kajino
                'If txtbox Is TextBox2 Then  'POWER LIMIT H(+)
                '    If bfrVal = CSng(Label3.Text) Then lmtok = True
                'End If

                'If txtbox Is TextBox1 Then  'POWER LIMIT L(-)
                '    If bfrVal = CSng(Label7.Text) Then lmtok = True
                'End If

                'If txtbox Is TextBox8 Then  'PLEN LIMIT H
                '    If bfrVal = CSng(Label36.Text) Then lmtok = True
                'End If

                'If txtbox Is TextBox7 Then  'PLEN LIMIT L
                '    If bfrVal = CSng(Label40.Text) Then lmtok = True
                'End If

                'If txtbox Is TextBox14 Then 'CHMB LIMIT H
                '    If bfrVal = CSng(Label52.Text) Then lmtok = True
                'End If

                'If txtbox Is TextBox13 Then 'CHMB LIMIT L
                '    If bfrVal = CSng(Label55.Text) Then lmtok = True
                'End If

                'If txtbox Is TextBox9 Then 'GAP
                '    If bfrVal = CSng(Label25.Text) Then lmtok = True
                'End If

                '2022/10/27 
                'If txtbox Is TextBox2 Or txtbox Is TextBox8 Or txtbox Is TextBox14 Then
                '    'H Limit
                '    If bfrVal > setval Then
                '        alm = True
                '    End If
                'ElseIf txtbox Is TextBox1 Or txtbox Is TextBox7 Or txtbox Is TextBox13 Then
                '    'L Limit
                '    If bfrVal < setval Then
                '        alm = True
                '    End If
                'End If

                atcbusy = AxDBDeviceManager1.Devices(5).ValueRead
                If atcbusy Then
                    If txtbox Is TextBox2 And (Mes.PowRefVal + setval < Mes.MW) Then
                        'MW H Limit
                        alm = True
                    ElseIf txtbox Is TextBox8 And (setval < Mes.Plen) Then
                        'Plen H Limit
                        alm = True
                    ElseIf txtbox Is TextBox14 And (setval < Mes.Pres) Then
                        'Pres H Limit
                        alm = True
                    ElseIf txtbox Is TextBox1 And (Mes.PowRefVal - setval > Mes.MW) Then
                        'MW L Limit
                        alm = True
                    ElseIf txtbox Is TextBox7 And (setval > Mes.Plen) Then
                        'Plen L Limit
                        alm = True
                    ElseIf txtbox Is TextBox13 And (setval > Mes.Pres) Then
                        'Pres L Limit
                        alm = True
                    ElseIf txtbox Is TextBox9 And (Mes.Pres - setval < Mes.Plen) Then
                        'GAP
                        alm = True
                    End If
                End If


                '2022/10/28 不要のため削除 Kajino
                'If alm = True And lmtok = True Then
                If alm = True Then
                    msg = "The set point will be changed instantly." & vbCr & "Do you want to rewrite the set value?"
                    If MessageBox.Show(msg, "Confirm", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation) = DialogResult.OK Then
                        If txtbox Is TextBox8 Or txtbox Is TextBox7 Or txtbox Is TextBox6 Or txtbox Is TextBox9 Or
                            txtbox Is TextBox14 Or txtbox Is TextBox13 Or txtbox Is TextBox12 Or txtbox Is TextBox5 Then

                            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, devnum, setval * 10)
                        Else
                            AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, devnum, setval)
                        End If
                        MessageBox.Show("Set point has been changed.")
                    End If

                Else

                    If txtbox Is TextBox8 Or txtbox Is TextBox7 Or txtbox Is TextBox6 Or txtbox Is TextBox9 Or
                         txtbox Is TextBox14 Or txtbox Is TextBox13 Or txtbox Is TextBox12 Or txtbox Is TextBox5 Then

                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, devnum, setval * 10)
                    Else
                        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM, devnum, setval)
                    End If
                End If

                txtbox.BackColor = SystemColors.ControlLight
                rd = CSng(AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.KV5000_DM, devnum))
                If txtbox Is TextBox8 Or txtbox Is TextBox7 Or txtbox Is TextBox6 Or txtbox Is TextBox9 Or
                txtbox Is TextBox14 Or txtbox Is TextBox13 Or txtbox Is TextBox12 Or txtbox Is TextBox5 Then

                    txtbox.Text = Format(rd / 10, "0.0")

                Else
                    txtbox.Text = Format(rd, "0")
                End If
            End If
        End If

    End Sub

    Private Sub TextBox25_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox25.KeyDown
        'TEMP TOLERANCE setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox25.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox25_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox25.KeyPress
        'TEMP TOLERANCE setpoint Write to PLC
        '入力制限反映 1～975が正 2022/10/24　
        Dim upperlmt As Single = 975
        Dim lowerlmt As Single = 1
        ChksetVal_PLCwrite(e.KeyChar, TextBox25, TmpCtl_set.Tmp_tolrnc, upperlmt, lowerlmt, "1502")
    End Sub

    Private Sub TextBox21_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox21.KeyDown
        'TEMP SET VALUE setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox21.BackColor = Color.Yellow
        End If

    End Sub

    Private Sub TextBox21_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox21.KeyPress
        'TEMP SET VALUE setpoint Write to PLC
        '入力制限反映 475～1450が正 2022/10/24　
        '入力制限反映 477～1473が正 2022/12/13 将来的には、メンテナンス画面のパイロレンジ±2から算出する　
        Dim upperlmt As Single = 1473
        Dim lowerlmt As Single = 477
        ChksetVal_PLCwrite(e.KeyChar, TextBox21, TmpCtl_set.Tmp_setval, upperlmt, lowerlmt, "1500")
    End Sub

    Private Sub TextBox2_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox2.KeyDown
        'POWER LMIT H(+) setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox2.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox2_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox2.KeyPress
        'POWER LMIT H(+) setpoint Write to PLC
        '入力制限反映 10～5400が正 2022/10/24　
        Dim upperlmt As Single = 5400
        Dim lowerlmt As Single = 10
        ChksetVal_PLCwrite(e.KeyChar, TextBox2, TmpCtl_set.Pow_LmtHp, upperlmt, lowerlmt, "1504")
    End Sub

    Private Sub TextBox1_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox1.KeyDown
        'POWER LMIT L(-) setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox1.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox1.KeyPress
        'POWER LMIT L(-) setpoint Write to PLC
        '入力制限反映 10～5400が正 2022/10/24　
        Dim upperlmt As Single = 5400
        Dim lowerlmt As Single = 10
        ChksetVal_PLCwrite(e.KeyChar, TextBox1, TmpCtl_set.Pow_LmtLm, upperlmt, lowerlmt, "1506")
    End Sub

    Private Sub TextBox4_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox4.KeyDown
        'POWER RAMP setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox4.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox4_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox4.KeyPress
        'POWER RAMP setpoint Write to PLC　
        Dim upperlmt As Single = 100
        Dim lowerlmt As Single = 10
        ChksetVal_PLCwrite(e.KeyChar, TextBox4, TmpCtl_set.Pow_Rmp, upperlmt, lowerlmt, "1508")
    End Sub

    Private Sub TextBox3_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox3.KeyDown
        'POWER SAMPLING TIME setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox3.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox3_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox3.KeyPress
        'POWER  SAMPLING TIME setpoint Write to PLC
        Dim upperlmt As Single = 3600
        Dim lowerlmt As Single = 1
        ChksetVal_PLCwrite(e.KeyChar, TextBox3, TmpCtl_set.Pow_smpT, upperlmt, lowerlmt, "1510")
    End Sub

    Private Sub TextBox8_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox8.KeyDown
        'PLEN LIMIT H setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox8.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox8_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox8.KeyPress
        'PLEN LIMIT H setpoint Write to PLC
        '2022/10/25 Fix GUI input restrictions Kajino
        'Dim upperlmt As Single = 220
        'Dim lowerlmt As Single = 10
        Dim upperlmt As Single = CSng(Label52.Text)
        Dim lowerlmt As Single = CSng(Label40.Text) + 0.1
        ChksetVal_PLCwrite(e.KeyChar, TextBox8, TmpCtl_set.Pln_LmtH, upperlmt, lowerlmt, "1512")
    End Sub

    Private Sub TextBox7_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox7.KeyDown
        'PLEN LIMIT L setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox7.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox7_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox7.KeyPress
        'PLEN LIMIT L setpoint Write to PLC
        '2022/10/25 Fix GUI input restrictions Kajino
        'Dim upperlmt As Single = 220
        'Dim lowerlmt As Single = 10
        Dim upperlmt As Single = CSng(Label36.Text) - 0.1
        Dim lowerlmt As Single = 5 'PLEN最低値10Torr→5Torrに変更 23/02/24
        ChksetVal_PLCwrite(e.KeyChar, TextBox7, TmpCtl_set.Pln_LmtL, upperlmt, lowerlmt, "1514")
    End Sub

    Private Sub TextBox6_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox6.KeyDown
        'PLEN RAMP setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox6.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox6_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox6.KeyPress
        'PLEN RAMP setpoint Write to PLC
        '入力制限反映 0.1～10が正 2022/10/24　
        Dim upperlmt As Single = 10
        Dim lowerlmt As Single = 0.1
        ChksetVal_PLCwrite(e.KeyChar, TextBox6, TmpCtl_set.Pln_Rmp, upperlmt, lowerlmt, "1516")
    End Sub

    Private Sub TextBox9_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox9.KeyDown
        'PLEN GAP setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox9.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox9_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox9.KeyPress
        'PLEN GAP setpoint Write to PLC
        Dim upperlmt As Single = 220
        Dim lowerlmt As Single = 0.1
        ChksetVal_PLCwrite(e.KeyChar, TextBox9, TmpCtl_set.Pln_Gap, upperlmt, lowerlmt, "1518")
    End Sub

    Private Sub TextBox5_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox5.KeyDown
        'PLEN SAMPLING TIME setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox5.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox5_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox5.KeyPress
        'PLEN SAMPLING TIME setpoint Write to PLC
        Dim upperlmt As Single = 3600
        Dim lowerlmt As Single = 0.5
        ChksetVal_PLCwrite(e.KeyChar, TextBox5, TmpCtl_set.Pln_smpT, upperlmt, lowerlmt, "1520")
    End Sub

    Private Sub TextBox14_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox14.KeyDown
        'CHMB LIMIT H setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox14.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox14_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox14.KeyPress
        'CHMB LIMIT H setpoint Write to PLC
        '入力制限反映 10.1～220が正 2022/10/24　
        '2022/10/25 Fix GUI input restrictions Kajino
        'Dim upperlmt As Single = 220
        'Dim lowerlmt As Single = 10
        Dim upperlmt As Single = 220
        Dim lowerlmt As Single = CSng(Label55.Text) + 0.1
        ChksetVal_PLCwrite(e.KeyChar, TextBox14, TmpCtl_set.Chmb_LmtH, upperlmt, lowerlmt, "1530")
    End Sub

    Private Sub TextBox13_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox13.KeyDown
        'CHMB LIMIT L setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox13.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox13_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox13.KeyPress
        'CHMB LIMIT L setpoint Write to PLC
        '2022/10/25 Fix GUI input restrictions Kajino
        'Dim upperlmt As Single = 220
        'Dim lowerlmt As Single = 60
        Dim upperlmt As Single = CSng(Label52.Text) - 0.1
        Dim lowerlmt As Single = 60
        ChksetVal_PLCwrite(e.KeyChar, TextBox13, TmpCtl_set.Chmb_LmtL, upperlmt, lowerlmt, "1532")
    End Sub

    Private Sub TextBox12_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox12.KeyDown
        'CHMB RAMP setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox12.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox12_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox12.KeyPress
        'CHMB RAMP setpoint Write to PLC
        '入力制限反映 0～10が正 2022/10/24　
        Dim upperlmt As Single = 10
        Dim lowerlmt As Single = 0.1
        ChksetVal_PLCwrite(e.KeyChar, TextBox12, TmpCtl_set.Chmb_Rmp, upperlmt, lowerlmt, "1534")
    End Sub

    Private Sub TextBox11_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox11.KeyDown
        'CHMB SAMPLING TIME setpoint
        If e.KeyData <> Keys.Enter Then
            TextBox11.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBox11_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox11.KeyPress
        'CHMB SAMPLING TIME setpoint Write to PLC
        Dim upperlmt As Single = 3600
        Dim lowerlmt As Single = 1
        ChksetVal_PLCwrite(e.KeyChar, TextBox11, TmpCtl_set.Chmb_smpT, upperlmt, lowerlmt, "1536")
    End Sub

    Private Sub Button32_Click(sender As Object, e As EventArgs) Handles Button32.Click
        'START
        AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, "1205", 1)
        Me.ActiveControl = Nothing
    End Sub

    Private Sub frmTmpCTL_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        If AxDBCommManager1.ReadDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV8K_RLY_B, "5300") = 1 Then
            e.Cancel = True
        End If
    End Sub
End Class