Public Class frmALRM

    '**************************************************************
    '              SDS6K-10kEALARM Ver1.00
    '                     2023/02/24   
    '**************************************************************
    'Ver1.00 20231005 ベースはV4.02 
    'FS4～6、オートチューナの故障信号追加に伴い予備の故障信号追加 MR21610 ~ M21915

    Structure AlarmTxt  '*Append 2021/07/21
        Dim Indx() As String
        Dim SDSNo() As String
        Dim Msg() As String
        Dim Sttus() As String
        Dim PsblCuss() As String
        Dim Rctfc() As String
    End Structure


    Const PATH As String = "C:\SDS6NETSYS\"
    Const PARAMFILE_NAM As String = "ALM.LOG"

    Public AlmTxt As AlarmTxt

    Dim RdComp As Boolean
    Dim WrComp As Boolean
    Dim Fsize As Boolean

    '*Append 2021/05/27
    Dim Mem(320) As Boolean '故障信号追加に伴い256→320に変更 23/02/24
    Dim Itmidx(320) As Integer '故障信号追加に伴い256→320に変更 23/02/24
    '*Append 2021/07/19
    Dim Mem2(320) As Boolean '故障信号追加に伴い256→320に変更 23/02/24
    Dim Itmidx2(320) As Integer '故障信号追加に伴い256→320に変更 23/02/24

    'Private Sub frmALRM_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
    '    If MessageBox.Show("Are you sure to exit?", "SDS6 ALARM", MessageBoxButtons.OKCancel) = Windows.Forms.DialogResult.OK Then
    '        e.Cancel = False
    '        'AxDBTriggerManager1.Active = False
    '        'AxDBTriggerManager2.Active = False
    '        'AxDBCommManager1.Disconnect()
    '    Else
    '        e.Cancel = True
    '    End If

    'End Sub


    Private Sub frmALRM_Load(sender As Object, e As System.EventArgs) Handles Me.Load
        '//////// Initialize ///////

        'Forbid Multi Execution
        If Diagnostics.Process.GetProcessesByName(Diagnostics.Process.GetCurrentProcess.ProcessName).Length > 1 Then
            Application.Exit()
        End If

        ReDim AlmTxt.Indx(400)
        ReDim AlmTxt.SDSNo(400)
        ReDim AlmTxt.Msg(400)
        ReDim AlmTxt.Sttus(400)
        ReDim AlmTxt.PsblCuss(400)
        ReDim AlmTxt.Rctfc(400)

        AlmLst_read()
        AlarmLog_Read()  'Disp Alarm History

        '-------- Rev.2022/02
        'IP and Version of PLC are stored in HKEY_CURRENT_USER\SoftWare\SDS\PLC
        AxDBCommManager1.Peer = RgsPlcIp() & ":8500"    'default 192.168.3.13 & port 8500
        AxDBCommManager1.PLC = CInt(RgsPlcSr())         'default 515 = KV-5500/5000/3000

        Try
            AxDBCommManager1.Connect()
            AxDBTriggerManager1.Active = False  'No USE
            AxDBTriggerManager2.Active = True
            AxDBTriggerManager3.Active = True
        Catch ex As Exception
            MessageBox.Show(ex.Message, "COM+")
            Exit Sub
        End Try


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

    Private Sub AlmLst_read()
        'Get Alarm message from "Alarm List.txt"
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim k As Integer = 0
        Dim fnam As String = PATH & "Alarm List.txt"
        Dim fno As Integer = FreeFile()
        Dim s() As String
        Dim sc() As String
        Dim ftxt As String

        ftxt = FileIO.FileSystem.ReadAllText(fnam)
        s = Strings.Split(ftxt, vbCrLf)
        k = s.Length - 1
        For i = 0 To k
            sc = Strings.Split(s(i), vbTab)
            Dim m As Integer = sc.Length
            If IsNumeric(sc(0)) = True Then
                j = CInt(sc(0))
                AlmTxt.Indx(j) = sc(0)
                AlmTxt.SDSNo(j) = sc(1)
                AlmTxt.Msg(j) = sc(2)
                AlmTxt.Sttus(j) = sc(3)
                AlmTxt.PsblCuss(j) = sc(4)
                AlmTxt.Rctfc(j) = sc(5)
            End If
        Next

    End Sub

    Private Sub AxDBTriggerManager1_Fire(sender As Object, e As AxDATABUILDERAXLibLB._IDBTriggerEvents_FireEvent) Handles AxDBTriggerManager1.Fire
        ''AlARM MESSAGE & CREATE ALMLOG
        'Dim s As String
        'Dim almtim As String
        'Dim almtxt As String
        'Dim i As Integer

        'Fsize = True : Me.WindowState = 0 : Me.Visible = True

        'With e.pTrigger
        '    s = .Description
        '    i = Val(Mid(s, 2, 3))   'Coment chr last 3(ex. 001)
        '    almtim = DateString & " " & TimeString
        '    almtxt = almtim & "> " & Alm_msg(i) & vbCrLf

        '    Dim fnam As String = PATH & PARAMFILE_NAM
        '    Dim fno As Integer = FreeFile()

        '    Try
        '        FileOpen(fno, fnam, OpenMode.Append)
        '        WriteLine(fno, almtim, Alm_msg(i))
        '        FileClose()
        '    Catch ex As Exception
        '        MessageBox.Show(ex.Message, "ALARM LOG")
        '    End Try

        '    TextBox1.Text = TextBox1.Text & almtxt
        '    TextBox2.Text = TextBox2.Text & almtxt
        '    'TextBox2.SelectionStart = Len(TextBox2)   '// Auto display the last line
        'End With

    End Sub

    Private Sub AxDBDeviceManager3_AfterRead(sender As Object, e As System.EventArgs) Handles AxDBDeviceManager3.AfterRead
        'AlARM MESSAGE & CREATE ALMLOG
        Dim i As Integer
        Dim alrmtim As String
        Dim alrmhead As String
        Dim alrmtxt As String
        Dim dvnum As String
        Dim dvc As DATABUILDERAXLibLB.DBDevice
        Dim idx As String

        For i = 1 To 320 '故障信号追加に伴い256→320に変更 23/02/24
            If AxDBDeviceManager3.Devices(i).ValueRead = 1 Then
                If Mem(i) = False Then
                    Mem(i) = True
                    dvc = AxDBDeviceManager3.Devices.Item(i)
                    dvnum = dvc.No
                    alrmtim = DateString & " " & TimeString
                    idx = Format(i, "000")
                    alrmhead = alrmtim & "> " & idx & " " & AlmTxt.SDSNo(i) & ":"
                    alrmtxt = alrmhead & AlmTxt.Msg(i) & vbCrLf

                    Dim fnam As String = PATH & PARAMFILE_NAM
                    Dim fno As Integer = FreeFile()

                    Try
                        FileOpen(fno, fnam, OpenMode.Append)
                        Print(fno, alrmtxt)
                        FileClose()
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, "ALARM LOG")
                    End Try

                    ListBox1.Items.Add(alrmtxt)  'Append 2021/05/08
                    Itmidx(i) = ListBox1.Items.Count

                    AlarmLog_Read()
                    'ListBox2.Items.Add(alrmtxt)
                    Itmidx2(i) = ListBox2.Items.Count
                End If
                'Else
                '    Mem(i) = False
            End If
        Next
        RdComp = True

    End Sub


    Private Sub AxDBTriggerManager2_Fire(sender As Object, e As AxDATABUILDERAXLibLB._IDBTriggerEvents_FireEvent) Handles AxDBTriggerManager2.Fire
        'ALARM RESET
        Dim device(320) As DATABUILDERAXLibLB.DBDevice '故障信号追加に伴い256→320に変更 23/02/24
        Dim rdval As Long
        Dim i As Integer
        Static j As Integer = 0
        Dim s As String
        Dim curitmidx As Integer
        '----------------------------------------
        '  Index of ListBoxItems: 0 to xxx
        '  Itmidx(): 1 to xxx
        '----------------------------------------

        With e.pTrigger
            s = .Description
            If s = "ALM_RST" Then
                Me.Visible = True
                RdComp = False
                AxDBDeviceManager3.ReadAll()
                Do
                    My.Application.DoEvents()
                Loop Until RdComp = True
                For i = 1 To 320 '故障信号追加に伴い256→320に変更 23/02/24
                    If Mem(i) = True Then
                        device(i) = AxDBDeviceManager3.Devices.Item(i)
                        rdval = device(i).ValueRead
                        If rdval = 0 Then
                            curitmidx = Itmidx(i)
                            ListBox1.Items.RemoveAt(Itmidx(i) - 1)  '  Itmidx(i): 1 ～
                            Itmidx(i) = 0

                            For j = 1 To 320 '故障信号追加に伴い256→320に変更 23/02/24
                                If Itmidx(j) > curitmidx Then
                                    Itmidx(j) = Itmidx(j) - 1
                                End If
                            Next

                            Mem(i) = False
                        End If
                    End If
                Next
            Else
                Me.Visible = True
            End If
        End With
    End Sub

    Private Sub AlarmLog_Read()
        'Read alarm data and display of alarm history
        Dim alrmtxt As String = ""
        Dim fnam As String = PATH & PARAMFILE_NAM
        Dim fno As Integer = FreeFile()
        Dim i As Long = 0
        Dim k As Long = 0
        Try
            FileOpen(fno, fnam, OpenMode.Input)
            Do Until EOF(fno)
                'Input(fno, alrmtxt)
                alrmtxt = LineInput(fno)
                k = k + 1
            Loop
            FileClose()

            FileOpen(fno, fnam, OpenMode.Input)
            Dim almmsg(k) As String
            k = k - 1
            For i = 0 To k
                almmsg(i) = LineInput(fno)
            Next
            FileClose()

            ListBox2.Items.Clear()
            For i = k To 0 Step -1
                'disp history
                ListBox2.Items.Add(almmsg(i))
            Next

        Catch ex As Exception
            MessageBox.Show(ex.Message, "ALARM LOG READ")
            FileClose()
        End Try

    End Sub


    Private Sub ListBox1_MouseClick(sender As Object, e As MouseEventArgs) Handles ListBox1.MouseClick
        Dim s As String
        Dim i As Integer

        i = ListBox1.SelectedIndex
        s = ListBox1.SelectedItem
        '"s=2021-05-10 15:26:32>_001_Co_001"
        If i > -1 Then
            Try
                i = Convert.ToInt16(Mid(s, 22, 3))
                frmHLP.Label5.Text = AlmTxt.SDSNo(i) & AlmTxt.Msg(i)
                frmHLP.TextBox1.Text = AlmTxt.Sttus(i)
                frmHLP.TextBox2.Text = AlmTxt.PsblCuss(i)
                frmHLP.TextBox3.Text = AlmTxt.Rctfc(i)
                frmHLP.Show()
            Catch ex As Exception

            End Try

        End If
    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click
        'To Debug

        'Dim almtim As String
        'almtim = DateString & " " & TimeString
        'TextBox2.Text = almtim
    End Sub

    Private Sub ListBox2_MouseClick(sender As Object, e As MouseEventArgs) Handles ListBox2.MouseClick
        Dim s As String
        Dim i As Integer

        i = ListBox2.SelectedIndex
        s = ListBox2.SelectedItem
        '"s=2021-05-10 15:26:32>_001_Co_001"
        If i > -1 Then
            Try
                i = Convert.ToInt16(Mid(s, 22, 3))
                frmHLP.Label5.Text = AlmTxt.SDSNo(i) & AlmTxt.Msg(i)
                frmHLP.TextBox1.Text = AlmTxt.Sttus(i)
                frmHLP.TextBox2.Text = AlmTxt.PsblCuss(i)
                frmHLP.TextBox3.Text = AlmTxt.Rctfc(i)
                frmHLP.Show()
            Catch ex As Exception

            End Try

        End If
    End Sub

End Class
