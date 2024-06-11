Public Class frmEntrbtchInfo
    Const SUPRVISR As Integer = 1
    Const OPRATR As Integer = 0
    Const UC As String = "CTL1861$"
    Const UA As String = "Admin"
    Const PC As String = "SEKI1418"

    Public UsrName As String
    Public PassWord As String

    Public OprtrName As String
    Public LotNumbr As String
    Public BatchInfo_OK As Boolean = False


    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'Cancel
        BatchInfo_OK = False
        Me.Close()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'OK

        OprtrName = TextBox1.Text
        LotNumbr = TextBox2.Text

        If OprtrName <> "" And LotNumbr <> "" Then
            If Len(OprtrName) > 32 Then
                MessageBox.Show("Operator name too long! L=<32")
                Exit Sub
            End If
            If Len(LotNumbr) > 32 Then
                MessageBox.Show("Lot number too long! L=<32")
                Exit Sub
            End If
            WrBtchInfo_PLC()
            BatchInfo_OK = True
            Me.Close()
        Else
            BatchInfo_OK = False
            Me.Close()
        End If

    End Sub

    Private Sub WrBtchInfo_PLC()

        Dim i As Integer = 0
        Dim s As String = ""

        With frmControl
            'Clear PLC regester
            For i = 1 To 116
                .AxDBDeviceManager4.Devices(i).ValueWrite = 0
            Next
            .WrComp = False
            .AxDBDeviceManager4.WriteAll()
            Do
                Application.DoEvents()
            Loop Until .WrComp = True

            'Write input data
            .AxDBCommManager1.WriteText(DBPlcDevice.DKV5000_DM, "40000", 32, OprtrName)
            .AxDBCommManager1.WriteText(DBPlcDevice.DKV5000_DM, "40100", 32, LotNumbr)
            .AxDBCommManager1.ReadText(DBPlcDevice.DKV5000_DM, "40000", 32, s)
            .Label210.Text = s
            .AxDBCommManager1.ReadText(DBPlcDevice.DKV5000_DM, "40100", 32, s)
            .Label211.Text = s
        End With

    End Sub

    Private Sub frmEntrbtchInfo_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        TextBox1.Text = ""
        TextBox2.Text = ""
    End Sub

    Private Sub frmEntrbtchInfo_Load(sender As Object, e As EventArgs) Handles Me.Load
        TextBox1.Text = ""
        SelectNextControl(TextBox1, False, False, False, False)

    End Sub

    Private Sub TextBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox1.KeyPress
        If e.KeyChar = vbCr Then
            TextBox2.Focus()
        End If
    End Sub

    Private Sub TextBox2_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox2.KeyPress
        If e.KeyChar = vbCr Then
            Button2.Focus()
        End If
    End Sub
End Class