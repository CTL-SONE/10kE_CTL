Public Class frmLogIn
    Const SUPRVISR As Integer = 1
    Const OPRATR As Integer = 0
    Const UC As String = "CTL1861"
    Const PC As String = "seki5630"
    Const LGMD_PLC As String = "47000"

    Const UA As String = "supervisor"
    Const PA As String = ""
    Const HASHPAS_PLC As String = "47200"

    Public UsrName As String
    Public PassWord As String
    Dim LoginKey As String = “e9c4efe35e2a90630ccfc6332d827167”

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'Cancel
        With frmControl
            .LoginMode = OPRATR
            .CtlVsble_Operation(.LoginMode)
            .Button39.Visible = False
        End With
        Me.Close()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'OK
        Dim inphash As String = ""
        Dim hashpw As String = ""

        UsrName = TextBox1.Text
        PassWord = TextBox2.Text

        If UsrName = UC And PassWord = PC Then
            'case of 'CORNES'
            With frmControl
                .LoginMode = SUPRVISR
                .CtlVsble_Operation(.LoginMode)
                .Button39.Visible = True
            End With

        ElseIf UsrName = UA Then
            'case of User
            With frmControl
                .AxDBCommManager1.ReadText(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM,
                                             HASHPAS_PLC, 64, hashpw)

                inphash = SHA256_cnv(PassWord).ToString
                If inphash = hashpw Then
                    .LoginMode = SUPRVISR
                    .Button39.Visible = True
                Else
                    .LoginMode = OPRATR
                    .Button39.Visible = False
                End If
                .CtlVsble_Operation(.LoginMode)
            End With
        Else
            With frmControl
                .LoginMode = OPRATR
                .CtlVsble_Operation(.LoginMode)
                .Button39.Visible = False
            End With
        End If

        Me.Close()

    End Sub

    Private Sub frmLogIn_FormClosed(sender As Object, e As FormClosedEventArgs) Handles MyBase.FormClosed

        TextBox1.Text = ""
        TextBox2.Text = ""

    End Sub

    Public Function SHA256_cnv(ByVal pasw As String) As System.Text.StringBuilder
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

    Private Sub frmLogIn_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        TextBox1.Text = "supervisor"
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