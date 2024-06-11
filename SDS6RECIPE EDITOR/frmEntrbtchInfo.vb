Public Class frmEntrbtchInfo
    Const SUPRVISR As Integer = 1
    Const OPRATR As Integer = 0
    Const UC As String = "CTL1861$"
    Const UA As String = "Admin"
    Const PC As String = "SEKI1418"

    Public UsrName As String
    Public PassWord As String

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'Cancel
        Me.Close()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'OK
        Dim sha256 As System.Security.Cryptography.SHA256 = New System.Security.Cryptography.SHA256CryptoServiceProvider

        UsrName = TextBox1.Text
        PassWord = TextBox2.Text

        If UsrName = UC Then
            If PassWord = PC Then
                With frmControl
                    .LoginMode = SUPRVISR
                    .CtlVsble_Operation(.LoginMode)
                End With
            End If
        ElseIf UsrName = UA Then
            With frmControl
                .LoginMode = SUPRVISR
                .CtlVsble_Operation(.LoginMode)
            End With
        End If

        Me.Close()

    End Sub
End Class