Public Class frmPassWord

    Const PassWord As String = "seki5630"


    Private Sub Button1_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click
        'OK
        If TextBox1.Text = PassWord Then
            frmMainte.Show()
            Me.Hide()
        Else
            TextBox1.Text = ""
            TextBox1.Focus()
        End If
    End Sub

    Private Sub Button2_Click(sender As System.Object, e As System.EventArgs) Handles Button2.Click
        'CANCEL
        End
    End Sub

    
End Class