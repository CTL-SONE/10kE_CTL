Public Class frmHLP
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'Close
        Try
            frmALRM.ListBox1.SetSelected(0, False)
        Catch ex As Exception

        End Try
        Try
            frmALRM.ListBox2.SetSelected(0, False)
        Catch ex As Exception

        End Try

        Me.Close()

    End Sub

End Class