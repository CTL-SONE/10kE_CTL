Imports System.Windows.Forms

Public Class WL_warningDialog
    Const WavLogrAbnormal_Ry As String = "6400" 'R6400
    Private Sub OK_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Abort_Button.Click
        Me.DialogResult = System.Windows.Forms.DialogResult.Abort
        Me.Close()
    End Sub

    Private Sub Cancel_Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Ignore_Button.Click
        Me.DialogResult = System.Windows.Forms.DialogResult.Ignore
        Me.Close()
    End Sub

    Private Sub WL_warningDialog_Closed(sender As Object, e As EventArgs) Handles Me.Closed
        frmControl.AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 0)
    End Sub
End Class
