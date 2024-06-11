Public Class frmWLalrm
    Const WavLogrAbnormal_Ry As String = "6400" 'R6400

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Me.Close()
    End Sub

    Private Sub frmWLalrm_Closed(sender As Object, e As EventArgs) Handles Me.Closed
        frmControl.AxDBCommManager1.WriteDevice(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_RLY_B, WavLogrAbnormal_Ry, 0)
    End Sub
End Class