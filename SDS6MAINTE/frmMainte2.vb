Public Class frmMainte2
    'Control Visible Prameter
    Structure ControlVisible
        Dim Spare_Str As String        'To match the format with the current parameter file "CtlVsPrm.txt" 2023/11/29 Kajino
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
        Dim WavlogDsp As Boolean
    End Structure


    Const PATH As String = "C:\SDS6NETSYS\"
    Const PASWORD_PLC As String = "47100"
    Const HASHPAS_PLC As String = "47200"

    Dim Ctlvsbl As ControlVisible

    Private Sub frmMainte2_Load(sender As Object, e As EventArgs) Handles Me.Load

        TextBox53.Text = ""
        TextBox54.Text = ""
        TextBox53.BackColor = SystemColors.ControlLight
        TextBox54.BackColor = SystemColors.ControlLight
        TextBox54.Enabled = False
        Button2.Enabled = False

        ReadCtlvsblPrm()

        With frmMainte.MwAlm
            Me.TextBox1.Text = .HigWarSP
            Me.TextBox5.Text = .HigWarDT
            Me.TextBox2.Text = .HigAlmSP
            Me.TextBox6.Text = .HigAlmDT

            Me.TextBox3.Text = .LowWarSP
            Me.TextBox7.Text = .LowWarDT
            Me.TextBox4.Text = .LowAlmSP
            Me.TextBox8.Text = .LowAlmDT
        End With

        With frmMainte.WtrAlm_FS1
            Me.TextBox16.Text = Format(.LowFlw_WarSP, "0.0")
            Me.TextBox12.Text = .LowFlw_WarDT
            Me.TextBox15.Text = Format(.LowFlw_AlmSP, "0.0")
            Me.TextBox11.Text = .LowFlw_AlmDT

            Me.TextBox14.Text = Format(.HigTmp_WarSP, "0.0")
            Me.TextBox10.Text = .HigTmp_WarDT
            Me.TextBox13.Text = Format(.HigTmp_AlmSP, "0.0")
            Me.TextBox9.Text = .HigTmp_AlmDT

            Me.TextBox20.Text = Format(.LowTmp_WarSP, "0.0")
            Me.TextBox18.Text = .LowTmp_WarDT
            Me.TextBox19.Text = Format(.LowTmp_AlmSP, "0.0")
            Me.TextBox17.Text = .LowTmp_AlmDT
        End With

        With frmMainte.WtrAlm_FS2
            Me.TextBox32.Text = Format(.LowFlw_WarSP, "0.0")
            Me.TextBox28.Text = .LowFlw_WarDT
            Me.TextBox31.Text = Format(.LowFlw_AlmSP, "0.0")
            Me.TextBox27.Text = .LowFlw_AlmDT

            Me.TextBox30.Text = Format(.HigTmp_WarSP, "0.0")
            Me.TextBox26.Text = .HigTmp_WarDT
            Me.TextBox29.Text = Format(.HigTmp_AlmSP, "0.0")
            Me.TextBox25.Text = .HigTmp_AlmDT

            Me.TextBox24.Text = Format(.LowTmp_WarSP, "0.0")
            Me.TextBox22.Text = .LowTmp_WarDT
            Me.TextBox23.Text = Format(.LowTmp_AlmSP, "0.0")
            Me.TextBox21.Text = .LowTmp_AlmDT
        End With

        With frmMainte.WtrAlm_FS3
            Me.TextBox44.Text = Format(.LowFlw_WarSP, "0.0")
            Me.TextBox40.Text = .LowFlw_WarDT
            Me.TextBox43.Text = Format(.LowFlw_AlmSP, "0.0")
            Me.TextBox39.Text = .LowFlw_AlmDT

            Me.TextBox42.Text = Format(.HigTmp_WarSP, "0.0")
            Me.TextBox38.Text = .HigTmp_WarDT
            Me.TextBox41.Text = Format(.HigTmp_AlmSP, "0.0")
            Me.TextBox37.Text = .HigTmp_AlmDT

            Me.TextBox36.Text = Format(.LowTmp_WarSP, "0.0")
            Me.TextBox34.Text = .LowTmp_WarDT
            Me.TextBox35.Text = Format(.LowTmp_AlmSP, "0.0")
            Me.TextBox33.Text = .LowTmp_AlmDT
        End With

        '23/2/2 FS4,5追加
        With frmMainte.WtrAlm_FS4
            Me.TextBox78.Text = Format(.LowFlw_WarSP, "0.0")
            Me.TextBox72.Text = .LowFlw_WarDT
            Me.TextBox77.Text = Format(.LowFlw_AlmSP, "0.0")
            Me.TextBox71.Text = .LowFlw_AlmDT

            Me.TextBox76.Text = Format(.HigTmp_WarSP, "0.0")
            Me.TextBox70.Text = .HigTmp_WarDT
            Me.TextBox75.Text = Format(.HigTmp_AlmSP, "0.0")
            Me.TextBox69.Text = .HigTmp_AlmDT

            Me.TextBox74.Text = Format(.LowTmp_WarSP, "0.0")
            Me.TextBox68.Text = .LowTmp_WarDT
            Me.TextBox73.Text = Format(.LowTmp_AlmSP, "0.0")
            Me.TextBox67.Text = .LowTmp_AlmDT
        End With

        With frmMainte.WtrAlm_FS5
            Me.TextBox66.Text = Format(.LowFlw_WarSP, "0.0")
            Me.TextBox60.Text = .LowFlw_WarDT
            Me.TextBox65.Text = Format(.LowFlw_AlmSP, "0.0")
            Me.TextBox59.Text = .LowFlw_AlmDT

            Me.TextBox64.Text = Format(.HigTmp_WarSP, "0.0")
            Me.TextBox58.Text = .HigTmp_WarDT
            Me.TextBox63.Text = Format(.HigTmp_AlmSP, "0.0")
            Me.TextBox57.Text = .HigTmp_AlmDT

            Me.TextBox62.Text = Format(.LowTmp_WarSP, "0.0")
            Me.TextBox56.Text = .LowTmp_WarDT
            Me.TextBox61.Text = Format(.LowTmp_AlmSP, "0.0")
            Me.TextBox55.Text = .LowTmp_AlmDT
        End With
        'FS6追加　23/10/04
        With frmMainte.WtrAlm_FS6
            Me.Txb_FS6_LFW_THR.Text = Format(.LowFlw_WarSP, "0.0")
            Me.Txb_FS6_LFW_DT.Text = .LowFlw_WarDT
            Me.Txb_FS6_LFA_THR.Text = Format(.LowFlw_AlmSP, "0.0")
            Me.Txb_FS6_LFA_DT.Text = .LowFlw_AlmDT

            Me.Txb_FS6_HTW_THR.Text = Format(.HigTmp_WarSP, "0.0")
            Me.Txb_FS6_HTW_DT.Text = .HigTmp_WarDT
            Me.Txb_FS6_HTA_THR.Text = Format(.HigTmp_AlmSP, "0.0")
            Me.Txb_FS6_HTA_DT.Text = .HigTmp_AlmDT

            Me.Txb_FS6_LTW_THR.Text = Format(.LowTmp_WarSP, "0.0")
            Me.Txb_FS6_LTW_DT.Text = .LowTmp_WarDT
            Me.Txb_FS6_LTA_THR.Text = Format(.LowTmp_AlmSP, "0.0")
            Me.Txb_FS6_LTA_DT.Text = .LowTmp_AlmDT
        End With

        With frmMainte.PyrAlm
            Me.TextBox52.Text = .HigWarSP
            Me.TextBox48.Text = .HigWarDT
            Me.TextBox51.Text = .HigAlmSP
            Me.TextBox47.Text = .HigAlmDT

            Me.TextBox50.Text = .LowWarSP
            Me.TextBox46.Text = .LowWarDT
            Me.TextBox49.Text = .LowAlmSP
            Me.TextBox45.Text = .LowAlmDT
        End With
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'Visible set
        SaveCtlvsblPrm()

    End Sub

    Private Sub SaveCtlvsblPrm()
        With Ctlvsbl
            .Hold_button = CheckBox1.Checked
            .SysMon_button = CheckBox2.Checked
            .WavLog_button = CheckBox3.Checked
            .Clr_button = CheckBox4.Checked
            .Pyro_panel = CheckBox5.Checked
            .Pump_button = CheckBox6.Checked
            .Roug_button = CheckBox7.Checked
            .Vent_button = CheckBox8.Checked
            .Roug_setP = CheckBox9.Checked
            .PrsChm_button = CheckBox10.Checked
            .PrsChm_setP = CheckBox11.Checked
            .PrsPln_button = CheckBox30.Checked
            .PrsPln_setP = CheckBox31.Checked
            .MwStnby_button = CheckBox12.Checked
            .MwFwd_button = CheckBox13.Checked
            .MwFwd_setP = CheckBox14.Checked
            .MwRef_setP = CheckBox15.Checked
            .GasFlwName_lbl = CheckBox16.Checked
            .GasFlw_button = CheckBox17.Checked
            .GasFlw_lmp = CheckBox18.Checked
            .GasFlw_setP_inAct = CheckBox19.Checked
            .GasFlw_meas = CheckBox20.Checked
            .GasFlw_Devlmp = CheckBox21.Checked
            .TmpCtl_Buuton = CheckBox23.Checked
            .TmpCtl_START = CheckBox22.Checked
            .TmpCtl_SetPoint = CheckBox27.Checked
            .TmpCtl_InAction = CheckBox28.Checked
            .TmpCtl_Measured = CheckBox29.Checked
            .Recp_jmpto = CheckBox32.Checked
            .Recp_Step = CheckBox33.Checked
            .Recp_Pres = CheckBox34.Checked
            .Recp_Plen = CheckBox35.Checked
            .Recp_Temp = CheckBox36.Checked
            .Recp_Power = CheckBox37.Checked
            .Recp_MFC = CheckBox38.Checked
            .WavlogDsp = CheckBox39.Checked
        End With

        'Save Parameters of controls visible
        Dim fnam As String = PATH & "CtlVsPrm.txt"
        Dim fno As Integer = FreeFile()

        Try
            FileOpen(fno, fnam, OpenMode.Output)

            'WriteLine(fno, frmMainte.VerInfo)  'bug fix 2023/11/29 Kajino
            With Ctlvsbl
                WriteLine(fno, .Spare_Str)  'bug fix 2023/11/29 Kajino
                WriteLine(fno, .Hold_button)
                WriteLine(fno, .SysMon_button)
                WriteLine(fno, .WavLog_button)
                WriteLine(fno, .Clr_button)
                WriteLine(fno, .Pyro_panel)
                WriteLine(fno, .Pump_button)
                WriteLine(fno, .Roug_button)
                WriteLine(fno, .Vent_button)
                WriteLine(fno, .Roug_setP)
                WriteLine(fno, .PrsChm_button)
                WriteLine(fno, .PrsChm_setP)
                WriteLine(fno, .PrsPln_button)
                WriteLine(fno, .PrsPln_setP)
                WriteLine(fno, .MwStnby_button)
                WriteLine(fno, .MwFwd_button)
                WriteLine(fno, .MwFwd_setP)
                WriteLine(fno, .MwRef_setP)
                WriteLine(fno, .GasFlwName_lbl)
                WriteLine(fno, .GasFlw_button)
                WriteLine(fno, .GasFlw_lmp)
                WriteLine(fno, .GasFlw_setP_inAct)
                WriteLine(fno, .GasFlw_meas)
                WriteLine(fno, .GasFlw_Devlmp)
                WriteLine(fno, .TmpCtl_Buuton)
                WriteLine(fno, .TmpCtl_START)
                WriteLine(fno, .TmpCtl_SetPoint)
                WriteLine(fno, .TmpCtl_InAction)
                WriteLine(fno, .TmpCtl_Measured)
                WriteLine(fno, .Recp_jmpto)
                WriteLine(fno, .Recp_Step)
                WriteLine(fno, .Recp_Pres)
                WriteLine(fno, .Recp_Plen)
                WriteLine(fno, .Recp_Temp)
                WriteLine(fno, .Recp_Power)
                WriteLine(fno, .Recp_MFC)
                WriteLine(fno, .WavlogDsp)
            End With

            FileClose()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
        End Try
    End Sub

    Private Sub ReadCtlvsblPrm()
        'Read Parameters of controls visible
        Dim fnam As String = PATH & "CtlVsPrm.txt"
        Dim fno As Integer = FreeFile()

        Try
            FileOpen(fno, fnam, OpenMode.Input)

            'Input(fno, frmMainte.VerInfo)  'bug fix 2023/11/29 Kajino
            With Ctlvsbl
                Input(fno, .Spare_Str)      'bug fix 2023/11/29 Kajino
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
                Input(fno, .WavlogDsp)
            End With

            FileClose()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
            FileClose()
        End Try

        With Ctlvsbl
            CheckBox1.Checked = .Hold_button
            CheckBox2.Checked = .SysMon_button
            CheckBox3.Checked = .WavLog_button
            CheckBox4.Checked = .Clr_button
            CheckBox5.Checked = .Pyro_panel
            CheckBox6.Checked = .Pump_button
            CheckBox7.Checked = .Roug_button
            CheckBox8.Checked = .Vent_button
            CheckBox9.Checked = .Roug_setP
            CheckBox10.Checked = .PrsChm_button
            CheckBox11.Checked = .PrsChm_setP
            CheckBox30.Checked = .PrsPln_button
            CheckBox31.Checked = .PrsPln_setP
            CheckBox12.Checked = .MwStnby_button
            CheckBox13.Checked = .MwFwd_button
            CheckBox14.Checked = .MwFwd_setP
            CheckBox15.Checked = .MwRef_setP
            CheckBox16.Checked = .GasFlwName_lbl
            CheckBox17.Checked = .GasFlw_button
            CheckBox18.Checked = .GasFlw_lmp
            CheckBox19.Checked = .GasFlw_setP_inAct
            CheckBox20.Checked = .GasFlw_meas
            CheckBox21.Checked = .GasFlw_Devlmp
            CheckBox23.Checked = .TmpCtl_Buuton
            CheckBox22.Checked = .TmpCtl_START
            CheckBox27.Checked = .TmpCtl_SetPoint
            CheckBox28.Checked = .TmpCtl_InAction
            CheckBox29.Checked = .TmpCtl_Measured
            CheckBox32.Checked = .Recp_jmpto
            CheckBox33.Checked = .Recp_Step
            CheckBox34.Checked = .Recp_Pres
            CheckBox35.Checked = .Recp_Plen
            CheckBox36.Checked = .Recp_Temp
            CheckBox37.Checked = .Recp_Power
            CheckBox38.Checked = .Recp_MFC
            CheckBox39.Checked = .WavlogDsp

        End With
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        'CLOSE BUTTON
        Me.Close()
    End Sub

    Private Sub frmMainte2_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        With frmMainte.MwAlm
            .HigWarSP = CInt(Me.TextBox1.Text)
            .HigWarDT = CLng(Me.TextBox5.Text)
            .HigAlmSP = CInt(Me.TextBox2.Text)
            .HigAlmDT = CLng(Me.TextBox6.Text)

            .LowWarSP = CDbl(Me.TextBox3.Text)
            .LowWarDT = CLng(Me.TextBox7.Text)
            .LowAlmSP = CDbl(Me.TextBox4.Text)
            .LowAlmDT = CLng(Me.TextBox8.Text)
        End With

        With frmMainte.WtrAlm_FS1
            .LowFlw_WarSP = CDbl(Me.TextBox16.Text)
            .LowFlw_WarDT = CLng(Me.TextBox12.Text)
            .LowFlw_AlmSP = CDbl(Me.TextBox15.Text)
            .LowFlw_AlmDT = CLng(Me.TextBox11.Text)

            .HigTmp_WarSP = CDbl(Me.TextBox14.Text)
            .HigTmp_WarDT = CLng(Me.TextBox10.Text)
            .HigTmp_AlmSP = CDbl(Me.TextBox13.Text)
            .HigTmp_AlmDT = CLng(Me.TextBox9.Text)

            .LowTmp_WarSP = CDbl(Me.TextBox20.Text)
            .LowTmp_WarDT = CLng(Me.TextBox18.Text)
            .LowTmp_AlmSP = CDbl(Me.TextBox19.Text)
            .LowTmp_AlmDT = CLng(Me.TextBox17.Text)
        End With

        With frmMainte.WtrAlm_FS2
            .LowFlw_WarSP = CDbl(Me.TextBox32.Text)
            .LowFlw_WarDT = CLng(Me.TextBox28.Text)
            .LowFlw_AlmSP = CDbl(Me.TextBox31.Text)
            .LowFlw_AlmDT = CLng(Me.TextBox27.Text)

            .HigTmp_WarSP = CDbl(Me.TextBox30.Text)
            .HigTmp_WarDT = CLng(Me.TextBox26.Text)
            .HigTmp_AlmSP = CDbl(Me.TextBox29.Text)
            .HigTmp_AlmDT = CLng(Me.TextBox25.Text)

            .LowTmp_WarSP = CDbl(Me.TextBox24.Text)
            .LowTmp_WarDT = CLng(Me.TextBox22.Text)
            .LowTmp_AlmSP = CDbl(Me.TextBox23.Text)
            .LowTmp_AlmDT = CLng(Me.TextBox21.Text)
        End With

        With frmMainte.WtrAlm_FS3
            .LowFlw_WarSP = CDbl(Me.TextBox44.Text)
            .LowFlw_WarDT = CLng(Me.TextBox40.Text)
            .LowFlw_AlmSP = CDbl(Me.TextBox43.Text)
            .LowFlw_AlmDT = CLng(Me.TextBox39.Text)

            .HigTmp_WarSP = CDbl(Me.TextBox42.Text)
            .HigTmp_WarDT = CLng(Me.TextBox38.Text)
            .HigTmp_AlmSP = CDbl(Me.TextBox41.Text)
            .HigTmp_AlmDT = CLng(Me.TextBox37.Text)

            .LowTmp_WarSP = CDbl(Me.TextBox36.Text)
            .LowTmp_WarDT = CLng(Me.TextBox34.Text)
            .LowTmp_AlmSP = CDbl(Me.TextBox35.Text)
            .LowTmp_AlmDT = CLng(Me.TextBox33.Text)
        End With
        '23/2/2 FS4,5追加
        '23/4/27 modify TextBox No Kajino
        With frmMainte.WtrAlm_FS4
            .LowFlw_WarSP = CDbl(Me.TextBox78.Text)
            .LowFlw_WarDT = CLng(Me.TextBox72.Text)
            .LowFlw_AlmSP = CDbl(Me.TextBox77.Text)
            .LowFlw_AlmDT = CLng(Me.TextBox71.Text)

            .HigTmp_WarSP = CDbl(Me.TextBox76.Text)
            .HigTmp_WarDT = CLng(Me.TextBox70.Text)
            .HigTmp_AlmSP = CDbl(Me.TextBox75.Text)
            .HigTmp_AlmDT = CLng(Me.TextBox69.Text)

            .LowTmp_WarSP = CDbl(Me.TextBox74.Text)
            .LowTmp_WarDT = CLng(Me.TextBox68.Text)
            .LowTmp_AlmSP = CDbl(Me.TextBox73.Text)
            .LowTmp_AlmDT = CLng(Me.TextBox67.Text)
        End With

        With frmMainte.WtrAlm_FS5
            .LowFlw_WarSP = CDbl(Me.TextBox66.Text)
            .LowFlw_WarDT = CLng(Me.TextBox60.Text)
            .LowFlw_AlmSP = CDbl(Me.TextBox65.Text)
            .LowFlw_AlmDT = CLng(Me.TextBox59.Text)

            .HigTmp_WarSP = CDbl(Me.TextBox64.Text)
            .HigTmp_WarDT = CLng(Me.TextBox58.Text)
            .HigTmp_AlmSP = CDbl(Me.TextBox63.Text)
            .HigTmp_AlmDT = CLng(Me.TextBox57.Text)

            .LowTmp_WarSP = CDbl(Me.TextBox62.Text)
            .LowTmp_WarDT = CLng(Me.TextBox56.Text)
            .LowTmp_AlmSP = CDbl(Me.TextBox61.Text)
            .LowTmp_AlmDT = CLng(Me.TextBox55.Text)
        End With

        'FS6追加　23/10/04
        With frmMainte.WtrAlm_FS6
            .LowFlw_WarSP = CDbl(Me.Txb_FS6_LFW_THR.Text)
            .LowFlw_WarDT = CLng(Me.Txb_FS6_LFW_DT.Text)
            .LowFlw_AlmSP = CDbl(Me.Txb_FS6_LFA_THR.Text)
            .LowFlw_AlmDT = CLng(Me.Txb_FS6_LFA_DT.Text)

            .HigTmp_WarSP = CDbl(Me.Txb_FS6_HTW_THR.Text)
            .HigTmp_WarDT = CLng(Me.Txb_FS6_HTW_DT.Text)
            .HigTmp_AlmSP = CDbl(Me.Txb_FS6_HTA_THR.Text)
            .HigTmp_AlmDT = CLng(Me.Txb_FS6_HTA_DT.Text)

            .LowTmp_WarSP = CDbl(Me.Txb_FS6_LTW_THR.Text)
            .LowTmp_WarDT = CLng(Me.Txb_FS6_LTW_DT.Text)
            .LowTmp_AlmSP = CDbl(Me.Txb_FS6_LTA_THR.Text)
            .LowTmp_AlmDT = CLng(Me.Txb_FS6_LTA_DT.Text)
        End With

        With frmMainte.PyrAlm
            .HigWarSP = CInt(Me.TextBox52.Text)
            .HigWarDT = CLng(Me.TextBox48.Text)
            .HigAlmSP = CInt(Me.TextBox51.Text)
            .HigAlmDT = CLng(Me.TextBox47.Text)

            .LowWarSP = CInt(Me.TextBox50.Text)
            .LowWarDT = CLng(Me.TextBox46.Text)
            .LowAlmSP = CInt(Me.TextBox49.Text)
            .LowAlmDT = CLng(Me.TextBox45.Text)
        End With
    End Sub

    Private Function SHA256_cnv(ByVal pasw As String) As System.Text.StringBuilder
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

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        'Pass Word Set
        Dim paswrd_w As String
        Dim hashpw_w As String
        Dim paswrd_r As String = ""
        Dim hashpw_r As String = ""
        Dim i As Integer

        If TextBox53.Text <> TextBox54.Text Then
            MessageBox.Show("Passwords does not match." & vbCr & "Re-enter passwords.")
            TextBox54.Enabled = False
            Button2.Enabled = False
            TextBox53.Focus()
            TextBox53.BackColor = Color.Yellow
            Exit Sub
        End If

        paswrd_w = TextBox53.Text
        hashpw_w = SHA256_cnv(paswrd_w).ToString

        'Clear PLC regester
        With frmMainte
            For i = 1 To 512
                .AxDBDeviceManager4.Devices(i).ValueWrite = 0
            Next
            .WrComp = False
            .AxDBDeviceManager4.WriteAll()
            Do
                Application.DoEvents()
            Loop Until .WrComp = True
        End With
        frmMainte.AxDBCommManager1.WriteText(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM,
                                             PASWORD_PLC, Len(paswrd_w), paswrd_w)
        frmMainte.AxDBCommManager1.WriteText(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM,
                                             HASHPAS_PLC, Len(hashpw_w), hashpw_w)
        frmMainte.AxDBCommManager1.ReadText(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM,
                                            PASWORD_PLC, Len(paswrd_w), paswrd_r)
        frmMainte.AxDBCommManager1.ReadText(DATABUILDERAXLibLB.DBPlcDevice.DKV5000_DM,
                                             HASHPAS_PLC, Len(hashpw_w), hashpw_r)

        If (paswrd_r = paswrd_w) And (hashpw_r = hashpw_w) Then
            MessageBox.Show("The password has been changed successfully.")
        Else
            MessageBox.Show("Failed to wright to PLC!", "", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
            TextBox54.Enabled = False
            Button2.Enabled = False
            Exit Sub
        End If

        i = Len(hashpw_r)
        TextBox53.BackColor = SystemColors.ControlLight
        TextBox54.BackColor = SystemColors.ControlLight
        TextBox53.Text = ""
        TextBox54.Text = ""
        TextBox54.Enabled = False
        Button2.Enabled = False
        Label88.Text = hashpw_w

    End Sub

    Private Sub TextBox53_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox53.KeyDown
        TextBox53.BackColor = Color.Yellow
    End Sub

    Private Sub TextBox53_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox53.KeyPress
        If e.KeyChar = vbCr Then
            TextBox53.BackColor = SystemColors.ControlLight
            TextBox54.Enabled = True
            TextBox54.Focus()
        End If
    End Sub

    Private Sub TextBox54_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox54.KeyDown, TextBox51.KeyDown
        TextBox54.BackColor = Color.Yellow
    End Sub

    Private Sub TextBox54_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox54.KeyPress
        If e.KeyChar = vbCr Then
            TextBox54.BackColor = SystemColors.ControlLight
            Button2.Enabled = True
            Button2.Focus()
        End If
    End Sub

    Private Sub Threshold_set(ByRef TextBox As TextBox, pyro As Boolean)
        Dim sp As Single = CSng(TextBox.Text)
        If pyro = True Then
            If sp < 0 Then
                sp = 0
            ElseIf sp > 9999 Then
                sp = 9999
            End If
        Else
            If sp < 0 Then
                sp = 0
            ElseIf sp > 999.9 Then
                sp = 999.9
            End If
        End If
        TextBox.Text = sp
        TextBox.BackColor = SystemColors.ControlLight
    End Sub

    Private Sub DelayTim_set(ByRef TextBox As TextBox)
        Dim sp As Single = CSng(TextBox.Text)
        If sp < 0 Then
            sp = 0
        ElseIf sp > 3000 Then
            sp = 3000
        End If
        TextBox.Text = sp
        TextBox.BackColor = SystemColors.ControlLight
    End Sub


    Private Sub TextBoxT_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox52.KeyPress, TextBox51.KeyPress, TextBox50.KeyPress, TextBox49.KeyPress,
                                                                                    TextBox20.KeyPress, TextBox19.KeyPress, TextBox16.KeyPress, TextBox15.KeyPress, TextBox14.KeyPress, TextBox13.KeyPress,
                                                                                    TextBox32.KeyPress, TextBox31.KeyPress, TextBox30.KeyPress, TextBox29.KeyPress, TextBox24.KeyPress, TextBox23.KeyPress,
                                                                                    TextBox44.KeyPress, TextBox43.KeyPress, TextBox42.KeyPress, TextBox41.KeyPress, TextBox36.KeyPress, TextBox35.KeyPress,
                                                                                    TextBox78.KeyPress, TextBox77.KeyPress, TextBox76.KeyPress, TextBox75.KeyPress, TextBox74.KeyPress, TextBox73.KeyPress,
                                                                                    TextBox66.KeyPress, TextBox65.KeyPress, TextBox64.KeyPress, TextBox63.KeyPress, TextBox62.KeyPress, TextBox61.KeyPress,
                                                                                    Txb_FS6_LFW_THR.KeyPress, Txb_FS6_LFA_THR.KeyPress, Txb_FS6_HTW_THR.KeyPress, Txb_FS6_HTA_THR.KeyPress,
                                                                                    Txb_FS6_LTW_THR.KeyPress, Txb_FS6_LTA_THR.KeyPress
        Dim pyro As Boolean = True
        Dim txtb As String = sender.Name
        Dim textbox As TextBox = TextBox52
        Select Case txtb
            Case "TextBox52"
                textbox = TextBox52 : pyro = True
            Case "TextBox51"
                textbox = TextBox51 : pyro = True
            Case "TextBox50"
                textbox = TextBox50 : pyro = True
            Case "TextBox49"
                textbox = TextBox49 : pyro = True
            Case "TextBox20"
                textbox = TextBox20 : pyro = False
            Case "TextBox19"
                textbox = TextBox19 : pyro = False
            Case "TextBox16"
                textbox = TextBox16 : pyro = False
            Case "TextBox15"
                textbox = TextBox15 : pyro = False
            Case "TextBox14"
                textbox = TextBox14 : pyro = False
            Case "TextBox13"
                textbox = TextBox13 : pyro = False
            Case "TextBox32"
                textbox = TextBox32 : pyro = False
            Case "TextBox31"
                textbox = TextBox31 : pyro = False
            Case "TextBox30"
                textbox = TextBox30 : pyro = False
            Case "TextBox29"
                textbox = TextBox29 : pyro = False
            Case "TextBox24"
                textbox = TextBox24 : pyro = False
            Case "TextBox23"
                textbox = TextBox23 : pyro = False
            Case "TextBox44"
                textbox = TextBox44 : pyro = False
            Case "TextBox43"
                textbox = TextBox43 : pyro = False
            Case "TextBox42"
                textbox = TextBox42 : pyro = False
            Case "TextBox41"
                textbox = TextBox41 : pyro = False
            Case "TextBox36"
                textbox = TextBox36 : pyro = False
            Case "TextBox35"
                textbox = TextBox35 : pyro = False
                '23/2/2 FS4,5追加
            Case "TextBox78"
                textbox = TextBox78 : pyro = False
            Case "TextBox77"
                textbox = TextBox77 : pyro = False
            Case "TextBox76"
                textbox = TextBox76 : pyro = False
            Case "TextBox75"
                textbox = TextBox75 : pyro = False
            Case "TextBox74"
                textbox = TextBox74 : pyro = False
            Case "TextBox73"
                textbox = TextBox73 : pyro = False
            Case "TextBox66"
                textbox = TextBox66 : pyro = False
            Case "TextBox65"
                textbox = TextBox65 : pyro = False
            Case "TextBox64"
                textbox = TextBox64 : pyro = False
            Case "TextBox63"
                textbox = TextBox63 : pyro = False
            Case "TextBox62"
                textbox = TextBox62 : pyro = False
            Case "TextBox61"
                textbox = TextBox61 : pyro = False
                 '23/10/04 FS6追加 Threshold
            Case "Txb_FS6_LFW_THR"
                textbox = Txb_FS6_LFW_THR : pyro = False
            Case "Txb_FS6_LFA_THR"
                textbox = Txb_FS6_LFA_THR : pyro = False
            Case "Txb_FS6_HTW_THR"
                textbox = Txb_FS6_HTW_THR : pyro = False
            Case "Txb_FS6_HTA_THR"
                textbox = Txb_FS6_HTA_THR : pyro = False
            Case "Txb_FS6_LTW_THR"
                textbox = Txb_FS6_LTW_THR : pyro = False
            Case "Txb_FS6_LTA_THR"
                textbox = Txb_FS6_LTA_THR : pyro = False
        End Select

        If e.KeyChar = vbCr Then
            Threshold_set(textbox, pyro)
        End If
    End Sub

    Private Sub TextBoxD_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox48.KeyPress, TextBox47.KeyPress, TextBox46.KeyPress, TextBox45.KeyPress,
                                                                                    TextBox18.KeyPress, TextBox17.KeyPress, TextBox12.KeyPress, TextBox11.KeyPress, TextBox10.KeyPress, TextBox9.KeyPress,
                                                                                    TextBox28.KeyPress, TextBox27.KeyPress, TextBox26.KeyPress, TextBox25.KeyPress, TextBox22.KeyPress, TextBox21.KeyPress,
                                                                                    TextBox40.KeyPress, TextBox39.KeyPress, TextBox38.KeyPress, TextBox37.KeyPress, TextBox34.KeyPress, TextBox33.KeyPress,
                                                                                    TextBox72.KeyPress, TextBox71.KeyPress, TextBox70.KeyPress, TextBox69.KeyPress, TextBox68.KeyPress, TextBox67.KeyPress,
                                                                                    TextBox60.KeyPress, TextBox59.KeyPress, TextBox58.KeyPress, TextBox57.KeyPress, TextBox56.KeyPress, TextBox55.KeyPress,
                                                                                    Txb_FS6_LFW_DT.KeyPress, Txb_FS6_LFA_DT.KeyPress, Txb_FS6_HTW_DT.KeyPress, Txb_FS6_HTA_DT.KeyPress,
                                                                                    Txb_FS6_LTW_DT.KeyPress, Txb_FS6_LTA_DT.KeyPress

        Dim txtb As String = sender.Name
        Dim textbox As TextBox = TextBox52
        Select Case txtb
            Case "TextBox48"
                textbox = TextBox48
            Case "TextBox47"
                textbox = TextBox47
            Case "TextBox46"
                textbox = TextBox46
            Case "TextBox45"
                textbox = TextBox45
            Case "TextBox18"
                textbox = TextBox18
            Case "TextBox17"
                textbox = TextBox17
            Case "TextBox12"
                textbox = TextBox12
            Case "TextBox11"
                textbox = TextBox11
            Case "TextBox10"
                textbox = TextBox10
            Case "TextBox9"
                textbox = TextBox9
            Case "TextBox28"
                textbox = TextBox28
            Case "TextBox27"
                textbox = TextBox27
            Case "TextBox26"
                textbox = TextBox26
            Case "TextBox25"
                textbox = TextBox25
            Case "TextBox22"
                textbox = TextBox22
            Case "TextBox21"
                textbox = TextBox21
            Case "TextBox40"
                textbox = TextBox40
            Case "TextBox39"
                textbox = TextBox39
            Case "TextBox38"
                textbox = TextBox38
            Case "TextBox37"
                textbox = TextBox37
            Case "TextBox34"
                textbox = TextBox34
            Case "TextBox33"
                textbox = TextBox33
                '23/2/2 FS4,5追加
            Case "TextBox72"
                textbox = TextBox72
            Case "TextBox71"
                textbox = TextBox71
            Case "TextBox70"
                textbox = TextBox70
            Case "TextBox69"
                textbox = TextBox69
            Case "TextBox68"
                textbox = TextBox68
            Case "TextBox67"
                textbox = TextBox67
            Case "TextBox60"
                textbox = TextBox60
            Case "TextBox59"
                textbox = TextBox59
            Case "TextBox58"
                textbox = TextBox58
            Case "TextBox57"
                textbox = TextBox57
            Case "TextBox56"
                textbox = TextBox56
            Case "TextBox55"
                textbox = TextBox55
                 '23/10/04 FS6追加 Delay Time
            Case "Txb_FS6_LFW_DT"
                textbox = Txb_FS6_LFW_DT
            Case "Txb_FS6_LFA_DT"
                textbox = Txb_FS6_LFA_DT
            Case "Txb_FS6_HTW_DT"
                textbox = Txb_FS6_HTW_DT
            Case "Txb_FS6_HTA_DT"
                textbox = Txb_FS6_HTA_DT
            Case "Txb_FS6_LTW_DT"
                textbox = Txb_FS6_LTW_DT
            Case "Txb_FS6_LTA_DT"
                textbox = Txb_FS6_LTA_DT
        End Select

        If e.KeyChar = vbCr Then
            DelayTim_set(textbox)
        End If

    End Sub

    Private Sub TextBoxT_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox52.KeyDown, TextBox51.KeyDown, TextBox50.KeyDown, TextBox49.KeyDown,
                                                                              TextBox20.KeyDown, TextBox19.KeyDown, TextBox16.KeyDown, TextBox15.KeyDown, TextBox14.KeyDown, TextBox13.KeyDown,
                                                                              TextBox32.KeyDown, TextBox31.KeyDown, TextBox30.KeyDown, TextBox29.KeyDown, TextBox24.KeyDown, TextBox23.KeyDown,
                                                                              TextBox44.KeyDown, TextBox43.KeyDown, TextBox42.KeyDown, TextBox41.KeyDown, TextBox36.KeyDown, TextBox35.KeyDown,
                                                                              TextBox78.KeyDown, TextBox77.KeyDown, TextBox76.KeyDown, TextBox75.KeyDown, TextBox74.KeyDown, TextBox73.KeyDown,
                                                                              TextBox66.KeyDown, TextBox65.KeyDown, TextBox64.KeyDown, TextBox63.KeyDown, TextBox62.KeyDown, TextBox61.KeyDown,
                                                                              Txb_FS6_LFW_THR.KeyDown, Txb_FS6_LFA_THR.KeyDown, Txb_FS6_HTW_THR.KeyDown, Txb_FS6_HTA_THR.KeyDown,
                                                                              Txb_FS6_LTW_THR.KeyDown, Txb_FS6_LTA_THR.KeyDown
        Dim pyro As Boolean = True
        Dim txtb As String = sender.Name
        Dim textbox As TextBox = TextBox52
        Select Case txtb
            Case "TextBox52"
                textbox = TextBox52 : pyro = True
            Case "TextBox51"
                textbox = TextBox51 : pyro = True
            Case "TextBox50"
                textbox = TextBox50 : pyro = True
            Case "TextBox49"
                textbox = TextBox49 : pyro = True
            Case "TextBox20"
                textbox = TextBox20 : pyro = False
            Case "TextBox19"
                textbox = TextBox19 : pyro = False
            Case "TextBox16"
                textbox = TextBox16 : pyro = False
            Case "TextBox15"
                textbox = TextBox15 : pyro = False
            Case "TextBox14"
                textbox = TextBox14 : pyro = False
            Case "TextBox13"
                textbox = TextBox13 : pyro = False
            Case "TextBox32"
                textbox = TextBox32 : pyro = False
            Case "TextBox31"
                textbox = TextBox31 : pyro = False
            Case "TextBox30"
                textbox = TextBox30 : pyro = False
            Case "TextBox29"
                textbox = TextBox29 : pyro = False
            Case "TextBox24"
                textbox = TextBox24 : pyro = False
            Case "TextBox23"
                textbox = TextBox23 : pyro = False
            Case "TextBox44"
                textbox = TextBox44 : pyro = False
            Case "TextBox43"
                textbox = TextBox43 : pyro = False
            Case "TextBox42"
                textbox = TextBox42 : pyro = False
            Case "TextBox41"
                textbox = TextBox41 : pyro = False
            Case "TextBox36"
                textbox = TextBox36 : pyro = False
            Case "TextBox35"
                textbox = TextBox35 : pyro = False
                '23/2/2 FS4,5追加
            Case "TextBox78"
                textbox = TextBox78 : pyro = False
            Case "TextBox77"
                textbox = TextBox77 : pyro = False
            Case "TextBox76"
                textbox = TextBox76 : pyro = False
            Case "TextBox75"
                textbox = TextBox75 : pyro = False
            Case "TextBox74"
                textbox = TextBox74 : pyro = False
            Case "TextBox73"
                textbox = TextBox73 : pyro = False
            Case "TextBox66"
                textbox = TextBox66 : pyro = False
            Case "TextBox65"
                textbox = TextBox65 : pyro = False
            Case "TextBox64"
                textbox = TextBox64 : pyro = False
            Case "TextBox63"
                textbox = TextBox63 : pyro = False
            Case "TextBox62"
                textbox = TextBox62 : pyro = False
            Case "TextBox61"
                textbox = TextBox61 : pyro = False
                '23/10/04 FS6追加 Threshold
            Case "Txb_FS6_LFW_THR"
                textbox = Txb_FS6_LFW_THR : pyro = False
            Case "Txb_FS6_LFA_THR"
                textbox = Txb_FS6_LFA_THR : pyro = False
            Case "Txb_FS6_HTW_THR"
                textbox = Txb_FS6_HTW_THR : pyro = False
            Case "Txb_FS6_HTA_THR"
                textbox = Txb_FS6_HTA_THR : pyro = False
            Case "Txb_FS6_LTW_THR"
                textbox = Txb_FS6_LTW_THR : pyro = False
            Case "Txb_FS6_LTA_THR"
                textbox = Txb_FS6_LTA_THR : pyro = False
        End Select

        If e.KeyCode = Keys.A Then
        Else
            textbox.BackColor = Color.Yellow
        End If
    End Sub

    Private Sub TextBoxD_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox48.KeyDown, TextBox47.KeyDown, TextBox46.KeyDown, TextBox45.KeyDown,
                                                                                    TextBox18.KeyDown, TextBox17.KeyDown, TextBox12.KeyDown, TextBox11.KeyDown, TextBox10.KeyDown, TextBox9.KeyDown,
                                                                                    TextBox28.KeyDown, TextBox27.KeyDown, TextBox26.KeyDown, TextBox25.KeyDown, TextBox22.KeyDown, TextBox21.KeyDown,
                                                                                    TextBox40.KeyDown, TextBox39.KeyDown, TextBox38.KeyDown, TextBox37.KeyDown, TextBox34.KeyDown, TextBox33.KeyDown,
                                                                                    TextBox72.KeyDown, TextBox71.KeyDown, TextBox70.KeyDown, TextBox69.KeyDown, TextBox68.KeyDown, TextBox67.KeyDown,
                                                                                    TextBox60.KeyDown, TextBox59.KeyDown, TextBox58.KeyDown, TextBox57.KeyDown, TextBox56.KeyDown, TextBox55.KeyDown,
                                                                                    Txb_FS6_LFW_DT.KeyDown, Txb_FS6_LFA_DT.KeyDown, Txb_FS6_HTW_DT.KeyDown, Txb_FS6_HTA_DT.KeyDown,
                                                                                    Txb_FS6_LTW_DT.KeyDown, Txb_FS6_LTA_DT.KeyDown

        Dim txtb As String = sender.Name
        Dim textbox As TextBox = TextBox52
        Select Case txtb
            Case "TextBox48"
                textbox = TextBox48
            Case "TextBox47"
                textbox = TextBox47
            Case "TextBox46"
                textbox = TextBox46
            Case "TextBox45"
                textbox = TextBox45
            Case "TextBox18"
                textbox = TextBox18
            Case "TextBox17"
                textbox = TextBox17
            Case "TextBox12"
                textbox = TextBox12
            Case "TextBox11"
                textbox = TextBox11
            Case "TextBox10"
                textbox = TextBox10
            Case "TextBox9"
                textbox = TextBox9
            Case "TextBox28"
                textbox = TextBox28
            Case "TextBox27"
                textbox = TextBox27
            Case "TextBox26"
                textbox = TextBox26
            Case "TextBox25"
                textbox = TextBox25
            Case "TextBox22"
                textbox = TextBox22
            Case "TextBox21"
                textbox = TextBox21
            Case "TextBox40"
                textbox = TextBox40
            Case "TextBox39"
                textbox = TextBox39
            Case "TextBox38"
                textbox = TextBox38
            Case "TextBox37"
                textbox = TextBox37
            Case "TextBox34"
                textbox = TextBox34
            Case "TextBox33"
                textbox = TextBox33
                '23/2/2 FS4,5追加
            Case "TextBox72"
                textbox = TextBox72
            Case "TextBox71"
                textbox = TextBox71
            Case "TextBox70"
                textbox = TextBox70
            Case "TextBox69"
                textbox = TextBox69
            Case "TextBox68"
                textbox = TextBox68
            Case "TextBox67"
                textbox = TextBox67
            Case "TextBox60"
                textbox = TextBox60
            Case "TextBox59"
                textbox = TextBox59
            Case "TextBox58"
                textbox = TextBox58
            Case "TextBox57"
                textbox = TextBox57
            Case "TextBox56"
                textbox = TextBox56
            Case "TextBox55"
                textbox = TextBox55
            '23/10/04 FS6追加 Delay Time
            Case "Txb_FS6_LFW_DT"
                textbox = Txb_FS6_LFW_DT
            Case "Txb_FS6_LFA_DT"
                textbox = Txb_FS6_LFA_DT
            Case "Txb_FS6_HTW_DT"
                textbox = Txb_FS6_HTW_DT
            Case "Txb_FS6_HTA_DT"
                textbox = Txb_FS6_HTA_DT
            Case "Txb_FS6_LTW_DT"
                textbox = Txb_FS6_LTW_DT
            Case "Txb_FS6_LTA_DT"
                textbox = Txb_FS6_LTA_DT

        End Select

        If e.KeyCode = Keys.A Then
        Else
            textbox.BackColor = Color.Yellow
        End If
    End Sub

End Class