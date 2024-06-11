<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmALRM
    Inherits System.Windows.Forms.Form

    'フォームがコンポーネントの一覧をクリーンアップするために dispose をオーバーライドします。
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Windows フォーム デザイナーで必要です。
    Private components As System.ComponentModel.IContainer

    'メモ: 以下のプロシージャは Windows フォーム デザイナーで必要です。
    'Windows フォーム デザイナーを使用して変更できます。  
    'コード エディターを使って変更しないでください。
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmALRM))
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.AxDBDeviceManager2 = New AxDATABUILDERAXLibLB.AxDBDeviceManager()
        Me.AxDBDeviceManager1 = New AxDATABUILDERAXLibLB.AxDBDeviceManager()
        Me.AxDBTriggerManager2 = New AxDATABUILDERAXLibLB.AxDBTriggerManager()
        Me.AxDBTriggerManager1 = New AxDATABUILDERAXLibLB.AxDBTriggerManager()
        Me.AxDBCommManager1 = New AxDATABUILDERAXLibLB.AxDBCommManager()
        Me.AxDBDeviceManager3 = New AxDATABUILDERAXLibLB.AxDBDeviceManager()
        Me.AxDBTriggerManager3 = New AxDATABUILDERAXLibLB.AxDBTriggerManager()
        Me.ListBox1 = New System.Windows.Forms.ListBox()
        Me.ListBox2 = New System.Windows.Forms.ListBox()
        CType(Me.AxDBDeviceManager2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.AxDBDeviceManager1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.AxDBTriggerManager2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.AxDBTriggerManager1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.AxDBCommManager1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.AxDBDeviceManager3, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.AxDBTriggerManager3, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Verdana", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.ForeColor = System.Drawing.Color.Yellow
        Me.Label1.Location = New System.Drawing.Point(12, 25)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(76, 18)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "Current"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Verdana", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.ForeColor = System.Drawing.Color.Yellow
        Me.Label2.Location = New System.Drawing.Point(12, 265)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(70, 18)
        Me.Label2.TabIndex = 2
        Me.Label2.Text = "History"
        '
        'AxDBDeviceManager2
        '
        Me.AxDBDeviceManager2.Enabled = True
        Me.AxDBDeviceManager2.Location = New System.Drawing.Point(497, 13)
        Me.AxDBDeviceManager2.Name = "AxDBDeviceManager2"
        Me.AxDBDeviceManager2.OcxState = CType(resources.GetObject("AxDBDeviceManager2.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxDBDeviceManager2.Size = New System.Drawing.Size(24, 24)
        Me.AxDBDeviceManager2.TabIndex = 8
        Me.AxDBDeviceManager2.Visible = False
        '
        'AxDBDeviceManager1
        '
        Me.AxDBDeviceManager1.Enabled = True
        Me.AxDBDeviceManager1.Location = New System.Drawing.Point(462, 13)
        Me.AxDBDeviceManager1.Name = "AxDBDeviceManager1"
        Me.AxDBDeviceManager1.OcxState = CType(resources.GetObject("AxDBDeviceManager1.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxDBDeviceManager1.Size = New System.Drawing.Size(24, 24)
        Me.AxDBDeviceManager1.TabIndex = 7
        Me.AxDBDeviceManager1.Visible = False
        '
        'AxDBTriggerManager2
        '
        Me.AxDBTriggerManager2.Enabled = True
        Me.AxDBTriggerManager2.Location = New System.Drawing.Point(427, 13)
        Me.AxDBTriggerManager2.Name = "AxDBTriggerManager2"
        Me.AxDBTriggerManager2.OcxState = CType(resources.GetObject("AxDBTriggerManager2.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxDBTriggerManager2.Size = New System.Drawing.Size(24, 24)
        Me.AxDBTriggerManager2.TabIndex = 6
        Me.AxDBTriggerManager2.Visible = False
        '
        'AxDBTriggerManager1
        '
        Me.AxDBTriggerManager1.Enabled = True
        Me.AxDBTriggerManager1.Location = New System.Drawing.Point(393, 13)
        Me.AxDBTriggerManager1.Name = "AxDBTriggerManager1"
        Me.AxDBTriggerManager1.OcxState = CType(resources.GetObject("AxDBTriggerManager1.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxDBTriggerManager1.Size = New System.Drawing.Size(24, 24)
        Me.AxDBTriggerManager1.TabIndex = 5
        Me.AxDBTriggerManager1.Visible = False
        '
        'AxDBCommManager1
        '
        Me.AxDBCommManager1.Enabled = True
        Me.AxDBCommManager1.Location = New System.Drawing.Point(360, 13)
        Me.AxDBCommManager1.Name = "AxDBCommManager1"
        Me.AxDBCommManager1.OcxState = CType(resources.GetObject("AxDBCommManager1.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxDBCommManager1.Size = New System.Drawing.Size(24, 24)
        Me.AxDBCommManager1.TabIndex = 4
        Me.AxDBCommManager1.Visible = False
        '
        'AxDBDeviceManager3
        '
        Me.AxDBDeviceManager3.Enabled = True
        Me.AxDBDeviceManager3.Location = New System.Drawing.Point(532, 13)
        Me.AxDBDeviceManager3.Name = "AxDBDeviceManager3"
        Me.AxDBDeviceManager3.OcxState = CType(resources.GetObject("AxDBDeviceManager3.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxDBDeviceManager3.Size = New System.Drawing.Size(24, 24)
        Me.AxDBDeviceManager3.TabIndex = 9
        '
        'AxDBTriggerManager3
        '
        Me.AxDBTriggerManager3.Enabled = True
        Me.AxDBTriggerManager3.Location = New System.Drawing.Point(562, 13)
        Me.AxDBTriggerManager3.Name = "AxDBTriggerManager3"
        Me.AxDBTriggerManager3.OcxState = CType(resources.GetObject("AxDBTriggerManager3.OcxState"), System.Windows.Forms.AxHost.State)
        Me.AxDBTriggerManager3.Size = New System.Drawing.Size(24, 24)
        Me.AxDBTriggerManager3.TabIndex = 10
        '
        'ListBox1
        '
        Me.ListBox1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.ListBox1.Font = New System.Drawing.Font("Verdana", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ListBox1.ForeColor = System.Drawing.Color.Red
        Me.ListBox1.FormattingEnabled = True
        Me.ListBox1.HorizontalScrollbar = True
        Me.ListBox1.ItemHeight = 16
        Me.ListBox1.Location = New System.Drawing.Point(12, 46)
        Me.ListBox1.Name = "ListBox1"
        Me.ListBox1.Size = New System.Drawing.Size(610, 194)
        Me.ListBox1.TabIndex = 11
        '
        'ListBox2
        '
        Me.ListBox2.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.ListBox2.Font = New System.Drawing.Font("Verdana", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ListBox2.ForeColor = System.Drawing.Color.DimGray
        Me.ListBox2.FormattingEnabled = True
        Me.ListBox2.HorizontalScrollbar = True
        Me.ListBox2.IntegralHeight = False
        Me.ListBox2.ItemHeight = 16
        Me.ListBox2.Location = New System.Drawing.Point(12, 286)
        Me.ListBox2.Name = "ListBox2"
        Me.ListBox2.Size = New System.Drawing.Size(610, 434)
        Me.ListBox2.TabIndex = 12
        '
        'frmALRM
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.Red
        Me.ClientSize = New System.Drawing.Size(634, 762)
        Me.Controls.Add(Me.ListBox2)
        Me.Controls.Add(Me.ListBox1)
        Me.Controls.Add(Me.AxDBTriggerManager3)
        Me.Controls.Add(Me.AxDBDeviceManager3)
        Me.Controls.Add(Me.AxDBDeviceManager2)
        Me.Controls.Add(Me.AxDBDeviceManager1)
        Me.Controls.Add(Me.AxDBTriggerManager2)
        Me.Controls.Add(Me.AxDBTriggerManager1)
        Me.Controls.Add(Me.AxDBCommManager1)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.Name = "frmALRM"
        Me.Text = "SDS6K-10kE ALARM LOGGER Ver1.00"
        CType(Me.AxDBDeviceManager2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.AxDBDeviceManager1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.AxDBTriggerManager2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.AxDBTriggerManager1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.AxDBCommManager1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.AxDBDeviceManager3, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.AxDBTriggerManager3, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents AxDBCommManager1 As AxDATABUILDERAXLibLB.AxDBCommManager
    Friend WithEvents AxDBTriggerManager1 As AxDATABUILDERAXLibLB.AxDBTriggerManager
    Friend WithEvents AxDBTriggerManager2 As AxDATABUILDERAXLibLB.AxDBTriggerManager
    Friend WithEvents AxDBDeviceManager1 As AxDATABUILDERAXLibLB.AxDBDeviceManager
    Friend WithEvents AxDBDeviceManager2 As AxDATABUILDERAXLibLB.AxDBDeviceManager
    Friend WithEvents AxDBDeviceManager3 As AxDATABUILDERAXLibLB.AxDBDeviceManager
    Friend WithEvents AxDBTriggerManager3 As AxDATABUILDERAXLibLB.AxDBTriggerManager
    Friend WithEvents ListBox1 As ListBox
    Friend WithEvents ListBox2 As ListBox
End Class
