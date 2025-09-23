<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmBattleReport
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
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

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        rtbBattleReport = New RichTextBox()
        SuspendLayout()
        ' 
        ' rtbBattleReport
        ' 
        rtbBattleReport.Dock = DockStyle.Fill
        rtbBattleReport.Location = New Point(0, 0)
        rtbBattleReport.Name = "rtbBattleReport"
        rtbBattleReport.ReadOnly = True
        rtbBattleReport.Size = New Size(1192, 825)
        rtbBattleReport.TabIndex = 0
        rtbBattleReport.Text = ""
        rtbBattleReport.WordWrap = False
        ' 
        ' frmBattleReport
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1192, 825)
        Controls.Add(rtbBattleReport)
        Name = "frmBattleReport"
        Text = "frmBattleReport"
        ResumeLayout(False)
    End Sub

    Friend WithEvents rtbBattleReport As RichTextBox
End Class
