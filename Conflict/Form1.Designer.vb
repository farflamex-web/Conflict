<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(disposing As Boolean)
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        btnPrint = New Button()
        pnlMap = New Panel()
        btn_Show = New Button()
        btnProcessTurn = New Button()
        rtbInfo = New RichTextBox()
        rtbResourceInfo = New RichTextBox()
        rtbPlayerSummary = New RichTextBox()
        SuspendLayout()
        ' 
        ' btnPrint
        ' 
        btnPrint.Location = New Point(15, 768)
        btnPrint.Name = "btnPrint"
        btnPrint.Size = New Size(124, 23)
        btnPrint.TabIndex = 0
        btnPrint.Text = "Print"
        btnPrint.UseVisualStyleBackColor = True
        ' 
        ' pnlMap
        ' 
        pnlMap.Location = New Point(15, 13)
        pnlMap.Name = "pnlMap"
        pnlMap.Size = New Size(391, 413)
        pnlMap.TabIndex = 2
        ' 
        ' btn_Show
        ' 
        btn_Show.Location = New Point(15, 730)
        btn_Show.Name = "btn_Show"
        btn_Show.Size = New Size(75, 23)
        btn_Show.TabIndex = 3
        btn_Show.Text = "Show"
        btn_Show.UseVisualStyleBackColor = True
        ' 
        ' btnProcessTurn
        ' 
        btnProcessTurn.Location = New Point(15, 797)
        btnProcessTurn.Name = "btnProcessTurn"
        btnProcessTurn.Size = New Size(124, 23)
        btnProcessTurn.TabIndex = 4
        btnProcessTurn.Text = "Process Turn"
        btnProcessTurn.UseVisualStyleBackColor = True
        ' 
        ' rtbInfo
        ' 
        rtbInfo.Location = New Point(412, 13)
        rtbInfo.Name = "rtbInfo"
        rtbInfo.ReadOnly = True
        rtbInfo.ScrollBars = RichTextBoxScrollBars.Vertical
        rtbInfo.Size = New Size(369, 807)
        rtbInfo.TabIndex = 5
        rtbInfo.Text = ""
        ' 
        ' rtbResourceInfo
        ' 
        rtbResourceInfo.Location = New Point(787, 13)
        rtbResourceInfo.Name = "rtbResourceInfo"
        rtbResourceInfo.Size = New Size(528, 807)
        rtbResourceInfo.TabIndex = 6
        rtbResourceInfo.Text = ""
        ' 
        ' rtbPlayerSummary
        ' 
        rtbPlayerSummary.Location = New Point(15, 432)
        rtbPlayerSummary.Name = "rtbPlayerSummary"
        rtbPlayerSummary.Size = New Size(391, 292)
        rtbPlayerSummary.TabIndex = 7
        rtbPlayerSummary.Text = ""
        ' 
        ' Form1
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1327, 832)
        Controls.Add(rtbPlayerSummary)
        Controls.Add(rtbResourceInfo)
        Controls.Add(rtbInfo)
        Controls.Add(btnProcessTurn)
        Controls.Add(btn_Show)
        Controls.Add(pnlMap)
        Controls.Add(btnPrint)
        Name = "Form1"
        Text = "Form1"
        ResumeLayout(False)
    End Sub

    Friend WithEvents btnPrint As Button
    Friend WithEvents pnlMap As Panel
    Friend WithEvents btn_Show As Button
    Friend WithEvents btnProcessTurn As Button
    Friend WithEvents rtbInfo As RichTextBox
    Friend WithEvents rtbResourceInfo As RichTextBox
    Friend WithEvents rtbPlayerSummary As RichTextBox

End Class
