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
        btn_Print = New Button()
        btnNextTurn = New Button()
        pnlMap = New Panel()
        btn_Show = New Button()
        SuspendLayout()
        ' 
        ' btn_Print
        ' 
        btn_Print.Location = New Point(1240, 768)
        btn_Print.Name = "btn_Print"
        btn_Print.Size = New Size(75, 23)
        btn_Print.TabIndex = 0
        btn_Print.Text = "Print"
        btn_Print.UseVisualStyleBackColor = True
        ' 
        ' btnNextTurn
        ' 
        btnNextTurn.Location = New Point(1240, 797)
        btnNextTurn.Name = "btnNextTurn"
        btnNextTurn.Size = New Size(75, 23)
        btnNextTurn.TabIndex = 1
        btnNextTurn.Text = "Next Turn"
        btnNextTurn.UseVisualStyleBackColor = True
        ' 
        ' pnlMap
        ' 
        pnlMap.Location = New Point(15, 13)
        pnlMap.Name = "pnlMap"
        pnlMap.Size = New Size(600, 600)
        pnlMap.TabIndex = 2
        ' 
        ' btn_Show
        ' 
        btn_Show.Location = New Point(1240, 739)
        btn_Show.Name = "btn_Show"
        btn_Show.Size = New Size(75, 23)
        btn_Show.TabIndex = 3
        btn_Show.Text = "Show"
        btn_Show.UseVisualStyleBackColor = True
        ' 
        ' Form1
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1327, 832)
        Controls.Add(btn_Show)
        Controls.Add(pnlMap)
        Controls.Add(btnNextTurn)
        Controls.Add(btn_Print)
        Name = "Form1"
        Text = "Form1"
        ResumeLayout(False)
    End Sub

    Friend WithEvents btn_Print As Button
    Friend WithEvents btnNextTurn As Button
    Friend WithEvents pnlMap As Panel
    Friend WithEvents btn_Show As Button

End Class
