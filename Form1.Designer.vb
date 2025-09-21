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
        SuspendLayout()
        ' 
        ' btnPrint
        ' 
        btnPrint.Location = New Point(1191, 768)
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
        ' btnProcessTurn
        ' 
        btnProcessTurn.Location = New Point(1191, 797)
        btnProcessTurn.Name = "btnProcessTurn"
        btnProcessTurn.Size = New Size(124, 23)
        btnProcessTurn.TabIndex = 4
        btnProcessTurn.Text = "Process Turn"
        btnProcessTurn.UseVisualStyleBackColor = True
        ' 
        ' Form1
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1327, 832)
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

End Class
