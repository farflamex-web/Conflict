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
        btnNextTurn = New Button()
        lstResources = New ListBox()
        SuspendLayout()
        ' 
        ' btnPrint
        ' 
        btnPrint.Location = New Point(713, 415)
        btnPrint.Name = "btnPrint"
        btnPrint.Size = New Size(75, 23)
        btnPrint.TabIndex = 0
        btnPrint.Text = "Print"
        btnPrint.UseVisualStyleBackColor = True
        ' 
        ' btnNextTurn
        ' 
        btnNextTurn.Location = New Point(632, 415)
        btnNextTurn.Name = "btnNextTurn"
        btnNextTurn.Size = New Size(75, 23)
        btnNextTurn.TabIndex = 1
        btnNextTurn.Text = "Next Turn"
        btnNextTurn.UseVisualStyleBackColor = True
        ' 
        ' lstResources
        ' 
        lstResources.FormattingEnabled = True
        lstResources.ItemHeight = 15
        lstResources.Location = New Point(21, 23)
        lstResources.Name = "lstResources"
        lstResources.Size = New Size(1219, 289)
        lstResources.TabIndex = 2
        ' 
        ' Form1
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1252, 450)
        Controls.Add(lstResources)
        Controls.Add(btnNextTurn)
        Controls.Add(btnPrint)
        Name = "Form1"
        Text = "Form1"
        ResumeLayout(False)
    End Sub

    Friend WithEvents btnPrint As Button
    Friend WithEvents btnNextTurn As Button
    Friend WithEvents lstResources As ListBox

End Class
