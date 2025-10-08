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
        btnProcessTurn = New Button()
        rtbInfo = New RichTextBox()
        rtbPlayerSummary = New RichTextBox()
        rtbArmies = New RichTextBox()
        lblHud = New Label()
        btnNewGame = New Button()
        btnLoadGame = New Button()
        btnKillPlayer = New Button()
        dgvCustomers = New DataGridView()
        btnSaveCustomers = New Button()
        grp = New GroupBox()
        btnApplyAssignments = New Button()
        cmbHuman = New ComboBox()
        lblHuman = New Label()
        cmbOrc = New ComboBox()
        lblOrc = New Label()
        cmbDwarf = New ComboBox()
        lblDwarf = New Label()
        cmbElf = New ComboBox()
        lblElf = New Label()
        btnSave = New Button()
        CType(dgvCustomers, ComponentModel.ISupportInitialize).BeginInit()
        grp.SuspendLayout()
        SuspendLayout()
        ' 
        ' btnPrint
        ' 
        btnPrint.Location = New Point(1168, 812)
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
        pnlMap.Size = New Size(391, 432)
        pnlMap.TabIndex = 2
        ' 
        ' btnProcessTurn
        ' 
        btnProcessTurn.Location = New Point(1038, 812)
        btnProcessTurn.Name = "btnProcessTurn"
        btnProcessTurn.Size = New Size(124, 23)
        btnProcessTurn.TabIndex = 4
        btnProcessTurn.Text = "Process Turn"
        btnProcessTurn.UseVisualStyleBackColor = True
        ' 
        ' rtbInfo
        ' 
        rtbInfo.Location = New Point(412, 12)
        rtbInfo.Name = "rtbInfo"
        rtbInfo.ReadOnly = True
        rtbInfo.ScrollBars = RichTextBoxScrollBars.Vertical
        rtbInfo.Size = New Size(369, 433)
        rtbInfo.TabIndex = 5
        rtbInfo.Text = ""
        ' 
        ' rtbPlayerSummary
        ' 
        rtbPlayerSummary.Location = New Point(15, 451)
        rtbPlayerSummary.Name = "rtbPlayerSummary"
        rtbPlayerSummary.Size = New Size(391, 508)
        rtbPlayerSummary.TabIndex = 7
        rtbPlayerSummary.Text = ""
        ' 
        ' rtbArmies
        ' 
        rtbArmies.Location = New Point(412, 451)
        rtbArmies.Name = "rtbArmies"
        rtbArmies.Size = New Size(369, 508)
        rtbArmies.TabIndex = 8
        rtbArmies.Text = ""
        ' 
        ' lblHud
        ' 
        lblHud.AutoSize = True
        lblHud.Location = New Point(1038, 874)
        lblHud.Name = "lblHud"
        lblHud.Size = New Size(41, 15)
        lblHud.TabIndex = 9
        lblHud.Text = "Label1"
        ' 
        ' btnNewGame
        ' 
        btnNewGame.Location = New Point(1381, 836)
        btnNewGame.Name = "btnNewGame"
        btnNewGame.Size = New Size(119, 23)
        btnNewGame.TabIndex = 10
        btnNewGame.Text = "Create Game"
        btnNewGame.UseVisualStyleBackColor = True
        ' 
        ' btnLoadGame
        ' 
        btnLoadGame.Location = New Point(1381, 865)
        btnLoadGame.Name = "btnLoadGame"
        btnLoadGame.Size = New Size(119, 23)
        btnLoadGame.TabIndex = 11
        btnLoadGame.Text = "Load Game"
        btnLoadGame.UseVisualStyleBackColor = True
        ' 
        ' btnKillPlayer
        ' 
        btnKillPlayer.Location = New Point(1091, 913)
        btnKillPlayer.Name = "btnKillPlayer"
        btnKillPlayer.Size = New Size(150, 23)
        btnKillPlayer.TabIndex = 12
        btnKillPlayer.Text = "Kill Player"
        btnKillPlayer.UseVisualStyleBackColor = True
        ' 
        ' dgvCustomers
        ' 
        dgvCustomers.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        dgvCustomers.Location = New Point(795, 13)
        dgvCustomers.Name = "dgvCustomers"
        dgvCustomers.Size = New Size(787, 432)
        dgvCustomers.TabIndex = 13
        ' 
        ' btnSaveCustomers
        ' 
        btnSaveCustomers.Location = New Point(795, 451)
        btnSaveCustomers.Name = "btnSaveCustomers"
        btnSaveCustomers.Size = New Size(130, 23)
        btnSaveCustomers.TabIndex = 14
        btnSaveCustomers.Text = "Save Customers"
        btnSaveCustomers.UseVisualStyleBackColor = True
        ' 
        ' grp
        ' 
        grp.Controls.Add(btnApplyAssignments)
        grp.Controls.Add(cmbHuman)
        grp.Controls.Add(lblHuman)
        grp.Controls.Add(cmbOrc)
        grp.Controls.Add(lblOrc)
        grp.Controls.Add(cmbDwarf)
        grp.Controls.Add(lblDwarf)
        grp.Controls.Add(cmbElf)
        grp.Controls.Add(lblElf)
        grp.Location = New Point(1381, 495)
        grp.Name = "grp"
        grp.Size = New Size(312, 325)
        grp.TabIndex = 15
        grp.TabStop = False
        grp.Text = "Player Assignments"
        ' 
        ' btnApplyAssignments
        ' 
        btnApplyAssignments.Location = New Point(96, 193)
        btnApplyAssignments.Name = "btnApplyAssignments"
        btnApplyAssignments.Size = New Size(121, 23)
        btnApplyAssignments.TabIndex = 8
        btnApplyAssignments.Text = "Apply"
        btnApplyAssignments.UseVisualStyleBackColor = True
        ' 
        ' cmbHuman
        ' 
        cmbHuman.FormattingEnabled = True
        cmbHuman.Location = New Point(96, 147)
        cmbHuman.Name = "cmbHuman"
        cmbHuman.Size = New Size(121, 23)
        cmbHuman.TabIndex = 7
        ' 
        ' lblHuman
        ' 
        lblHuman.AutoSize = True
        lblHuman.Location = New Point(48, 150)
        lblHuman.Name = "lblHuman"
        lblHuman.Size = New Size(47, 15)
        lblHuman.TabIndex = 6
        lblHuman.Text = "Human"
        ' 
        ' cmbOrc
        ' 
        cmbOrc.FormattingEnabled = True
        cmbOrc.Location = New Point(96, 104)
        cmbOrc.Name = "cmbOrc"
        cmbOrc.Size = New Size(121, 23)
        cmbOrc.TabIndex = 5
        ' 
        ' lblOrc
        ' 
        lblOrc.AutoSize = True
        lblOrc.Location = New Point(48, 107)
        lblOrc.Name = "lblOrc"
        lblOrc.Size = New Size(26, 15)
        lblOrc.TabIndex = 4
        lblOrc.Text = "Orc"
        ' 
        ' cmbDwarf
        ' 
        cmbDwarf.FormattingEnabled = True
        cmbDwarf.Location = New Point(96, 62)
        cmbDwarf.Name = "cmbDwarf"
        cmbDwarf.Size = New Size(121, 23)
        cmbDwarf.TabIndex = 3
        ' 
        ' lblDwarf
        ' 
        lblDwarf.AutoSize = True
        lblDwarf.Location = New Point(48, 65)
        lblDwarf.Name = "lblDwarf"
        lblDwarf.Size = New Size(38, 15)
        lblDwarf.TabIndex = 2
        lblDwarf.Text = "Dwarf"
        ' 
        ' cmbElf
        ' 
        cmbElf.FormattingEnabled = True
        cmbElf.Location = New Point(96, 22)
        cmbElf.Name = "cmbElf"
        cmbElf.Size = New Size(121, 23)
        cmbElf.TabIndex = 1
        ' 
        ' lblElf
        ' 
        lblElf.AutoSize = True
        lblElf.Location = New Point(48, 25)
        lblElf.Name = "lblElf"
        lblElf.Size = New Size(20, 15)
        lblElf.TabIndex = 0
        lblElf.Text = "Elf"
        ' 
        ' btnSave
        ' 
        btnSave.Location = New Point(1381, 894)
        btnSave.Name = "btnSave"
        btnSave.Size = New Size(119, 22)
        btnSave.TabIndex = 16
        btnSave.Text = "Save Game"
        btnSave.UseVisualStyleBackColor = True
        ' 
        ' Form1
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1778, 971)
        Controls.Add(btnSave)
        Controls.Add(grp)
        Controls.Add(btnSaveCustomers)
        Controls.Add(dgvCustomers)
        Controls.Add(btnKillPlayer)
        Controls.Add(btnLoadGame)
        Controls.Add(btnNewGame)
        Controls.Add(lblHud)
        Controls.Add(rtbArmies)
        Controls.Add(rtbPlayerSummary)
        Controls.Add(rtbInfo)
        Controls.Add(btnProcessTurn)
        Controls.Add(pnlMap)
        Controls.Add(btnPrint)
        Name = "Form1"
        Text = "Form1"
        CType(dgvCustomers, ComponentModel.ISupportInitialize).EndInit()
        grp.ResumeLayout(False)
        grp.PerformLayout()
        ResumeLayout(False)
        PerformLayout()
    End Sub

    Friend WithEvents btnPrint As Button
    Friend WithEvents pnlMap As Panel
    Friend WithEvents btnProcessTurn As Button
    Friend WithEvents rtbInfo As RichTextBox
    Friend WithEvents rtbPlayerSummary As RichTextBox
    Friend WithEvents rtbArmies As RichTextBox
    Friend WithEvents lblHud As Label
    Friend WithEvents btnNewGame As Button
    Friend WithEvents btnLoadGame As Button
    Friend WithEvents btnKillPlayer As Button
    Friend WithEvents dgvCustomers As DataGridView
    Friend WithEvents btnSaveCustomers As Button
    Friend WithEvents grp As GroupBox
    Friend WithEvents btnApplyAssignments As Button
    Friend WithEvents cmbHuman As ComboBox
    Friend WithEvents lblHuman As Label
    Friend WithEvents cmbOrc As ComboBox
    Friend WithEvents lblOrc As Label
    Friend WithEvents cmbDwarf As ComboBox
    Friend WithEvents lblDwarf As Label
    Friend WithEvents cmbElf As ComboBox
    Friend WithEvents lblElf As Label
    Friend WithEvents btnSave As Button

End Class
