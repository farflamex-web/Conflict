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
        dgvOrders = New DataGridView()
        colPlayer = New DataGridViewTextBoxColumn()
        colArmy = New DataGridViewTextBoxColumn()
        Move1 = New DataGridViewComboBoxColumn()
        Move2 = New DataGridViewComboBoxColumn()
        Move3 = New DataGridViewComboBoxColumn()
        Move4 = New DataGridViewComboBoxColumn()
        Move5 = New DataGridViewComboBoxColumn()
        colRecruitUnit = New DataGridViewComboBoxColumn()
        colRecruitAmount = New DataGridViewTextBoxColumn()
        lblElf2 = New Label()
        chkBuySummonerElf = New CheckBox()
        cmbSummonerElf = New ComboBox()
        cmbArmyElf = New ComboBox()
        cmbArmyDwarf = New ComboBox()
        cmbSummonerDwarf = New ComboBox()
        chkBuySummonerDwarf = New CheckBox()
        lblDwarf2 = New Label()
        cmbArmyOrc = New ComboBox()
        cmbSummonerOrc = New ComboBox()
        chkBuySummonerOrc = New CheckBox()
        lblOrc2 = New Label()
        cmbArmyHuman = New ComboBox()
        cmbSummonerHuman = New ComboBox()
        chkBuySummonerHuman = New CheckBox()
        lblHuman2 = New Label()
        CType(dgvCustomers, ComponentModel.ISupportInitialize).BeginInit()
        grp.SuspendLayout()
        CType(dgvOrders, ComponentModel.ISupportInitialize).BeginInit()
        SuspendLayout()
        ' 
        ' btnPrint
        ' 
        btnPrint.Location = New Point(542, 869)
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
        btnProcessTurn.Location = New Point(412, 869)
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
        rtbPlayerSummary.Size = New Size(391, 408)
        rtbPlayerSummary.TabIndex = 7
        rtbPlayerSummary.Text = ""
        ' 
        ' rtbArmies
        ' 
        rtbArmies.Location = New Point(412, 451)
        rtbArmies.Name = "rtbArmies"
        rtbArmies.Size = New Size(369, 408)
        rtbArmies.TabIndex = 8
        rtbArmies.Text = ""
        ' 
        ' lblHud
        ' 
        lblHud.AutoSize = True
        lblHud.Location = New Point(15, 869)
        lblHud.Name = "lblHud"
        lblHud.Size = New Size(41, 15)
        lblHud.TabIndex = 9
        lblHud.Text = "Label1"
        ' 
        ' btnNewGame
        ' 
        btnNewGame.Location = New Point(412, 898)
        btnNewGame.Name = "btnNewGame"
        btnNewGame.Size = New Size(124, 23)
        btnNewGame.TabIndex = 10
        btnNewGame.Text = "Create Game"
        btnNewGame.UseVisualStyleBackColor = True
        ' 
        ' btnLoadGame
        ' 
        btnLoadGame.Location = New Point(542, 898)
        btnLoadGame.Name = "btnLoadGame"
        btnLoadGame.Size = New Size(124, 23)
        btnLoadGame.TabIndex = 11
        btnLoadGame.Text = "Load Game"
        btnLoadGame.UseVisualStyleBackColor = True
        ' 
        ' btnKillPlayer
        ' 
        btnKillPlayer.Location = New Point(672, 869)
        btnKillPlayer.Name = "btnKillPlayer"
        btnKillPlayer.Size = New Size(109, 23)
        btnKillPlayer.TabIndex = 12
        btnKillPlayer.Text = "Kill Player"
        btnKillPlayer.UseVisualStyleBackColor = True
        ' 
        ' dgvCustomers
        ' 
        dgvCustomers.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        dgvCustomers.Location = New Point(795, 13)
        dgvCustomers.Name = "dgvCustomers"
        dgvCustomers.Size = New Size(743, 432)
        dgvCustomers.TabIndex = 13
        ' 
        ' btnSaveCustomers
        ' 
        btnSaveCustomers.Location = New Point(1588, 13)
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
        grp.Location = New Point(1548, 64)
        grp.Name = "grp"
        grp.Size = New Size(199, 234)
        grp.TabIndex = 15
        grp.TabStop = False
        grp.Text = "Player Assignments"
        ' 
        ' btnApplyAssignments
        ' 
        btnApplyAssignments.Location = New Point(40, 193)
        btnApplyAssignments.Name = "btnApplyAssignments"
        btnApplyAssignments.Size = New Size(121, 23)
        btnApplyAssignments.TabIndex = 8
        btnApplyAssignments.Text = "Apply"
        btnApplyAssignments.UseVisualStyleBackColor = True
        ' 
        ' cmbHuman
        ' 
        cmbHuman.FormattingEnabled = True
        cmbHuman.Location = New Point(59, 147)
        cmbHuman.Name = "cmbHuman"
        cmbHuman.Size = New Size(121, 23)
        cmbHuman.TabIndex = 7
        ' 
        ' lblHuman
        ' 
        lblHuman.AutoSize = True
        lblHuman.Location = New Point(6, 150)
        lblHuman.Name = "lblHuman"
        lblHuman.Size = New Size(47, 15)
        lblHuman.TabIndex = 6
        lblHuman.Text = "Human"
        ' 
        ' cmbOrc
        ' 
        cmbOrc.FormattingEnabled = True
        cmbOrc.Location = New Point(59, 104)
        cmbOrc.Name = "cmbOrc"
        cmbOrc.Size = New Size(121, 23)
        cmbOrc.TabIndex = 5
        ' 
        ' lblOrc
        ' 
        lblOrc.AutoSize = True
        lblOrc.Location = New Point(6, 107)
        lblOrc.Name = "lblOrc"
        lblOrc.Size = New Size(26, 15)
        lblOrc.TabIndex = 4
        lblOrc.Text = "Orc"
        ' 
        ' cmbDwarf
        ' 
        cmbDwarf.FormattingEnabled = True
        cmbDwarf.Location = New Point(59, 62)
        cmbDwarf.Name = "cmbDwarf"
        cmbDwarf.Size = New Size(121, 23)
        cmbDwarf.TabIndex = 3
        ' 
        ' lblDwarf
        ' 
        lblDwarf.AutoSize = True
        lblDwarf.Location = New Point(6, 65)
        lblDwarf.Name = "lblDwarf"
        lblDwarf.Size = New Size(38, 15)
        lblDwarf.TabIndex = 2
        lblDwarf.Text = "Dwarf"
        ' 
        ' cmbElf
        ' 
        cmbElf.FormattingEnabled = True
        cmbElf.Location = New Point(59, 22)
        cmbElf.Name = "cmbElf"
        cmbElf.Size = New Size(121, 23)
        cmbElf.TabIndex = 1
        ' 
        ' lblElf
        ' 
        lblElf.AutoSize = True
        lblElf.Location = New Point(6, 25)
        lblElf.Name = "lblElf"
        lblElf.Size = New Size(20, 15)
        lblElf.TabIndex = 0
        lblElf.Text = "Elf"
        ' 
        ' btnSave
        ' 
        btnSave.Location = New Point(672, 898)
        btnSave.Name = "btnSave"
        btnSave.Size = New Size(109, 22)
        btnSave.TabIndex = 16
        btnSave.Text = "Save Game"
        btnSave.UseVisualStyleBackColor = True
        ' 
        ' dgvOrders
        ' 
        dgvOrders.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        dgvOrders.Columns.AddRange(New DataGridViewColumn() {colPlayer, colArmy, Move1, Move2, Move3, Move4, Move5, colRecruitUnit, colRecruitAmount})
        dgvOrders.Location = New Point(795, 455)
        dgvOrders.Name = "dgvOrders"
        dgvOrders.Size = New Size(940, 341)
        dgvOrders.TabIndex = 17
        ' 
        ' colPlayer
        ' 
        colPlayer.HeaderText = "Player"
        colPlayer.Name = "colPlayer"
        colPlayer.ReadOnly = True
        ' 
        ' colArmy
        ' 
        colArmy.HeaderText = "Army"
        colArmy.Name = "colArmy"
        ' 
        ' Move1
        ' 
        Move1.HeaderText = "Move1"
        Move1.Name = "Move1"
        Move1.Resizable = DataGridViewTriState.True
        Move1.SortMode = DataGridViewColumnSortMode.Automatic
        ' 
        ' Move2
        ' 
        Move2.HeaderText = "Move2"
        Move2.Name = "Move2"
        Move2.Resizable = DataGridViewTriState.True
        Move2.SortMode = DataGridViewColumnSortMode.Automatic
        ' 
        ' Move3
        ' 
        Move3.HeaderText = "Move3"
        Move3.Name = "Move3"
        Move3.Resizable = DataGridViewTriState.True
        Move3.SortMode = DataGridViewColumnSortMode.Automatic
        ' 
        ' Move4
        ' 
        Move4.HeaderText = "Move4"
        Move4.Name = "Move4"
        Move4.Resizable = DataGridViewTriState.True
        Move4.SortMode = DataGridViewColumnSortMode.Automatic
        ' 
        ' Move5
        ' 
        Move5.HeaderText = "Move5"
        Move5.Name = "Move5"
        Move5.Resizable = DataGridViewTriState.True
        Move5.SortMode = DataGridViewColumnSortMode.Automatic
        ' 
        ' colRecruitUnit
        ' 
        colRecruitUnit.HeaderText = "Unit Type"
        colRecruitUnit.Name = "colRecruitUnit"
        ' 
        ' colRecruitAmount
        ' 
        colRecruitAmount.HeaderText = "Amount"
        colRecruitAmount.Name = "colRecruitAmount"
        ' 
        ' lblElf2
        ' 
        lblElf2.AutoSize = True
        lblElf2.Location = New Point(795, 824)
        lblElf2.Name = "lblElf2"
        lblElf2.Size = New Size(20, 15)
        lblElf2.TabIndex = 18
        lblElf2.Text = "Elf"
        ' 
        ' chkBuySummonerElf
        ' 
        chkBuySummonerElf.AutoSize = True
        chkBuySummonerElf.Location = New Point(836, 823)
        chkBuySummonerElf.Name = "chkBuySummonerElf"
        chkBuySummonerElf.Size = New Size(108, 19)
        chkBuySummonerElf.TabIndex = 19
        chkBuySummonerElf.Text = "Buy Summoner"
        chkBuySummonerElf.UseVisualStyleBackColor = True
        ' 
        ' cmbSummonerElf
        ' 
        cmbSummonerElf.DropDownStyle = ComboBoxStyle.DropDownList
        cmbSummonerElf.FormattingEnabled = True
        cmbSummonerElf.Location = New Point(950, 819)
        cmbSummonerElf.Name = "cmbSummonerElf"
        cmbSummonerElf.Size = New Size(121, 23)
        cmbSummonerElf.TabIndex = 20
        ' 
        ' cmbArmyElf
        ' 
        cmbArmyElf.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyElf.FormattingEnabled = True
        cmbArmyElf.Location = New Point(1077, 819)
        cmbArmyElf.Name = "cmbArmyElf"
        cmbArmyElf.Size = New Size(121, 23)
        cmbArmyElf.TabIndex = 21
        ' 
        ' cmbArmyDwarf
        ' 
        cmbArmyDwarf.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyDwarf.FormattingEnabled = True
        cmbArmyDwarf.Location = New Point(1077, 844)
        cmbArmyDwarf.Name = "cmbArmyDwarf"
        cmbArmyDwarf.Size = New Size(121, 23)
        cmbArmyDwarf.TabIndex = 25
        ' 
        ' cmbSummonerDwarf
        ' 
        cmbSummonerDwarf.DropDownStyle = ComboBoxStyle.DropDownList
        cmbSummonerDwarf.FormattingEnabled = True
        cmbSummonerDwarf.Location = New Point(950, 844)
        cmbSummonerDwarf.Name = "cmbSummonerDwarf"
        cmbSummonerDwarf.Size = New Size(121, 23)
        cmbSummonerDwarf.TabIndex = 24
        ' 
        ' chkBuySummonerDwarf
        ' 
        chkBuySummonerDwarf.AutoSize = True
        chkBuySummonerDwarf.Location = New Point(836, 848)
        chkBuySummonerDwarf.Name = "chkBuySummonerDwarf"
        chkBuySummonerDwarf.Size = New Size(108, 19)
        chkBuySummonerDwarf.TabIndex = 23
        chkBuySummonerDwarf.Text = "Buy Summoner"
        chkBuySummonerDwarf.UseVisualStyleBackColor = True
        ' 
        ' lblDwarf2
        ' 
        lblDwarf2.AutoSize = True
        lblDwarf2.Location = New Point(795, 849)
        lblDwarf2.Name = "lblDwarf2"
        lblDwarf2.Size = New Size(38, 15)
        lblDwarf2.TabIndex = 22
        lblDwarf2.Text = "Dwarf"
        ' 
        ' cmbArmyOrc
        ' 
        cmbArmyOrc.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyOrc.FormattingEnabled = True
        cmbArmyOrc.Location = New Point(1077, 869)
        cmbArmyOrc.Name = "cmbArmyOrc"
        cmbArmyOrc.Size = New Size(121, 23)
        cmbArmyOrc.TabIndex = 29
        ' 
        ' cmbSummonerOrc
        ' 
        cmbSummonerOrc.DropDownStyle = ComboBoxStyle.DropDownList
        cmbSummonerOrc.FormattingEnabled = True
        cmbSummonerOrc.Location = New Point(950, 869)
        cmbSummonerOrc.Name = "cmbSummonerOrc"
        cmbSummonerOrc.Size = New Size(121, 23)
        cmbSummonerOrc.TabIndex = 28
        ' 
        ' chkBuySummonerOrc
        ' 
        chkBuySummonerOrc.AutoSize = True
        chkBuySummonerOrc.Location = New Point(836, 873)
        chkBuySummonerOrc.Name = "chkBuySummonerOrc"
        chkBuySummonerOrc.Size = New Size(108, 19)
        chkBuySummonerOrc.TabIndex = 27
        chkBuySummonerOrc.Text = "Buy Summoner"
        chkBuySummonerOrc.UseVisualStyleBackColor = True
        ' 
        ' lblOrc2
        ' 
        lblOrc2.AutoSize = True
        lblOrc2.Location = New Point(795, 874)
        lblOrc2.Name = "lblOrc2"
        lblOrc2.Size = New Size(26, 15)
        lblOrc2.TabIndex = 26
        lblOrc2.Text = "Orc"
        ' 
        ' cmbArmyHuman
        ' 
        cmbArmyHuman.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyHuman.FormattingEnabled = True
        cmbArmyHuman.Location = New Point(1077, 894)
        cmbArmyHuman.Name = "cmbArmyHuman"
        cmbArmyHuman.Size = New Size(121, 23)
        cmbArmyHuman.TabIndex = 33
        ' 
        ' cmbSummonerHuman
        ' 
        cmbSummonerHuman.DropDownStyle = ComboBoxStyle.DropDownList
        cmbSummonerHuman.FormattingEnabled = True
        cmbSummonerHuman.Location = New Point(950, 894)
        cmbSummonerHuman.Name = "cmbSummonerHuman"
        cmbSummonerHuman.Size = New Size(121, 23)
        cmbSummonerHuman.TabIndex = 32
        ' 
        ' chkBuySummonerHuman
        ' 
        chkBuySummonerHuman.AutoSize = True
        chkBuySummonerHuman.Location = New Point(836, 898)
        chkBuySummonerHuman.Name = "chkBuySummonerHuman"
        chkBuySummonerHuman.Size = New Size(108, 19)
        chkBuySummonerHuman.TabIndex = 31
        chkBuySummonerHuman.Text = "Buy Summoner"
        chkBuySummonerHuman.UseVisualStyleBackColor = True
        ' 
        ' lblHuman2
        ' 
        lblHuman2.AutoSize = True
        lblHuman2.Location = New Point(795, 899)
        lblHuman2.Name = "lblHuman2"
        lblHuman2.Size = New Size(47, 15)
        lblHuman2.TabIndex = 30
        lblHuman2.Text = "Human"
        ' 
        ' Form1
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1778, 971)
        Controls.Add(cmbArmyHuman)
        Controls.Add(cmbSummonerHuman)
        Controls.Add(chkBuySummonerHuman)
        Controls.Add(lblHuman2)
        Controls.Add(cmbArmyOrc)
        Controls.Add(cmbSummonerOrc)
        Controls.Add(chkBuySummonerOrc)
        Controls.Add(lblOrc2)
        Controls.Add(cmbArmyDwarf)
        Controls.Add(cmbSummonerDwarf)
        Controls.Add(chkBuySummonerDwarf)
        Controls.Add(lblDwarf2)
        Controls.Add(cmbArmyElf)
        Controls.Add(cmbSummonerElf)
        Controls.Add(chkBuySummonerElf)
        Controls.Add(lblElf2)
        Controls.Add(dgvOrders)
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
        CType(dgvOrders, ComponentModel.ISupportInitialize).EndInit()
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
    Friend WithEvents dgvOrders As DataGridView
    Friend WithEvents colPlayer As DataGridViewTextBoxColumn
    Friend WithEvents colArmy As DataGridViewTextBoxColumn
    Friend WithEvents Move1 As DataGridViewComboBoxColumn
    Friend WithEvents Move2 As DataGridViewComboBoxColumn
    Friend WithEvents Move3 As DataGridViewComboBoxColumn
    Friend WithEvents Move4 As DataGridViewComboBoxColumn
    Friend WithEvents Move5 As DataGridViewComboBoxColumn
    Friend WithEvents colRecruitUnit As DataGridViewComboBoxColumn
    Friend WithEvents colRecruitAmount As DataGridViewTextBoxColumn
    Friend WithEvents lblElf2 As Label
    Friend WithEvents chkBuySummonerElf As CheckBox
    Friend WithEvents cmbSummonerElf As ComboBox
    Friend WithEvents cmbArmyElf As ComboBox
    Friend WithEvents cmbArmyDwarf As ComboBox
    Friend WithEvents cmbSummonerDwarf As ComboBox
    Friend WithEvents chkBuySummonerDwarf As CheckBox
    Friend WithEvents lblDwarf2 As Label
    Friend WithEvents cmbArmyOrc As ComboBox
    Friend WithEvents cmbSummonerOrc As ComboBox
    Friend WithEvents chkBuySummonerOrc As CheckBox
    Friend WithEvents lblOrc2 As Label
    Friend WithEvents cmbArmyHuman As ComboBox
    Friend WithEvents cmbSummonerHuman As ComboBox
    Friend WithEvents chkBuySummonerHuman As CheckBox
    Friend WithEvents lblHuman2 As Label

End Class
