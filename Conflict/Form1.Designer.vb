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
        lblMercenaryBids = New Label()
        numMercBidElf = New NumericUpDown()
        numMercBidDwarf = New NumericUpDown()
        numMercBidOrc = New NumericUpDown()
        numMercBidHuman = New NumericUpDown()
        cmbArmyElfMerc = New ComboBox()
        cmbArmyDwarfMerc = New ComboBox()
        cmbArmyOrcMerc = New ComboBox()
        cmbArmyHumanMerc = New ComboBox()
        lblInvestments = New Label()
        numInvestElf = New NumericUpDown()
        numInvestDwarf = New NumericUpDown()
        numInvestOrc = New NumericUpDown()
        numInvestHuman = New NumericUpDown()
        lblBuySummoners = New Label()
        cmbMarketResourceElf = New ComboBox()
        numMarketAmountElf = New NumericUpDown()
        numMarketAmountDwarf = New NumericUpDown()
        cmbMarketResourceDwarf = New ComboBox()
        numMarketAmountHuman = New NumericUpDown()
        cmbMarketResourceHuman = New ComboBox()
        numMarketAmountOrc = New NumericUpDown()
        cmbMarketResourceOrc = New ComboBox()
        lblMarket = New Label()
        cmbGameSelect = New ComboBox()
        cmbTurn = New ComboBox()
        CType(dgvCustomers, ComponentModel.ISupportInitialize).BeginInit()
        grp.SuspendLayout()
        CType(dgvOrders, ComponentModel.ISupportInitialize).BeginInit()
        CType(numMercBidElf, ComponentModel.ISupportInitialize).BeginInit()
        CType(numMercBidDwarf, ComponentModel.ISupportInitialize).BeginInit()
        CType(numMercBidOrc, ComponentModel.ISupportInitialize).BeginInit()
        CType(numMercBidHuman, ComponentModel.ISupportInitialize).BeginInit()
        CType(numInvestElf, ComponentModel.ISupportInitialize).BeginInit()
        CType(numInvestDwarf, ComponentModel.ISupportInitialize).BeginInit()
        CType(numInvestOrc, ComponentModel.ISupportInitialize).BeginInit()
        CType(numInvestHuman, ComponentModel.ISupportInitialize).BeginInit()
        CType(numMarketAmountElf, ComponentModel.ISupportInitialize).BeginInit()
        CType(numMarketAmountDwarf, ComponentModel.ISupportInitialize).BeginInit()
        CType(numMarketAmountHuman, ComponentModel.ISupportInitialize).BeginInit()
        CType(numMarketAmountOrc, ComponentModel.ISupportInitialize).BeginInit()
        SuspendLayout()
        ' 
        ' btnPrint
        ' 
        btnPrint.Location = New Point(412, 898)
        btnPrint.Name = "btnPrint"
        btnPrint.Size = New Size(109, 23)
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
        btnProcessTurn.Size = New Size(109, 23)
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
        btnNewGame.Location = New Point(412, 927)
        btnNewGame.Name = "btnNewGame"
        btnNewGame.Size = New Size(109, 23)
        btnNewGame.TabIndex = 10
        btnNewGame.Text = "Create Game"
        btnNewGame.UseVisualStyleBackColor = True
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
        lblElf2.Location = New Point(790, 822)
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
        chkBuySummonerElf.Size = New Size(15, 14)
        chkBuySummonerElf.TabIndex = 19
        chkBuySummonerElf.UseVisualStyleBackColor = True
        ' 
        ' cmbSummonerElf
        ' 
        cmbSummonerElf.DropDownStyle = ComboBoxStyle.DropDownList
        cmbSummonerElf.FormattingEnabled = True
        cmbSummonerElf.Location = New Point(857, 816)
        cmbSummonerElf.Name = "cmbSummonerElf"
        cmbSummonerElf.Size = New Size(104, 23)
        cmbSummonerElf.TabIndex = 20
        ' 
        ' cmbArmyElf
        ' 
        cmbArmyElf.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyElf.FormattingEnabled = True
        cmbArmyElf.Location = New Point(967, 815)
        cmbArmyElf.Name = "cmbArmyElf"
        cmbArmyElf.Size = New Size(104, 23)
        cmbArmyElf.TabIndex = 21
        ' 
        ' cmbArmyDwarf
        ' 
        cmbArmyDwarf.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyDwarf.FormattingEnabled = True
        cmbArmyDwarf.Location = New Point(967, 840)
        cmbArmyDwarf.Name = "cmbArmyDwarf"
        cmbArmyDwarf.Size = New Size(104, 23)
        cmbArmyDwarf.TabIndex = 25
        ' 
        ' cmbSummonerDwarf
        ' 
        cmbSummonerDwarf.DropDownStyle = ComboBoxStyle.DropDownList
        cmbSummonerDwarf.FormattingEnabled = True
        cmbSummonerDwarf.Location = New Point(857, 841)
        cmbSummonerDwarf.Name = "cmbSummonerDwarf"
        cmbSummonerDwarf.Size = New Size(104, 23)
        cmbSummonerDwarf.TabIndex = 24
        ' 
        ' chkBuySummonerDwarf
        ' 
        chkBuySummonerDwarf.AutoSize = True
        chkBuySummonerDwarf.Location = New Point(836, 848)
        chkBuySummonerDwarf.Name = "chkBuySummonerDwarf"
        chkBuySummonerDwarf.Size = New Size(15, 14)
        chkBuySummonerDwarf.TabIndex = 23
        chkBuySummonerDwarf.UseVisualStyleBackColor = True
        ' 
        ' lblDwarf2
        ' 
        lblDwarf2.AutoSize = True
        lblDwarf2.Location = New Point(790, 847)
        lblDwarf2.Name = "lblDwarf2"
        lblDwarf2.Size = New Size(38, 15)
        lblDwarf2.TabIndex = 22
        lblDwarf2.Text = "Dwarf"
        ' 
        ' cmbArmyOrc
        ' 
        cmbArmyOrc.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyOrc.FormattingEnabled = True
        cmbArmyOrc.Location = New Point(967, 865)
        cmbArmyOrc.Name = "cmbArmyOrc"
        cmbArmyOrc.Size = New Size(104, 23)
        cmbArmyOrc.TabIndex = 29
        ' 
        ' cmbSummonerOrc
        ' 
        cmbSummonerOrc.DropDownStyle = ComboBoxStyle.DropDownList
        cmbSummonerOrc.FormattingEnabled = True
        cmbSummonerOrc.Location = New Point(857, 866)
        cmbSummonerOrc.Name = "cmbSummonerOrc"
        cmbSummonerOrc.Size = New Size(104, 23)
        cmbSummonerOrc.TabIndex = 28
        ' 
        ' chkBuySummonerOrc
        ' 
        chkBuySummonerOrc.AutoSize = True
        chkBuySummonerOrc.Location = New Point(836, 873)
        chkBuySummonerOrc.Name = "chkBuySummonerOrc"
        chkBuySummonerOrc.Size = New Size(15, 14)
        chkBuySummonerOrc.TabIndex = 27
        chkBuySummonerOrc.UseVisualStyleBackColor = True
        ' 
        ' lblOrc2
        ' 
        lblOrc2.AutoSize = True
        lblOrc2.Location = New Point(790, 872)
        lblOrc2.Name = "lblOrc2"
        lblOrc2.Size = New Size(26, 15)
        lblOrc2.TabIndex = 26
        lblOrc2.Text = "Orc"
        ' 
        ' cmbArmyHuman
        ' 
        cmbArmyHuman.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyHuman.FormattingEnabled = True
        cmbArmyHuman.Location = New Point(967, 890)
        cmbArmyHuman.Name = "cmbArmyHuman"
        cmbArmyHuman.Size = New Size(104, 23)
        cmbArmyHuman.TabIndex = 33
        ' 
        ' cmbSummonerHuman
        ' 
        cmbSummonerHuman.DropDownStyle = ComboBoxStyle.DropDownList
        cmbSummonerHuman.FormattingEnabled = True
        cmbSummonerHuman.Location = New Point(857, 891)
        cmbSummonerHuman.Name = "cmbSummonerHuman"
        cmbSummonerHuman.Size = New Size(104, 23)
        cmbSummonerHuman.TabIndex = 32
        ' 
        ' chkBuySummonerHuman
        ' 
        chkBuySummonerHuman.AutoSize = True
        chkBuySummonerHuman.Location = New Point(836, 898)
        chkBuySummonerHuman.Name = "chkBuySummonerHuman"
        chkBuySummonerHuman.Size = New Size(15, 14)
        chkBuySummonerHuman.TabIndex = 31
        chkBuySummonerHuman.UseVisualStyleBackColor = True
        ' 
        ' lblHuman2
        ' 
        lblHuman2.AutoSize = True
        lblHuman2.Location = New Point(790, 897)
        lblHuman2.Name = "lblHuman2"
        lblHuman2.Size = New Size(47, 15)
        lblHuman2.TabIndex = 30
        lblHuman2.Text = "Human"
        ' 
        ' lblMercenaryBids
        ' 
        lblMercenaryBids.AutoSize = True
        lblMercenaryBids.Location = New Point(1137, 799)
        lblMercenaryBids.Name = "lblMercenaryBids"
        lblMercenaryBids.Size = New Size(88, 15)
        lblMercenaryBids.TabIndex = 34
        lblMercenaryBids.Text = "Mercenary Bids"
        ' 
        ' numMercBidElf
        ' 
        numMercBidElf.Increment = New Decimal(New Integer() {100, 0, 0, 0})
        numMercBidElf.Location = New Point(1086, 815)
        numMercBidElf.Maximum = New Decimal(New Integer() {1410065407, 2, 0, 0})
        numMercBidElf.Name = "numMercBidElf"
        numMercBidElf.Size = New Size(93, 23)
        numMercBidElf.TabIndex = 35
        numMercBidElf.TextAlign = HorizontalAlignment.Right
        numMercBidElf.ThousandsSeparator = True
        ' 
        ' numMercBidDwarf
        ' 
        numMercBidDwarf.Increment = New Decimal(New Integer() {100, 0, 0, 0})
        numMercBidDwarf.Location = New Point(1086, 840)
        numMercBidDwarf.Maximum = New Decimal(New Integer() {1410065407, 2, 0, 0})
        numMercBidDwarf.Name = "numMercBidDwarf"
        numMercBidDwarf.Size = New Size(93, 23)
        numMercBidDwarf.TabIndex = 36
        numMercBidDwarf.TextAlign = HorizontalAlignment.Right
        numMercBidDwarf.ThousandsSeparator = True
        ' 
        ' numMercBidOrc
        ' 
        numMercBidOrc.Increment = New Decimal(New Integer() {100, 0, 0, 0})
        numMercBidOrc.Location = New Point(1086, 864)
        numMercBidOrc.Maximum = New Decimal(New Integer() {1410065407, 2, 0, 0})
        numMercBidOrc.Name = "numMercBidOrc"
        numMercBidOrc.Size = New Size(93, 23)
        numMercBidOrc.TabIndex = 37
        numMercBidOrc.TextAlign = HorizontalAlignment.Right
        numMercBidOrc.ThousandsSeparator = True
        ' 
        ' numMercBidHuman
        ' 
        numMercBidHuman.Increment = New Decimal(New Integer() {100, 0, 0, 0})
        numMercBidHuman.Location = New Point(1086, 890)
        numMercBidHuman.Maximum = New Decimal(New Integer() {1410065407, 2, 0, 0})
        numMercBidHuman.Name = "numMercBidHuman"
        numMercBidHuman.Size = New Size(93, 23)
        numMercBidHuman.TabIndex = 38
        numMercBidHuman.TextAlign = HorizontalAlignment.Right
        numMercBidHuman.ThousandsSeparator = True
        ' 
        ' cmbArmyElfMerc
        ' 
        cmbArmyElfMerc.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyElfMerc.FormattingEnabled = True
        cmbArmyElfMerc.Location = New Point(1185, 815)
        cmbArmyElfMerc.Name = "cmbArmyElfMerc"
        cmbArmyElfMerc.Size = New Size(87, 23)
        cmbArmyElfMerc.TabIndex = 39
        ' 
        ' cmbArmyDwarfMerc
        ' 
        cmbArmyDwarfMerc.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyDwarfMerc.FormattingEnabled = True
        cmbArmyDwarfMerc.Location = New Point(1185, 839)
        cmbArmyDwarfMerc.Name = "cmbArmyDwarfMerc"
        cmbArmyDwarfMerc.Size = New Size(87, 23)
        cmbArmyDwarfMerc.TabIndex = 40
        ' 
        ' cmbArmyOrcMerc
        ' 
        cmbArmyOrcMerc.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyOrcMerc.FormattingEnabled = True
        cmbArmyOrcMerc.Location = New Point(1185, 864)
        cmbArmyOrcMerc.Name = "cmbArmyOrcMerc"
        cmbArmyOrcMerc.Size = New Size(87, 23)
        cmbArmyOrcMerc.TabIndex = 41
        ' 
        ' cmbArmyHumanMerc
        ' 
        cmbArmyHumanMerc.DropDownStyle = ComboBoxStyle.DropDownList
        cmbArmyHumanMerc.FormattingEnabled = True
        cmbArmyHumanMerc.Location = New Point(1185, 890)
        cmbArmyHumanMerc.Name = "cmbArmyHumanMerc"
        cmbArmyHumanMerc.Size = New Size(87, 23)
        cmbArmyHumanMerc.TabIndex = 42
        ' 
        ' lblInvestments
        ' 
        lblInvestments.AutoSize = True
        lblInvestments.Location = New Point(1287, 799)
        lblInvestments.Name = "lblInvestments"
        lblInvestments.Size = New Size(71, 15)
        lblInvestments.TabIndex = 43
        lblInvestments.Text = "Investments"
        ' 
        ' numInvestElf
        ' 
        numInvestElf.Increment = New Decimal(New Integer() {1000, 0, 0, 0})
        numInvestElf.Location = New Point(1278, 815)
        numInvestElf.Maximum = New Decimal(New Integer() {1000000, 0, 0, 0})
        numInvestElf.Name = "numInvestElf"
        numInvestElf.Size = New Size(93, 23)
        numInvestElf.TabIndex = 44
        numInvestElf.TextAlign = HorizontalAlignment.Right
        numInvestElf.ThousandsSeparator = True
        ' 
        ' numInvestDwarf
        ' 
        numInvestDwarf.Increment = New Decimal(New Integer() {1000, 0, 0, 0})
        numInvestDwarf.Location = New Point(1278, 839)
        numInvestDwarf.Maximum = New Decimal(New Integer() {1000000, 0, 0, 0})
        numInvestDwarf.Name = "numInvestDwarf"
        numInvestDwarf.Size = New Size(93, 23)
        numInvestDwarf.TabIndex = 45
        numInvestDwarf.TextAlign = HorizontalAlignment.Right
        numInvestDwarf.ThousandsSeparator = True
        ' 
        ' numInvestOrc
        ' 
        numInvestOrc.Increment = New Decimal(New Integer() {1000, 0, 0, 0})
        numInvestOrc.Location = New Point(1278, 864)
        numInvestOrc.Maximum = New Decimal(New Integer() {1000000, 0, 0, 0})
        numInvestOrc.Name = "numInvestOrc"
        numInvestOrc.Size = New Size(93, 23)
        numInvestOrc.TabIndex = 46
        numInvestOrc.TextAlign = HorizontalAlignment.Right
        numInvestOrc.ThousandsSeparator = True
        ' 
        ' numInvestHuman
        ' 
        numInvestHuman.Increment = New Decimal(New Integer() {1000, 0, 0, 0})
        numInvestHuman.Location = New Point(1278, 891)
        numInvestHuman.Maximum = New Decimal(New Integer() {1000000, 0, 0, 0})
        numInvestHuman.Name = "numInvestHuman"
        numInvestHuman.Size = New Size(93, 23)
        numInvestHuman.TabIndex = 47
        numInvestHuman.TextAlign = HorizontalAlignment.Right
        numInvestHuman.ThousandsSeparator = True
        ' 
        ' lblBuySummoners
        ' 
        lblBuySummoners.AutoSize = True
        lblBuySummoners.Location = New Point(915, 799)
        lblBuySummoners.Name = "lblBuySummoners"
        lblBuySummoners.Size = New Size(94, 15)
        lblBuySummoners.TabIndex = 49
        lblBuySummoners.Text = "Buy Summoners"
        ' 
        ' cmbMarketResourceElf
        ' 
        cmbMarketResourceElf.FormattingEnabled = True
        cmbMarketResourceElf.Location = New Point(1386, 815)
        cmbMarketResourceElf.Name = "cmbMarketResourceElf"
        cmbMarketResourceElf.Size = New Size(93, 23)
        cmbMarketResourceElf.TabIndex = 50
        ' 
        ' numMarketAmountElf
        ' 
        numMarketAmountElf.Location = New Point(1485, 815)
        numMarketAmountElf.Minimum = New Decimal(New Integer() {9999999, 0, 0, Integer.MinValue})
        numMarketAmountElf.Name = "numMarketAmountElf"
        numMarketAmountElf.Size = New Size(93, 23)
        numMarketAmountElf.TabIndex = 51
        ' 
        ' numMarketAmountDwarf
        ' 
        numMarketAmountDwarf.Location = New Point(1485, 839)
        numMarketAmountDwarf.Minimum = New Decimal(New Integer() {9999999, 0, 0, Integer.MinValue})
        numMarketAmountDwarf.Name = "numMarketAmountDwarf"
        numMarketAmountDwarf.Size = New Size(93, 23)
        numMarketAmountDwarf.TabIndex = 53
        ' 
        ' cmbMarketResourceDwarf
        ' 
        cmbMarketResourceDwarf.FormattingEnabled = True
        cmbMarketResourceDwarf.Location = New Point(1386, 839)
        cmbMarketResourceDwarf.Name = "cmbMarketResourceDwarf"
        cmbMarketResourceDwarf.Size = New Size(93, 23)
        cmbMarketResourceDwarf.TabIndex = 52
        ' 
        ' numMarketAmountHuman
        ' 
        numMarketAmountHuman.Location = New Point(1485, 891)
        numMarketAmountHuman.Minimum = New Decimal(New Integer() {9999999, 0, 0, Integer.MinValue})
        numMarketAmountHuman.Name = "numMarketAmountHuman"
        numMarketAmountHuman.Size = New Size(93, 23)
        numMarketAmountHuman.TabIndex = 57
        ' 
        ' cmbMarketResourceHuman
        ' 
        cmbMarketResourceHuman.FormattingEnabled = True
        cmbMarketResourceHuman.Location = New Point(1386, 891)
        cmbMarketResourceHuman.Name = "cmbMarketResourceHuman"
        cmbMarketResourceHuman.Size = New Size(93, 23)
        cmbMarketResourceHuman.TabIndex = 56
        ' 
        ' numMarketAmountOrc
        ' 
        numMarketAmountOrc.Location = New Point(1485, 864)
        numMarketAmountOrc.Minimum = New Decimal(New Integer() {9999999, 0, 0, Integer.MinValue})
        numMarketAmountOrc.Name = "numMarketAmountOrc"
        numMarketAmountOrc.Size = New Size(93, 23)
        numMarketAmountOrc.TabIndex = 55
        ' 
        ' cmbMarketResourceOrc
        ' 
        cmbMarketResourceOrc.FormattingEnabled = True
        cmbMarketResourceOrc.Location = New Point(1386, 864)
        cmbMarketResourceOrc.Name = "cmbMarketResourceOrc"
        cmbMarketResourceOrc.Size = New Size(93, 23)
        cmbMarketResourceOrc.TabIndex = 54
        ' 
        ' lblMarket
        ' 
        lblMarket.AutoSize = True
        lblMarket.Location = New Point(1426, 798)
        lblMarket.Name = "lblMarket"
        lblMarket.Size = New Size(112, 15)
        lblMarket.TabIndex = 58
        lblMarket.Text = "Market Transactions"
        ' 
        ' cmbGameSelect
        ' 
        cmbGameSelect.FormattingEnabled = True
        cmbGameSelect.Location = New Point(672, 873)
        cmbGameSelect.Name = "cmbGameSelect"
        cmbGameSelect.Size = New Size(109, 23)
        cmbGameSelect.TabIndex = 59
        ' 
        ' cmbTurn
        ' 
        cmbTurn.FormattingEnabled = True
        cmbTurn.Location = New Point(672, 902)
        cmbTurn.Name = "cmbTurn"
        cmbTurn.Size = New Size(109, 23)
        cmbTurn.TabIndex = 60
        ' 
        ' Form1
        ' 
        AutoScaleDimensions = New SizeF(7F, 15F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(1778, 971)
        Controls.Add(cmbTurn)
        Controls.Add(cmbGameSelect)
        Controls.Add(lblMarket)
        Controls.Add(numMarketAmountHuman)
        Controls.Add(cmbMarketResourceHuman)
        Controls.Add(numMarketAmountOrc)
        Controls.Add(cmbMarketResourceOrc)
        Controls.Add(numMarketAmountDwarf)
        Controls.Add(cmbMarketResourceDwarf)
        Controls.Add(numMarketAmountElf)
        Controls.Add(cmbMarketResourceElf)
        Controls.Add(lblBuySummoners)
        Controls.Add(numInvestHuman)
        Controls.Add(numInvestOrc)
        Controls.Add(numInvestDwarf)
        Controls.Add(numInvestElf)
        Controls.Add(lblInvestments)
        Controls.Add(cmbArmyHumanMerc)
        Controls.Add(cmbArmyOrcMerc)
        Controls.Add(cmbArmyDwarfMerc)
        Controls.Add(cmbArmyElfMerc)
        Controls.Add(numMercBidHuman)
        Controls.Add(numMercBidOrc)
        Controls.Add(numMercBidDwarf)
        Controls.Add(numMercBidElf)
        Controls.Add(lblMercenaryBids)
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
        Controls.Add(grp)
        Controls.Add(btnSaveCustomers)
        Controls.Add(dgvCustomers)
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
        CType(numMercBidElf, ComponentModel.ISupportInitialize).EndInit()
        CType(numMercBidDwarf, ComponentModel.ISupportInitialize).EndInit()
        CType(numMercBidOrc, ComponentModel.ISupportInitialize).EndInit()
        CType(numMercBidHuman, ComponentModel.ISupportInitialize).EndInit()
        CType(numInvestElf, ComponentModel.ISupportInitialize).EndInit()
        CType(numInvestDwarf, ComponentModel.ISupportInitialize).EndInit()
        CType(numInvestOrc, ComponentModel.ISupportInitialize).EndInit()
        CType(numInvestHuman, ComponentModel.ISupportInitialize).EndInit()
        CType(numMarketAmountElf, ComponentModel.ISupportInitialize).EndInit()
        CType(numMarketAmountDwarf, ComponentModel.ISupportInitialize).EndInit()
        CType(numMarketAmountHuman, ComponentModel.ISupportInitialize).EndInit()
        CType(numMarketAmountOrc, ComponentModel.ISupportInitialize).EndInit()
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
    Friend WithEvents lblMercenaryBids As Label
    Friend WithEvents numMercBidElf As NumericUpDown
    Friend WithEvents numMercBidDwarf As NumericUpDown
    Friend WithEvents numMercBidOrc As NumericUpDown
    Friend WithEvents numMercBidHuman As NumericUpDown
    Friend WithEvents cmbArmyElfMerc As ComboBox
    Friend WithEvents cmbArmyDwarfMerc As ComboBox
    Friend WithEvents cmbArmyOrcMerc As ComboBox
    Friend WithEvents cmbArmyHumanMerc As ComboBox
    Friend WithEvents lblInvestments As Label
    Friend WithEvents numInvestElf As NumericUpDown
    Friend WithEvents numInvestDwarf As NumericUpDown
    Friend WithEvents numInvestOrc As NumericUpDown
    Friend WithEvents numInvestHuman As NumericUpDown
    Friend WithEvents lblBuySummoners As Label
    Friend WithEvents cmbMarketResourceElf As ComboBox
    Friend WithEvents numMarketAmountElf As NumericUpDown
    Friend WithEvents numMarketAmountDwarf As NumericUpDown
    Friend WithEvents cmbMarketResourceDwarf As ComboBox
    Friend WithEvents numMarketAmountHuman As NumericUpDown
    Friend WithEvents cmbMarketResourceHuman As ComboBox
    Friend WithEvents numMarketAmountOrc As NumericUpDown
    Friend WithEvents cmbMarketResourceOrc As ComboBox
    Friend WithEvents lblMarket As Label
    Friend WithEvents cmbGameSelect As ComboBox
    Friend WithEvents cmbTurn As ComboBox

End Class
