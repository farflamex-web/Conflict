' ============================================================
'  UIModule.vb
'  Contains all UI-related shared procedures, such as combo setup,
'  form resets, numeric box restrictions, and display refreshes.
' ============================================================

Imports System.Drawing
Imports System.Windows.Forms
Imports Conflict.Form1

Module UIModule
    ' Reference to main game form (set once in Form1_Load)
    Public CurrentForm As Form1

    ' === MAX helpers for Amount column in dgvOrders ===
    Private Const AmountColumnName As String = "colRecruitAmount"
    Private maxHandlersWired As Boolean = False
    Private amountEditor As TextBox ' active editor textbox for Amount cell
    Private isEditingOrders As Boolean = False

    Public Sub WireOrdersGridMaxShortcuts()
        If CurrentForm Is Nothing OrElse CurrentForm.dgvOrders Is Nothing Then Exit Sub

        ' Always reattach in case grid was rebuilt after a turn
        RemoveHandler CurrentForm.dgvOrders.EditingControlShowing, AddressOf OrdersGrid_EditingControlShowing
        RemoveHandler CurrentForm.dgvOrders.CellDoubleClick, AddressOf OrdersGrid_CellDoubleClick
        RemoveHandler CurrentForm.dgvOrders.KeyDown, AddressOf OrdersGrid_KeyDown
        RemoveHandler CurrentForm.dgvOrders.CellBeginEdit, AddressOf dgvOrders_CellBeginEdit
        RemoveHandler CurrentForm.dgvOrders.CellEndEdit, AddressOf dgvOrders_CellEndEdit
        RemoveHandler CurrentForm.dgvOrders.GotFocus, AddressOf dgvOrders_GotFocus

        AddHandler CurrentForm.dgvOrders.EditingControlShowing, AddressOf OrdersGrid_EditingControlShowing
        AddHandler CurrentForm.dgvOrders.CellDoubleClick, AddressOf OrdersGrid_CellDoubleClick
        AddHandler CurrentForm.dgvOrders.KeyDown, AddressOf OrdersGrid_KeyDown
        AddHandler CurrentForm.dgvOrders.CellBeginEdit, AddressOf dgvOrders_CellBeginEdit
        AddHandler CurrentForm.dgvOrders.CellEndEdit, AddressOf dgvOrders_CellEndEdit
        AddHandler CurrentForm.dgvOrders.GotFocus, AddressOf dgvOrders_GotFocus
    End Sub


    Private Sub dgvOrders_GotFocus(sender As Object, e As EventArgs)
        isEditingOrders = False
    End Sub


    ' Track when user starts and finishes editing to suppress hover hotkeys
    Private Sub dgvOrders_CellBeginEdit(sender As Object, e As DataGridViewCellCancelEventArgs)
        isEditingOrders = True
    End Sub

    Private Sub dgvOrders_CellEndEdit(sender As Object, e As DataGridViewCellEventArgs)
        isEditingOrders = False
    End Sub


    Public Sub Form_KeyDown_ForwardToOrdersGrid(sender As Object, e As KeyEventArgs)
        If CurrentForm Is Nothing OrElse CurrentForm.dgvOrders Is Nothing Then Exit Sub
        ' Only forward F1–F8 keys
        If e.KeyCode >= Keys.F1 AndAlso e.KeyCode <= Keys.F8 Then
            OrdersGrid_KeyDown(CurrentForm.dgvOrders, e)
        End If
    End Sub


    Private Sub OrdersGrid_EditingControlShowing(sender As Object, e As DataGridViewEditingControlShowingEventArgs)
        Dim grid = DirectCast(sender, DataGridView)

        ' --- Cancel edit mode instantly for Move1–Move5 so dropdown never opens ---
        If grid.CurrentCell IsNot Nothing AndAlso IsMoveComboColumn(grid.Columns(grid.CurrentCell.ColumnIndex)) Then
            grid.EndEdit()
            Return
        End If

        ' --- amount editor bit you already had ---
        If amountEditor IsNot Nothing Then
            RemoveHandler amountEditor.KeyDown, AddressOf AmountEditor_KeyDown
            amountEditor = Nothing
        End If

        If grid.CurrentCell Is Nothing Then Exit Sub

        ' Amount column => capture textbox
        If grid.Columns(grid.CurrentCell.ColumnIndex).Name = AmountColumnName Then
            Dim tb = TryCast(e.Control, TextBox)
            If tb IsNot Nothing Then
                amountEditor = tb
                AddHandler amountEditor.KeyDown, AddressOf AmountEditor_KeyDown
            End If
            Return
        End If

    End Sub

    ' Map F1..F8 to compass text
    Public Function MapFKeyToDir(key As Keys) As String
        Select Case key
            Case Keys.F1 : Return "N"
            Case Keys.F2 : Return "NE"
            Case Keys.F3 : Return "E"
            Case Keys.F4 : Return "SE"
            Case Keys.F5 : Return "S"
            Case Keys.F6 : Return "SW"
            Case Keys.F7 : Return "W"
            Case Keys.F8 : Return "NW"
            Case Else : Return Nothing
        End Select
    End Function


    Public Sub ApplyDirectionHotkeyToHoveredCell(key As Keys)
        If isEditingOrders Then Return

        If CurrentForm Is Nothing OrElse CurrentForm.dgvOrders Is Nothing Then Exit Sub

        Dim grid As DataGridView = CurrentForm.dgvOrders

        grid.EndEdit(DataGridViewDataErrorContexts.Commit)
        grid.CurrentCell = Nothing

        Dim pt As Point = grid.PointToClient(Control.MousePosition)
        Dim hit As DataGridView.HitTestInfo = grid.HitTest(pt.X, pt.Y)
        If hit.Type <> DataGridViewHitTestType.Cell Then Exit Sub

        Dim col = grid.Columns(hit.ColumnIndex)
        Dim colName As String = col.Name
        If Not (colName Like "Move[1-5]") Then Exit Sub

        Dim value As String = Nothing
        Select Case key
            Case Keys.F1 : value = "N"
            Case Keys.F2 : value = "NE"
            Case Keys.F3 : value = "E"
            Case Keys.F4 : value = "SE"
            Case Keys.F5 : value = "S"
            Case Keys.F6 : value = "SW"
            Case Keys.F7 : value = "W"
            Case Keys.F8 : value = "NW"
        End Select

        ' Add R/T for Move5 only
        If colName = "Move5" Then
            Select Case key
                Case Keys.R : value = "RECRUIT"
                Case Keys.T : value = "TRAIN"
            End Select
        End If

        If String.IsNullOrEmpty(value) Then Exit Sub

        grid.Rows(hit.RowIndex).Cells(hit.ColumnIndex).Value = value
        grid.InvalidateCell(hit.ColumnIndex, hit.RowIndex)
    End Sub



    Public Sub OrdersGrid_KeyDown(sender As Object, e As KeyEventArgs)
        If isEditingOrders Then Return

        Dim grid = DirectCast(sender, DataGridView)

        grid.EndEdit(DataGridViewDataErrorContexts.Commit)
        grid.CurrentCell = Nothing

        ' Map F1–F8 to directions
        Dim dir As String = Nothing
        Select Case e.KeyCode
            Case Keys.F1 : dir = "N"
            Case Keys.F2 : dir = "NE"
            Case Keys.F3 : dir = "E"
            Case Keys.F4 : dir = "SE"
            Case Keys.F5 : dir = "S"
            Case Keys.F6 : dir = "SW"
            Case Keys.F7 : dir = "W"
            Case Keys.F8 : dir = "NW"
        End Select
        If dir Is Nothing Then Exit Sub

        ' Find which cell the mouse is hovering over
        Dim pt As Point = grid.PointToClient(Control.MousePosition)
        Dim hit = grid.HitTest(pt.X, pt.Y)
        If hit.Type <> DataGridViewHitTestType.Cell Then Exit Sub

        ' Only act on Move1–Move5
        Dim col = grid.Columns(hit.ColumnIndex)
        If Not (col.Name.StartsWith("Move")) Then Exit Sub

        ' Commit any prior edit, then set the value directly
        If grid.IsCurrentCellInEditMode Then grid.EndEdit()
        Dim cell = grid.Rows(hit.RowIndex).Cells(hit.ColumnIndex)
        cell.Value = dir
        grid.InvalidateCell(cell)

        ' Swallow the key so the grid doesn't beep
        e.Handled = True
        e.SuppressKeyPress = True
    End Sub



    ' F9 while editing Amount => set to MAX
    Private Sub AmountEditor_KeyDown(sender As Object, e As KeyEventArgs)
        If e.KeyCode = Keys.F9 Then
            amountEditor.Text = "MAX"
            amountEditor.SelectionStart = amountEditor.TextLength
            amountEditor.SelectionLength = 0
            e.SuppressKeyPress = True
        End If
    End Sub

    ' Double-click Amount cell => enter edit mode and show "MAX" immediately
    Private Sub OrdersGrid_CellDoubleClick(sender As Object, e As DataGridViewCellEventArgs)
        If e.RowIndex < 0 Then Exit Sub
        Dim grid = DirectCast(sender, DataGridView)
        If grid.Columns(e.ColumnIndex).Name <> AmountColumnName Then Exit Sub

        ' Make this the current cell
        grid.CurrentCell = grid.Rows(e.RowIndex).Cells(e.ColumnIndex)

        ' Start editing so we can modify the live editor control
        grid.BeginEdit(True)

        ' Try to grab the editor immediately
        Dim tb = TryCast(grid.EditingControl, TextBox)
        If tb IsNot Nothing Then
            tb.Text = "MAX"
            tb.SelectionStart = tb.TextLength
            tb.SelectionLength = 0
        Else
            ' If the editor isn't created yet, do it right after the UI loop continues
            grid.BeginInvoke(Sub()
                                 Dim tb2 = TryCast(grid.EditingControl, TextBox)
                                 If tb2 IsNot Nothing Then
                                     tb2.Text = "MAX"
                                     tb2.SelectionStart = tb2.TextLength
                                     tb2.SelectionLength = 0
                                 End If
                             End Sub)
        End If

        ' Keep the underlying value in sync (so if they immediately leave the cell it's already correct)
        grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = "MAX"
        ' Optional: inform the grid the cell has pending changes
        grid.NotifyCurrentCellDirty(True)
    End Sub

    ' ----------------------------------------------------------------
    ' Populate the orders grid from current Players and their Armies
    ' ----------------------------------------------------------------
    Public Sub PopulateArmyOrdersGrid()
        CurrentForm.dgvOrders.Rows.Clear()

        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For
            For Each a In p.Armies
                If a Is Nothing Then Continue For

                Dim idx As Integer = CurrentForm.dgvOrders.Rows.Add()
                Dim row As DataGridViewRow = CurrentForm.dgvOrders.Rows(idx)

                row.Cells("colPlayer").Value = p.Race
                row.Cells("colArmy").Value = a.Name
            Next
        Next
    End Sub



    ' ----------------------------------------------------------------
    ' Extract typed orders from the grid (handles RECRUIT/TRAIN on Move5)
    ' ----------------------------------------------------------------
    Public Function GetArmyOrders() As List(Of ArmyOrder)
        Dim orders As New List(Of ArmyOrder)()

        For Each row As DataGridViewRow In CurrentForm.dgvOrders.Rows
            If row Is Nothing OrElse row.IsNewRow Then Continue For

            Dim order As New ArmyOrder()
            order.Player = If(row.Cells("colPlayer").Value, "").ToString().Trim()
            order.ArmyName = If(row.Cells("colArmy").Value, "").ToString().Trim()

            Dim moves As New List(Of String)()

            ' === Moves 1–4 ===
            For i As Integer = 1 To 4
                Dim mv As String = If(row.Cells("Move" & i).Value, "").ToString().Trim().ToUpperInvariant()
                If mv <> "" Then moves.Add(mv)
            Next

            ' === Move 5 (can be direction, TRAIN, or RECRUIT) ===
            Dim move5 As String = If(row.Cells("Move5").Value, "").ToString().Trim().ToUpperInvariant()
            Dim unitKey As String = ""
            Dim amt As String = ""

            If CurrentForm.dgvOrders.Columns.Contains("colRecruitUnit") Then
                unitKey = If(row.Cells("colRecruitUnit").Value, "").ToString().Trim()
            End If
            If CurrentForm.dgvOrders.Columns.Contains("colRecruitAmount") Then
                amt = If(row.Cells("colRecruitAmount").Value, "").ToString().Trim()
            End If

            ' Normalise values
            If amt.Equals("0") Then amt = ""
            If amt.Equals("MAX", StringComparison.OrdinalIgnoreCase) OrElse amt = "" Then amt = "MAX"
            If unitKey = "-" OrElse unitKey = "(none)" Then unitKey = ""

            Select Case move5
                Case "TRAIN"
                    moves.Add("TRAIN")

                Case "RECRUIT"
                    ' Add with parameters if available
                    If unitKey <> "" Then
                        moves.Add($"RECRUIT {unitKey} {amt}")
                    Else
                        moves.Add("RECRUIT")
                    End If

                Case "N", "NE", "E", "SE", "S", "SW", "W", "NW"
                    moves.Add(move5)

                Case ""
                    ' If Move5 blank but recruit info entered
                    If unitKey <> "" Then
                        moves.Add($"RECRUIT {unitKey} {amt}")
                    End If

                Case Else
                    ' Defensive: pass through anything unrecognised
                    moves.Add(move5)
            End Select

            order.Moves = moves
            orders.Add(order)
        Next

        For Each o In orders
            Debug.WriteLine($"[ORDERS] {o.Player} / {o.ArmyName} : {String.Join(", ", o.Moves)}")
        Next


        Return orders
    End Function



    ' ----------------------------------------------------------------
    ' Assign MoveQueue to each Army
    ' ----------------------------------------------------------------
    Public Sub ApplyOrdersToArmies(orders As List(Of ArmyOrder))
        If CurrentForm.Players Is Nothing OrElse orders Is Nothing Then Exit Sub

        For Each order In orders
            Dim p As Player = CurrentForm.Players.FirstOrDefault(Function(pl) pl.Race.Equals(order.Player, StringComparison.OrdinalIgnoreCase))
            If p Is Nothing Then Continue For
            If p.Armies Is Nothing OrElse p.Armies.Count = 0 Then Continue For

            Dim a As Army = Nothing

            a = p.Armies.FirstOrDefault(Function(ar)
                                            Return Not String.IsNullOrEmpty(ar.Name) AndAlso
                                                   ar.Name.Equals(order.ArmyName, StringComparison.OrdinalIgnoreCase)
                                        End Function)

            If a Is Nothing Then
                Dim index As Integer = orders.IndexOf(order)
                If index >= 0 AndAlso index < p.Armies.Count Then a = p.Armies(index)
            End If
            If a Is Nothing Then Continue For

            If Not String.IsNullOrWhiteSpace(order.ArmyName) AndAlso Not a.Name.Equals(order.ArmyName, StringComparison.OrdinalIgnoreCase) Then
                Dim trimmed As String = order.ArmyName.Trim()
                Dim newName As String = If(trimmed.Length > 0,
                   System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(trimmed.ToLower()),
                   String.Empty)
                a.Name = newName
            End If


            If a.MoveQueue Is Nothing Then a.MoveQueue = New List(Of ArmyCommand)
            a.MoveQueue.Clear()

            If order.Moves IsNot Nothing Then
                For Each raw In order.Moves
                    Dim parts As String() = raw.Split(New Char() {" "c, ","c, ";"c}, StringSplitOptions.RemoveEmptyEntries)
                    If parts.Length = 0 Then Continue For
                    Dim cmd As String = parts(0).ToUpperInvariant()
                    Dim ac As New ArmyCommand With {.Command = cmd}
                    If cmd = "RECRUIT" Then
                        If parts.Length >= 2 Then ac.Parameter = parts(1)
                        If parts.Length >= 3 Then ac.Amount = parts(2)
                    ElseIf parts.Length > 1 Then
                        ac.Parameter = parts(1)
                    End If
                    a.MoveQueue.Add(ac)
                Next
            End If
        Next

        For Each p In CurrentForm.Players
            For Each a In p.Armies
                If a.MoveQueue IsNot Nothing AndAlso a.MoveQueue.Count > 0 Then
                    For Each c In a.MoveQueue
                        Debug.WriteLine($"[QUEUE] {p.Race} / {a.Name} : {c.Command} {c.Parameter} {c.Amount}")
                    Next
                End If
            Next
        Next


    End Sub

    Public Sub InitialiseMoveColumns()
        For Each col As DataGridViewColumn In CurrentForm.dgvOrders.Columns
            If Not IsMoveComboColumn(col) Then Continue For
            Dim comboCol As DataGridViewComboBoxColumn = DirectCast(col, DataGridViewComboBoxColumn)
            comboCol.Items.Clear()
            If col.Name = "Move5" Then
                comboCol.Items.AddRange(moveChoicesSpecial.ToArray())
            Else
                comboCol.Items.AddRange(moveChoicesCommon.ToArray())
            End If
            comboCol.DisplayStyle = DataGridViewComboBoxDisplayStyle.DropDownButton
            comboCol.FlatStyle = FlatStyle.Standard
            comboCol.Sorted = False
        Next
    End Sub

    Public Sub FinaliseMoveColumns()
        For Each col As DataGridViewColumn In CurrentForm.dgvOrders.Columns
            If Not IsMoveComboColumn(col) Then Continue For
            Dim comboCol = DirectCast(col, DataGridViewComboBoxColumn)
            comboCol.DataSource = Nothing
            comboCol.Sorted = False
            If col.Name = "Move5" Then
                comboCol.DataSource = New List(Of String)(moveChoicesSpecial)
            Else
                comboCol.DataSource = New List(Of String)(moveChoicesCommon)
            End If
        Next

        For Each row As DataGridViewRow In CurrentForm.dgvOrders.Rows
            If row.IsNewRow Then Continue For
            For i = 1 To 5
                Dim cell = row.Cells("Move" & i)
                If cell IsNot Nothing Then
                    Dim v As String = If(cell.Value, "").ToString()
                    If v.Equals("Stay", StringComparison.OrdinalIgnoreCase) Then cell.Value = Nothing
                End If
            Next
        Next
    End Sub

    Public Sub EnableMoveColumns()
        With CurrentForm.dgvOrders
            .EditMode = DataGridViewEditMode.EditOnEnter
            .AutoGenerateColumns = False
            .AllowUserToAddRows = False
        End With

        For Each col As DataGridViewColumn In CurrentForm.dgvOrders.Columns
            If Not IsMoveComboColumn(col) Then Continue For
            Dim c = DirectCast(col, DataGridViewComboBoxColumn)
            c.ReadOnly = False
            c.DisplayStyle = DataGridViewComboBoxDisplayStyle.DropDownButton
            c.FlatStyle = FlatStyle.Standard
        Next
    End Sub

    Public Function IsMoveComboColumn(col As DataGridViewColumn) As Boolean
        If col Is Nothing OrElse Not TypeOf col Is DataGridViewComboBoxColumn Then Return False
        Dim n As String = col.Name
        Return n = "Move1" OrElse n = "Move2" OrElse n = "Move3" OrElse n = "Move4" OrElse n = "Move5"
    End Function
    Public Class ArmyOrder
        Public Property Player As String
        Public Property ArmyName As String
        Public Property Moves As List(Of String)
    End Class

    ' Directions only (no "Stay")
    Public ReadOnly moveChoicesCommon As List(Of String) =
    New List(Of String) From {"", "N", "NE", "E", "SE", "S", "SW", "W", "NW"}

    ' Directions + special RECRUIT for Move5
    Public ReadOnly moveChoicesSpecial As List(Of String) =
    New List(Of String) From {"", "N", "NE", "E", "SE", "S", "SW", "W", "NW", "RECRUIT", "TRAIN"}



    ' ============================================================
    ' === Player Assignment and Control Refresh ==================
    ' ============================================================
    Public Sub UpdatePlayerAssignment(race As String, combo As ComboBox)
        If CurrentForm.Players Is Nothing OrElse CurrentForm.Players.Count = 0 Then Exit Sub

        Dim p As Player = CurrentForm.Players.FirstOrDefault(Function(pl) pl.Race.Equals(race, StringComparison.OrdinalIgnoreCase))
        If p Is Nothing Then Exit Sub

        Dim sel As String = If(combo.SelectedItem?.ToString(), "").Trim()

        If String.IsNullOrEmpty(sel) OrElse sel.Equals("AI", StringComparison.OrdinalIgnoreCase) Then
            p.Nickname = ""
            p.AIControlled = True
        Else
            p.Nickname = sel
            p.AIControlled = False
        End If

        ' --- Auto-save whenever a player assignment changes ---
        Try
            ' Format folder name based on current game number
            Dim gameFolder As String = $"Game{CurrentForm.GameNumber:D3}"

            ' Only save if a valid game is actually active
            If CurrentForm.GameNumber > 0 Then
                CurrentForm.SaveGame(gameFolder)
                Debug.WriteLine($"[AUTOSAVE] Game {gameFolder} saved after assigning {race} -> {sel}")
            End If

        Catch ex As Exception
            Debug.WriteLine($"[AUTOSAVE ERROR] {ex.Message}")
        End Try
    End Sub


    Public Sub RefreshArmyOrdersGrid()
        If CurrentForm.Players Is Nothing OrElse CurrentForm.Players.Count = 0 Then
            CurrentForm.dgvOrders.Rows.Clear()
            Return
        End If
        PopulateArmyOrdersGrid()
        InitialiseMoveColumns()
        CurrentForm.dgvOrders.AllowUserToAddRows = False
        CurrentForm.dgvOrders.EditMode = DataGridViewEditMode.EditOnEnter
        RefreshBuySummonerControls()
        RefreshMercBidControls()
        RefreshInvestmentControls()
        WireOrdersGridMaxShortcuts()
    End Sub

    Public Sub RefreshMercBidControls()
        If CurrentForm.Players Is Nothing OrElse CurrentForm.Players.Count = 0 Then Exit Sub

        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For
            Dim race As String = p.Race.ToLowerInvariant()
            Dim suffix As String = Char.ToUpper(race(0)) & race.Substring(1).ToLower()
            Dim numBox = TryCast(CurrentForm.Controls("numMercBid" & suffix), NumericUpDown)
            If numBox IsNot Nothing Then numBox.Value = 0
            Dim cmbArmy = TryCast(CurrentForm.Controls("cmbArmy" & suffix & "Merc"), ComboBox)
            If cmbArmy Is Nothing Then Continue For
            cmbArmy.Items.Clear()
            For i As Integer = 0 To p.Armies.Count - 1
                Dim a = p.Armies(i)
                If a IsNot Nothing Then
                    Dim item As New ArmyListItem With {.Display = $"{i + 1} - {a.Name}", .Index = i}
                    cmbArmy.Items.Add(item)
                End If
            Next
            cmbArmy.SelectedIndex = -1
        Next
    End Sub

    Public Sub RefreshBuySummonerControls()
        If CurrentForm.Players Is Nothing OrElse CurrentForm.Players.Count = 0 Then Exit Sub
        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For
            Dim race As String = p.Race.ToLowerInvariant()
            Dim suffix As String = Char.ToUpper(race(0)) & race.Substring(1).ToLower()
            Dim cmbSummoner = TryCast(CurrentForm.Controls("cmbSummoner" & suffix), ComboBox)
            Dim cmbArmy = TryCast(CurrentForm.Controls("cmbArmy" & suffix), ComboBox)
            If cmbSummoner Is Nothing OrElse cmbArmy Is Nothing Then Continue For
            cmbSummoner.Items.Clear()
            Dim list As List(Of String) = GetSummonerListForRace(race)
            If list IsNot Nothing Then cmbSummoner.Items.AddRange(list.ToArray())
            cmbArmy.Items.Clear()
            For i As Integer = 0 To p.Armies.Count - 1
                Dim a = p.Armies(i)
                If a IsNot Nothing Then
                    Dim item As New ArmyListItem With {.Display = $"{i + 1} - {a.Name}", .Index = i}
                    cmbArmy.Items.Add(item)
                End If
            Next
        Next
    End Sub

    Public Sub ApplyInvestmentPurchases()
        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated OrElse p.AIControlled Then Continue For

            Dim suffix As String = Char.ToUpper(p.Race(0)) & p.Race.Substring(1).ToLower()
            Dim numBox = TryCast(CurrentForm.Controls("numInvest" & suffix), NumericUpDown)
            If numBox Is Nothing Then Continue For

            Dim amount As Integer = CInt(numBox.Value)
            p.PendingInvestment = amount
        Next
    End Sub


    Public Sub RefreshInvestmentControls()
        If CurrentForm.Players Is Nothing OrElse CurrentForm.Players.Count = 0 Then Exit Sub

        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For

            Dim race As String = p.Race.ToLowerInvariant()
            Dim suffix As String = Char.ToUpper(race(0)) & race.Substring(1).ToLower()

            Dim numBox = TryCast(CurrentForm.Controls("numInvest" & suffix), NumericUpDown)
            If numBox Is Nothing Then Continue For

            numBox.Value = 0 ' always reset after a turn
            numBox.Increment = 1000
            numBox.Maximum = 10000000 ' limit if you want, optional
        Next
    End Sub


    Public Function GetSummonerListForRace(race As String) As List(Of String)
        Dim list As New List(Of String)
        For Each s In CurrentForm.SummonerDefinitions
            If s IsNot Nothing AndAlso s.AllowedRace.Equals(race, StringComparison.OrdinalIgnoreCase) Then list.Add(s.Name)
        Next
        Return list
    End Function

    ' ===== UI helpers =====
    Public Class UnitOption
        Public Property Display As String
        Public Property ShortName As String
    End Class

    Private Function UnitTypeToDisplay(t As UnitType) As String
        Return t.ToString().Replace("_"c, " "c)
    End Function

    Private Function SanitizeShort(name As String) As String
        If String.IsNullOrWhiteSpace(name) Then Return ""
        Dim sb As New System.Text.StringBuilder()
        For Each ch In name.ToLowerInvariant()
            If Char.IsLetterOrDigit(ch) Then sb.Append(ch)
        Next
        Return sb.ToString()
    End Function

    Public Function BuildUnitOptionsForRace(race As String) As List(Of UnitOption)
        Dim opts As New List(Of UnitOption)
        If String.IsNullOrWhiteSpace(race) Then Return opts
        Dim ru As RaceUnits = Nothing
        For Each r In CurrentForm.AllRaces
            If r IsNot Nothing AndAlso r.RaceName.Equals(race, StringComparison.OrdinalIgnoreCase) Then ru = r : Exit For
        Next
        If ru Is Nothing OrElse ru.Units Is Nothing Then Return opts
        For Each u In ru.Units
            If u Is Nothing Then Continue For
            Dim sn As String = If(String.IsNullOrWhiteSpace(u.ShortName), SanitizeShort(u.Name), u.ShortName)
            opts.Add(New UnitOption With {.Display = $"{u.Name} / {UnitTypeToDisplay(u.Type)}", .ShortName = sn})
        Next
        Return opts
    End Function

    ' ----------------------------------------------------------------
    ' Sync all player combo selections to the actual player objects
    ' ----------------------------------------------------------------
    Public Sub SyncPlayerAssignmentsToCombos()
        If CurrentForm Is Nothing OrElse CurrentForm.Players Is Nothing Then Exit Sub

        For Each p In CurrentForm.Players
            If p Is Nothing Then Continue For
            Dim race As String = p.Race.ToLowerInvariant()
            Dim suffix As String = Char.ToUpper(race(0)) & race.Substring(1).ToLower()

            Dim combo = TryCast(CurrentForm.Controls("cmb" & suffix), ComboBox)
            If combo Is Nothing Then Continue For

            Dim sel As String = If(combo.SelectedItem?.ToString(), "").Trim()

            If String.IsNullOrEmpty(sel) OrElse sel.Equals("AI", StringComparison.OrdinalIgnoreCase) Then
                p.Nickname = ""
                p.AIControlled = True
            Else
                p.Nickname = sel
                p.AIControlled = False
            End If
        Next
    End Sub



End Module
