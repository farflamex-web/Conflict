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
    ' Extract typed orders from the grid (handles RECRUIT on move 5)
    ' ----------------------------------------------------------------
    Public Function GetArmyOrders() As List(Of ArmyOrder)
        Dim orders As New List(Of ArmyOrder)()

        For Each row As DataGridViewRow In CurrentForm.dgvOrders.Rows
            If row Is Nothing OrElse row.IsNewRow Then Continue For

            Dim order As New ArmyOrder()
            order.Player = If(row.Cells("colPlayer").Value, "").ToString().Trim()
            order.ArmyName = If(row.Cells("colArmy").Value, "").ToString().Trim()

            Dim moves As New List(Of String)()

            ' === Moves 1..4 ===
            For i As Integer = 1 To 4
                Dim mv As String = If(row.Cells("Move" & i).Value, "").ToString().Trim()
                If mv <> "" Then moves.Add(mv)
            Next

            ' === Move 5 or Recruit ===
            Dim move5 As String = If(row.Cells("Move5").Value, "").ToString().Trim()
            Dim unitKey As String = ""
            Dim amt As String = ""

            If CurrentForm.dgvOrders.Columns.Contains("colRecruitUnit") Then
                unitKey = If(row.Cells("colRecruitUnit").Value, "").ToString().Trim()
            End If
            If CurrentForm.dgvOrders.Columns.Contains("colRecruitAmount") Then
                amt = If(row.Cells("colRecruitAmount").Value, "").ToString().Trim()
            End If

            If amt.Equals("MAX", StringComparison.OrdinalIgnoreCase) OrElse amt = "0" Then amt = ""
            If unitKey = "-" OrElse unitKey = "(none)" Then unitKey = ""

            If move5.ToUpperInvariant() = "RECRUIT" AndAlso (unitKey <> "" OrElse amt <> "") Then
                If amt = "" Then amt = "MAX"
                moves.Add($"RECRUIT {unitKey} {amt}")
            ElseIf move5 <> "" Then
                moves.Add(move5)
            ElseIf unitKey <> "" AndAlso amt <> "" Then
                moves.Add($"RECRUIT {unitKey} {amt}")
            End If

            order.Moves = moves
            orders.Add(order)
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

            If Not String.IsNullOrWhiteSpace(order.ArmyName) AndAlso
               Not a.Name.Equals(order.ArmyName, StringComparison.OrdinalIgnoreCase) Then
                Dim newName As String = StrConv(order.ArmyName.Trim(), VbStrConv.ProperCase)
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
    New List(Of String) From {"", "N", "NE", "E", "SE", "S", "SW", "W", "NW", "RECRUIT"}



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
End Module
