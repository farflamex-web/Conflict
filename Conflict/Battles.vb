
Imports Conflict.Form1
Imports Conflict.UIModule
Module Battles

    Public Sub ResolveCombat()
        Dim armiesAlreadyInBattle As New HashSet(Of Army)()
        Dim processedLocations As New HashSet(Of Point)()

        ' === Always enumerate snapshots to avoid "collection modified" ===
        Dim playersSnapshot As List(Of Player) =
        If(CurrentForm.Players, New List(Of Player)()) _
        .Where(Function(pp) pp IsNot Nothing AndAlso Not pp.IsEliminated AndAlso pp.Armies IsNot Nothing) _
        .ToList()

        ' Loop through each player (snapshot)
        For Each p As Player In playersSnapshot
            ' Snapshot this player's armies (live list may change due to retreats/elims)
            Dim armiesOfP As List(Of Army) = If(p.Armies, New List(Of Army)()).ToList()

            For Each a As Army In armiesOfP
                If a Is Nothing Then Continue For
                ' Skip armies that cannot fight or already battled
                If a.TotalSoldiers <= 0 OrElse armiesAlreadyInBattle.Contains(a) Then Continue For

                Dim grid As List(Of Point) = GetCombatGrid(a)
                If grid Is Nothing OrElse grid.Count = 0 Then Continue For

                ' Snapshot grid too (GetCombatGrid could be dynamic)
                For Each pt As Point In grid.ToList()
                    If processedLocations.Contains(pt) Then Continue For

                    ' --- Gather all armies on this tile that can fight (via snapshots) ---
                    Dim allArmiesHere As New List(Of Army)

                    For Each pl As Player In playersSnapshot
                        Dim plArmies As List(Of Army) = If(pl.Armies, New List(Of Army)()).ToList()
                        For Each ar As Army In plArmies
                            If ar Is Nothing Then Continue For
                            If ar.TotalSoldiers <= 0 Then Continue For
                            If armiesAlreadyInBattle.Contains(ar) Then Continue For

                            Dim arGrid As List(Of Point) = GetCombatGrid(ar)
                            If arGrid IsNot Nothing AndAlso arGrid.Contains(pt) Then
                                allArmiesHere.Add(ar)
                            End If
                        Next
                    Next

                    ' Debug line to confirm how many armies are found at this tile
                    Console.WriteLine($"[COMBAT] Tile {pt.X},{pt.Y} has {allArmiesHere.Count} armies engaged.")

                    ' Need at least 2 distinct sides
                    Dim distinctPlayers As New HashSet(Of String)(
                    allArmiesHere.Where(Function(ar) ar IsNot Nothing).Select(Function(ar) ar.Race),
                    StringComparer.OrdinalIgnoreCase
                )
                    If distinctPlayers.Count < 2 Then Continue For

                    processedLocations.Add(pt)

                    ' --- Merge armies per player temporarily for battle ---
                    Dim mergedArmies As New List(Of Army)
                    Dim mergedToOriginal As New Dictionary(Of Army, List(Of Army))()

                    For Each race As String In distinctPlayers.ToList()
                        Dim playerArmies As List(Of Army) =
                        allArmiesHere.Where(Function(ar) ar.Race.Equals(race, StringComparison.OrdinalIgnoreCase)).ToList()

                        ' Guard: skip if somehow empty
                        If playerArmies.Count = 0 Then Continue For

                        Dim mergedArmy As New Army With {
                        .Race = race,
                        .X = playerArmies(0).X,
                        .Y = playerArmies(0).Y
                    }

                        ' Copy units from each source army into the merged army (clone stacks)
                        For Each ua As Army In playerArmies
                            For Each u As Unit In ua.Units.ToList()
                                mergedArmy.Units.Add(New Unit(u)) ' clone constructor
                            Next
                        Next

                        mergedArmies.Add(mergedArmy)
                        mergedToOriginal(mergedArmy) = playerArmies
                    Next

                    ' --- Immutable pre-battle snapshot BEFORE Battle() mutates anything ---
                    Dim startSnapshot As List(Of Army) = CloneArmiesForSnapshot(mergedArmies)

                    ' --- Conduct battle ---
                    Dim battleLog As BattleLog = Battle(mergedArmies)
                    If battleLog Is Nothing Then Continue For

                    ' --- Distribute casualties back proportionally to original armies ---
                    For Each mergedArmy As Army In mergedArmies.ToList()
                        Dim originalArmies As List(Of Army) = mergedToOriginal(mergedArmy)

                        For Each mergedUnit As Unit In mergedArmy.Units.ToList()
                            ' Total size of this unit across all original armies (pre-battle)
                            Dim totalOriginalSize As Integer =
                            originalArmies.Sum(Function(origArmy) origArmy.Units.
                                Where(Function(u) u.Name = mergedUnit.Name AndAlso u.Type = mergedUnit.Type).
                                Sum(Function(u) u.Size))

                            If totalOriginalSize = 0 Then Continue For

                            ' Sum casualties this specific merged unit took across all phases (by reference)
                            Dim mergedCasualties As Integer = 0
                            For Each phaseList In battleLog.PhaseEntries.Values.ToList()
                                For Each e In phaseList.ToList()
                                    If Object.ReferenceEquals(e.Defender, mergedUnit) Then
                                        mergedCasualties += e.Casualties
                                    End If
                                Next
                            Next
                            If mergedCasualties <= 0 Then Continue For

                            ' === EXACT ALLOCATION (floor + largest remainder) ===
                            Dim targets As New List(Of Tuple(Of Unit, Integer))() ' (unitRef, preSize)
                            For Each origArmy In originalArmies
                                For Each u In origArmy.Units.ToList()
                                    If u.Name = mergedUnit.Name AndAlso u.Type = mergedUnit.Type Then
                                        targets.Add(Tuple.Create(u, u.Size))
                                    End If
                                Next
                            Next
                            If targets.Count = 0 Then Continue For

                            Dim shares(targets.Count - 1) As Integer
                            Dim remainders As New List(Of Tuple(Of Integer, Double))()
                            Dim assigned As Integer = 0

                            For i As Integer = 0 To targets.Count - 1
                                Dim pre As Integer = targets(i).Item2
                                Dim exact As Double = mergedCasualties * (pre / CDbl(totalOriginalSize))
                                Dim flo As Integer = CInt(Math.Floor(exact))
                                flo = Math.Min(flo, pre)
                                shares(i) = flo
                                assigned += flo
                                remainders.Add(Tuple.Create(i, exact - flo))
                            Next

                            Dim remainder As Integer = mergedCasualties - assigned
                            If remainder > 0 Then
                                remainders = remainders.OrderByDescending(Function(t) t.Item2).ThenBy(Function(t) t.Item1).ToList()
                                Dim k As Integer = 0
                                While remainder > 0 AndAlso remainders.Count > 0
                                    Dim idx As Integer = remainders(k Mod remainders.Count).Item1
                                    Dim pre As Integer = targets(idx).Item2
                                    If shares(idx) < pre Then
                                        shares(idx) += 1
                                        remainder -= 1
                                        k += 1
                                    Else
                                        ' drop capped index from rotation
                                        remainders.RemoveAt(k Mod remainders.Count)
                                    End If
                                End While
                            End If

                            ' Apply shares to live unit refs
                            For i As Integer = 0 To targets.Count - 1
                                Dim uRef As Unit = targets(i).Item1
                                Dim take As Integer = shares(i)
                                If take > uRef.Size Then take = uRef.Size
                                uRef.Size -= take
                            Next
                            ' === END EXACT ALLOCATION ===
                        Next
                    Next

                    ' --- Determine winning race from post-battle merged totals ---
                    Dim winningRace As String = Nothing
                    Dim raceTotals = mergedArmies _
                    .GroupBy(Function(ma) ma.Race, StringComparer.OrdinalIgnoreCase) _
                    .Select(Function(g) New With {
                        .Race = g.Key,
                        .Total = g.Sum(Function(ma) ma.TotalSoldiers)
                    }) _
                    .OrderByDescending(Function(x) x.Total) _
                    .ToList()

                    If raceTotals.Count > 0 Then
                        Dim top = raceTotals(0)
                        If raceTotals.Count = 1 OrElse top.Total > raceTotals(1).Total Then
                            winningRace = top.Race
                        End If
                    End If

                    ' --- Retreat losers on the live armies (does not remove lists) ---
                    If Not String.IsNullOrEmpty(winningRace) Then
                        For Each army As Army In allArmiesHere.ToList()
                            If Not army.Race.Equals(winningRace, StringComparison.OrdinalIgnoreCase) Then
                                CurrentForm.SendArmyBackToSpawn(army)      ' must NOT remove army from its owner's list
                                battleLog.RecordRetreat(army)
                            End If
                        Next
                    End If

                    ' --- Mark these live armies as having battled ---
                    For Each army As Army In allArmiesHere.ToList()
                        armiesAlreadyInBattle.Add(army)
                    Next

                    ' --- Report ---
                    Dim compactReport As String = CurrentForm.GenerateCompactPhaseReport(battleLog, mergedArmies, startSnapshot, allArmiesHere)
                    CurrentForm.rtbInfo.AppendText(compactReport)

                    ' --- Cleanup dead stacks (safe, only mutates Units of each army) ---
                    For Each army As Army In allArmiesHere.ToList()
                        army.Units.RemoveAll(Function(u) u.Size <= 0)
                    Next

                    ' Do NOT recompute eliminations here; queue them and apply after loops
                    ' (prevents collection mutation mid-enumeration)

                    ' *** Removed "Exit For" to allow all armies on same tile to be processed ***
                Next
            Next
        Next

        ' === Post-battle training reset for wiped armies ===
        For Each p In CurrentForm.Players
            If p.Armies Is Nothing Then Continue For
            For Each a In p.Armies
                If a Is Nothing Then Continue For
                If a.Units Is Nothing OrElse a.Units.Sum(Function(u) u.Size) <= 0 Then
                    a.TrainingLevel = 0
                End If
            Next
        Next


        ' After all enumerations are complete, now it’s safe to eliminate players
        CurrentForm.QueueEliminationChecksSafe()
    End Sub



    Public Function Battle(battleArmies As List(Of Army)) As BattleLog
        ' --- Filter armies with soldiers remaining ---
        Dim activeArmies As List(Of Army) = battleArmies.Where(Function(a) a.TotalSoldiers > 0).ToList()
        If activeArmies.Count = 0 Then Return Nothing

        Dim battleLog As New BattleLog(activeArmies)

        ' --- Run the first three phases exactly as before ---
        For Each phaseName In New String() {"Ranged", "Charge", "Melee"}
            ' Snapshot of units for proportional damage (phase-start sizes)
            Dim unitSnapshot As New Dictionary(Of Unit, Integer)
            For Each army In activeArmies
                For Each u In army.Units
                    unitSnapshot(u) = u.Size
                Next
            Next

            ' Apply proportional damage for each defending army from all attackers
            For Each defArmy In activeArmies
                Dim attackers As List(Of Unit) = activeArmies.
                                             Where(Function(a) a IsNot defArmy).
                                             SelectMany(Function(a) a.Units).ToList()
                Dim attackerArmies As List(Of Army) = activeArmies.Where(Function(a) a IsNot defArmy).ToList()
                ApplyProportionalDamage(defArmy, attackerArmies, phaseName, unitSnapshot, battleLog)
            Next
        Next

        ' === Decide winner/losers after Melee (phase 3) ===
        ' Compute current strengths post-casualties
        If activeArmies.Count < 2 Then
            ' Nothing to chase if only one side remains
            Return battleLog
        End If

        Dim maxStrength As Integer = activeArmies.Max(Function(a) a.TotalSoldiers)
        Dim topArmies = activeArmies.Where(Function(a) a.TotalSoldiers = maxStrength).ToList()

        ' If tie for strongest -> no chase
        If topArmies.Count <> 1 Then
            Return battleLog
        End If

        Dim winner As Army = topArmies(0)
        Dim losers As List(Of Army) = activeArmies.Where(Function(a) a IsNot winner).ToList()

        ' Winner's LC only
        Dim winnerLC As List(Of Unit) = winner.Units.
            Where(Function(u) u.CanChase AndAlso u.Size > 0).ToList()

        ' If no LC or no losers -> no chase
        If winnerLC.Count = 0 OrElse losers.Count = 0 Then
            Return battleLog
        End If

        ' --- CHASE PHASE (only winner LC attacks, only losers defend) ---
        ' Build a fresh snapshot at start of Chase (sizes after Melee)
        Dim chaseSnapshot As New Dictionary(Of Unit, Integer)
        For Each army In activeArmies
            For Each u In army.Units
                chaseSnapshot(u) = u.Size
            Next
        Next

        ' Each losing army takes proportional damage from winner LC
        For Each defArmy In losers
            ApplyProportionalDamage(defArmy, {winner}.ToList(), "Chase", chaseSnapshot, battleLog)
        Next

        Return battleLog
    End Function


    Public Sub ClearArmyMoveQueue(army As Army)
        ''' <summary>
        ''' Clears all remaining moves in the army's MoveQueue.
        ''' This is used after an army is sent back to spawn,
        ''' since any queued moves are no longer relevant.
        ''' </summary>

        If army.MoveQueue IsNot Nothing Then
            army.MoveQueue.Clear()
        End If
    End Sub


    Public Sub ApplyProportionalDamage(defArmy As Army,
                                   attackerArmies As List(Of Army),
                                   phase As String,
                                   unitSnapshot As Dictionary(Of Unit, Integer),
                                   battleLog As BattleLog)

        ' --- Initialize dictionary to accumulate casualties per defender unit ---
        Dim calculatedCasualties As New Dictionary(Of Unit, Long)
        For Each defUnit In defArmy.Units
            calculatedCasualties(defUnit) = 0
        Next

        ' --- Flatten attackers for iteration ---
        Dim attackingUnits As List(Of Unit) = attackerArmies.SelectMany(Function(a) a.Units).ToList()

        ' --- Loop over each attacker ---
        For Each atkUnit In attackingUnits
            Dim atkSize As Integer = unitSnapshot(atkUnit) ' snapshot for attacker's size
            Dim atkValue As Integer = 0
            Dim atkExplanation As String = ""

            ' Determine attack value based on phase
            Select Case phase.ToLower()
                Case "ranged"
                    If atkUnit.Ranged > 0 Then
                        atkValue = atkUnit.Ranged
                        atkExplanation = $"{atkUnit.Ranged} ranged"
                    Else
                        Continue For
                    End If

                Case "charge"
                    If atkUnit.CanCharge Then
                        atkValue = atkUnit.Melee
                        atkExplanation = $"{atkUnit.Melee} melee (charge phase)"
                    Else
                        Continue For
                    End If

                Case "melee"
                    atkValue = atkUnit.Melee
                    atkExplanation = $"{atkUnit.Melee} melee"

                Case "chase"
                    If atkUnit.CanChase Then
                        atkValue = atkUnit.Melee
                        If atkUnit.Flying Then
                            atkValue *= 2
                            atkExplanation = $"{atkUnit.Melee} melee (flying, double in chase)"
                        Else
                            atkExplanation = $"{atkUnit.Melee} melee for chase"
                        End If
                    Else
                        Continue For
                    End If
            End Select

            ' === Find this unit's army (for training level lookup) ===
            Dim atkArmy As Army = attackerArmies.FirstOrDefault(Function(a) a.Units.Contains(atkUnit))

            ' === Apply attacker training bonus ===
            If atkArmy IsNot Nothing AndAlso atkArmy.TrainingLevel > 0 Then
                Dim atkBonus As Double = 1.0 + atkArmy.TrainingLevel
                atkValue = CInt(Math.Round(atkValue * atkBonus))
                atkExplanation &= $" +{atkArmy.TrainingLevel:P0} training"
            End If

            ' --- Total HP-weighted size of defenders ---
            Dim totalDefWeight As Double = defArmy.Units.Sum(Function(u) unitSnapshot(u) * u.GetEffectiveHP())
            If totalDefWeight = 0 Then Continue For

            ' --- Apply proportional damage to each defender unit ---
            For Each defUnit In defArmy.Units
                Dim sizeBefore As Integer = unitSnapshot(defUnit)
                Dim unitWeight As Double = sizeBefore * defUnit.GetEffectiveHP()

                ' Raw damage proportional to weight
                Dim rawDamage As Double = atkValue * atkSize * (unitWeight / totalDefWeight)

                ' Mitigation fraction + explanation
                Dim mitResult = GetUnitMitigation(defUnit)
                Dim mitigationValue As Double = mitResult.Mitigation
                Dim mitigationExplanation As String = mitResult.Explanation

                ' === Apply defender training bonus (reduces damage slightly) ===
                If defArmy.TrainingLevel > 0 Then
                    ' Defensive training bonus is half as strong as attack training
                    Dim defBonus As Double = Math.Min(defArmy.TrainingLevel * 0.5, 0.9)
                    mitigationValue = 1 - ((1 - mitigationValue) * (1 - defBonus))
                    mitigationExplanation &= $" +{defArmy.TrainingLevel:P0} training"
                End If

                ' Final damage after mitigation, with global casualty multiplier
                Const CASUALTY_MULTIPLIER As Double = 3.0
                Dim finalDamage As Double = rawDamage * (1 - mitigationValue) * CASUALTY_MULTIPLIER

                ' --- Calculate casualties (clamped to remaining men this phase) ---
                Dim casualtiesRaw As Integer = CInt(Math.Floor(finalDamage / defUnit.GetEffectiveHP()))
                Dim alreadyAllocated As Integer = CInt(calculatedCasualties(defUnit))
                Dim remainingThisPhase As Integer = Math.Max(0, sizeBefore - alreadyAllocated)
                Dim casualties As Integer = Math.Min(remainingThisPhase, casualtiesRaw)

                ' --- Record entry in BattleLog (using clamped casualties) ---
                If battleLog IsNot Nothing AndAlso casualties > 0 Then
                    Dim entry As New BattleEntry With {
                    .Attacker = atkUnit,
                    .Defender = defUnit,
                    .Phase = phase,
                    .SizeBefore = sizeBefore,
                    .SizeAfter = Math.Max(0, sizeBefore - (alreadyAllocated + casualties)),
                    .RawDamage = rawDamage,
                    .Mitigation = mitigationValue,
                    .MitigationExplanation = mitigationExplanation,
                    .FinalDamage = finalDamage,
                    .Casualties = casualties,
                    .RawDamageExplanation = $"Damage from {atkUnit.Name} ({atkExplanation})",
                    .AttackerSizeAtPhaseStart = atkSize
                }

                    If Not battleLog.PhaseEntries.ContainsKey(phase) Then
                        battleLog.PhaseEntries(phase) = New List(Of BattleEntry)
                    End If
                    battleLog.PhaseEntries(phase).Add(entry)
                End If

                ' Accumulate casualties for simultaneous application
                calculatedCasualties(defUnit) += casualties
            Next
        Next

        ' --- Apply all accumulated casualties to real unit sizes ---
        For Each kvp In calculatedCasualties
            kvp.Key.Size -= CInt(kvp.Value)
            If kvp.Key.Size < 0 Then kvp.Key.Size = 0

            ' Optional: mark/report dead now
            If kvp.Key.Size = 0 Then
                'Debug.WriteLine($"[BATTLE] {kvp.Key.Name} wiped out in {phase} phase")
            End If
        Next

        ' ✅ Note: Do not remove units here. Cleanup will happen at the end of ResolveBattle.
    End Sub



    Public Function GetUnitMitigation(unit As Unit) As (Mitigation As Double, Explanation As String)
        Dim mitigation As Double = 0.0
        Dim parts As New List(Of String)

        ' DefencePoints mitigation — use effective value for heroes
        Dim effectiveDef As Integer = unit.GetEffectiveDefencePoints()
        If effectiveDef > 0 Then
            Dim defMit As Double = effectiveDef * 0.05
            mitigation += defMit
            parts.Add($"{effectiveDef * 5}% defence points")
        End If

        ' Cap total mitigation at 70%
        If mitigation > 0.7 Then
            mitigation = 0.7
            parts.Add("(capped at 70%)")
        End If

        Dim explanation As String = If(parts.Count > 0, String.Join(" + ", parts), "No mitigation")
        Return (mitigation, explanation)
    End Function

    Private Function GetCombatGrid(a As Army) As List(Of Point)
        Dim points As New List(Of Point)
        Dim maxX As Integer = CurrentForm.Map.GetLength(0) - 1
        Dim maxY As Integer = CurrentForm.Map.GetLength(1) - 1
        For dx As Integer = -2 To 2
            For dy As Integer = -2 To 2
                Dim x = a.X + dx
                Dim y = a.Y + dy
                If x >= 0 AndAlso x <= maxX AndAlso y >= 0 AndAlso y <= maxY Then
                    points.Add(New Point(x, y))
                End If
            Next
        Next
        Return points
    End Function
    Private Function CloneArmiesForSnapshot(src As List(Of Army)) As List(Of Army)
        Dim clone As New List(Of Army)
        For Each a In src
            Dim na As New Army With {.Race = a.Race, .X = a.X, .Y = a.Y}
            For Each u In a.Units
                na.Units.Add(New Unit(u)) ' use the clone constructor
            Next
            clone.Add(na)
        Next
        Return clone
    End Function

End Module
