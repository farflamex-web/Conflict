
Imports Conflict.UIModule

Module Mercenaries

    Public MercPriceLevel As Integer = 0

    Private Sub AwardMercenariesToPlayer(offer As Form1.MercenaryArmy, winner As Form1.Player)
        If offer Is Nothing OrElse winner Is Nothing Then Exit Sub
        If winner.Armies Is Nothing OrElse winner.Armies.Count = 0 Then Exit Sub

        ' === Determine target army ===
        Dim target As Form1.Army = Nothing

        ' Human players: use the army chosen via ApplyMercBids (if still valid)
        If Not winner.AIControlled AndAlso winner.TargetArmyForMercs IsNot Nothing Then
            Dim idx As Integer = winner.Armies.IndexOf(winner.TargetArmyForMercs)
            If idx >= 0 Then target = winner.Armies(idx)
        End If

        ' AI players: random army 0..2 (safe if fewer than 3 armies)
        If target Is Nothing AndAlso winner.AIControlled Then
            Dim rnd As New Random()
            Dim upper As Integer = Math.Min(3, winner.Armies.Count) ' Next upper bound is exclusive
            Dim randIndex As Integer = rnd.Next(0, upper)           ' 0..2 if 3+, otherwise 0..Count-1
            target = winner.Armies(randIndex)
        End If

        ' Fallback
        If target Is Nothing Then target = winner.Armies(0)

        ' Ensure Units list exists
        If target.Units Is Nothing Then target.Units = New List(Of Form1.Unit)()

        ' Build a concise composition string as we add/merge
        Dim parts As New List(Of String)

        ' Track total men and total training effect
        Dim totalNewMen As Integer = 0
        Dim weightedTraining As Double = 0.0

        For Each s In offer.Units
            If s Is Nothing Then Continue For

            If s.Hero IsNot Nothing Then
                ' === Hero mercenary ===
                s.Hero.IsMercenary = True
                target.Units.Add(s.Hero)
                parts.Add($"{s.Hero.Name} (Lvl {s.Hero.Level})")

            ElseIf s.Template IsNot Nothing Then
                ' === Normal mercenary ===
                Dim t As Form1.UnitStats = s.Template
                Dim count As Integer = s.Count
                totalNewMen += count

                ' Accumulate weighted training bonus if this template has one
                If t.TrainingBonus > 0 Then
                    weightedTraining += t.TrainingBonus * count
                End If

                ' Merge if this type already exists
                Dim existing = target.Units.FirstOrDefault(Function(u) u.Name = t.Name AndAlso u.Type = t.Type)
                If existing IsNot Nothing Then
                    existing.Size += count
                    existing.IsMercenary = True
                Else
                    Dim newU As New Form1.Unit(t, winner.Race, count)
                    newU.IsMercenary = True
                    target.Units.Add(newU)
                End If

                parts.Add($"{t.Name} x{count}")
            End If
        Next

        ' === Apply combined training from unit-level bonuses ===
        If totalNewMen > 0 AndAlso weightedTraining > 0 Then
            Dim oldMen As Integer = target.TotalSoldiers
            Dim totalMen As Integer = Math.Max(1, oldMen + totalNewMen)
            Dim averageNewTraining As Double = weightedTraining / totalNewMen
            Dim combined As Double = ((target.TrainingLevel * oldMen) + (averageNewTraining * totalNewMen)) / totalMen
            target.TrainingLevel = combined
            Debug.WriteLine($"[MERC] Combined merc training {averageNewTraining:P0}; new army training level = {target.TrainingLevel:P0}")
        End If

        ' === Debug: where it went and what it was ===
        Dim armyIndex As Integer = winner.Armies.IndexOf(target)
        If armyIndex < 0 Then armyIndex = 0
        Debug.WriteLine($"[MERC] Awarded to {winner.Race} (Player {winner.PlayerNumber + 1}) → Army #{armyIndex + 1} '{target.Name}' at ({target.X},{target.Y}). Faction: {offer.Faction}")
        Debug.WriteLine($"[MERC] Composition: {String.Join(", ", parts)}")

        ' Cleanup selection for next turn
        winner.TargetArmyForMercs = Nothing
    End Sub

    Public Function GenerateMercenaryOffer(turnNumber As Integer) As Form1.MercenaryArmy
        Dim rnd As New Random()

        ' === 1. Budget scales directly with current market level ===
        ' MercPriceLevel starts at 50 and increases by 50 each time an army is bought.
        Dim maxBudget As Integer = MercPriceLevel

        Dim mercFactions As String() = {
        "Warrior Monks", "Skulkrin", "Barbarians", "Werecreatures", "Harpies",
        "Cultists", "Demons", "Undead", "Golems", "Elementals",
        "Bandits", "Nomads", "Freeblades", "Sellswords"
    }

        Dim mercArmyNormal As Form1.MercenaryArmy = Nothing
        Dim outerGuard As Integer = 1000 ' prevent infinite faction loop

        Do While outerGuard > 0
            outerGuard -= 1

            ' === 2. Pick a random faction ===
            Dim faction As String = mercFactions(rnd.Next(mercFactions.Length))
            Dim roster As Form1.RaceUnits = CurrentForm.AllRaces.FirstOrDefault(Function(r) r.RaceName.Equals(faction, StringComparison.OrdinalIgnoreCase))
            If roster Is Nothing OrElse roster.Units.Count = 0 Then Continue Do

            mercArmyNormal = New Form1.MercenaryArmy With {
            .Faction = faction,
            .Units = New List(Of Form1.MercenaryStack),
            .MinBid = MercPriceLevel        ' Minimum bid equals the current market level
        }

            Dim remaining As Integer = maxBudget
            Dim cheapestPower As Integer = roster.Units.Min(Function(u) u.Power)
            Dim hasFodder As Boolean = roster.Units.Any(Function(u) u.Power <= 2)

            ' --- Allowed block sizes ---
            Dim blockSizes As Integer()
            If hasFodder Then
                blockSizes = {10} ' Only groups of 10 for fodder-based factions
            Else
                blockSizes = {10, 5, 1} ' fallback allowed for elite units
            End If

            Dim pickedAnything As Boolean = False
            Dim innerGuard As Integer = 10000

            ' === 3. Randomly assemble units up to the budget ===
            Do While remaining >= cheapestPower AndAlso innerGuard > 0
                innerGuard -= 1

                ' Weighted bias toward earlier (usually weaker) units
                Dim weighted As New List(Of Form1.UnitStats)
                For i As Integer = 0 To roster.Units.Count - 1
                    Dim u = roster.Units(i)
                    Dim weight As Integer = Math.Max(1, (roster.Units.Count - i) * 5)
                    For n As Integer = 1 To weight
                        weighted.Add(u)
                    Next
                Next

                Dim pick As Form1.UnitStats = weighted(rnd.Next(weighted.Count))

                ' Skip units that don't fit budget
                If pick.Power <= 0 OrElse pick.Power > remaining Then Continue Do

                ' Find largest block that fits
                Dim count As Integer = 0
                For Each b In blockSizes
                    If pick.Power * b <= remaining Then
                        count = b
                        Exit For
                    End If
                Next

                If count = 0 Then Continue Do

                ' Add to army
                Dim existing = mercArmyNormal.Units.FirstOrDefault(Function(s) s.Template IsNot Nothing AndAlso s.Template.Name = pick.Name)
                If existing IsNot Nothing Then
                    existing.Count += count
                Else
                    mercArmyNormal.Units.Add(New Form1.MercenaryStack With {.Template = pick, .Count = count})
                End If

                pickedAnything = True
                remaining -= pick.Power * count
            Loop

            If innerGuard = 0 Then
                'Debug.WriteLine("Guard triggered in GenerateMercenaryOffer inner loop — possible bad roster config.")
            End If

            ' === 4. Guarantee at least one unit ===
            If Not pickedAnything Then
                If cheapestPower > maxBudget Then
                    mercArmyNormal = Nothing
                    Continue Do
                End If

                Dim cheapest = roster.Units.Where(Function(u) u.Power <= maxBudget).OrderBy(Function(u) u.Power).First()
                mercArmyNormal.Units.Add(New Form1.MercenaryStack With {.Template = cheapest, .Count = 1})
            End If

            ' === 5. Return the completed mercenary army ===
            If mercArmyNormal.Units.Count > 0 Then Exit Do
        Loop

        If outerGuard = 0 Then
            'Debug.WriteLine("Guard triggered in GenerateMercenaryOffer outer loop — possible issue with faction selection.")
        End If

        ' === Reveal this new mercenary offer to all human players ===
        RevealMercenaryOffer(mercArmyNormal, CurrentForm.Players)

        Return mercArmyNormal
    End Function


    Public Sub ResolveMercenaryAuction(bids As Dictionary(Of Form1.Player, Integer))
        ' === 1. Validate offer ===
        If Form1.CurrentMercOffer Is Nothing OrElse Form1.CurrentMercOffer.Units.Count = 0 Then Exit Sub



        ' === 2. Handle no bids ===
        If bids Is Nothing OrElse bids.Count = 0 Then
            'Debug.WriteLine("No bids received; mercenary offer dismissed.")
            Form1.CurrentMercOffer = Nothing
            Exit Sub
        End If

        ' === 3. Filter invalid bids ===
        Dim candidateBids = bids.
        Where(Function(kv) kv.Value >= Form1.CurrentMercOffer.MinBid AndAlso
                         kv.Key IsNot Nothing AndAlso
                         kv.Key.Gold >= kv.Value).
        ToList()

        If candidateBids.Count = 0 Then
            'Debug.WriteLine("No valid bids (must meet MinBid and be within available gold); mercenary offer dismissed.")
            Form1.CurrentMercOffer = Nothing
            Exit Sub
        End If

        ' === 4. Winner selection ===
        Dim topAmount = candidateBids.Max(Function(kv) kv.Value)
        Dim topBidders = candidateBids.Where(Function(kv) kv.Value = topAmount).ToList()

        Dim winnerKV As KeyValuePair(Of Form1.Player, Integer)
        If topBidders.Count = 1 Then
            winnerKV = topBidders(0)
        Else
            ' Tie-breaker: richest bidder, then lowest PlayerNumber
            winnerKV = topBidders.
            OrderByDescending(Function(kv) kv.Key.Gold).
            ThenBy(Function(kv) kv.Key.PlayerNumber).
            First()
        End If

        Dim winner As Form1.Player = winnerKV.Key
        Dim amount As Integer = winnerKV.Value

        ' === 5. Deduct gold and award ===
        winner.Gold -= amount
        winner.MercenariesHiredCostThisTurn = amount
        If winner.Gold < 0 Then winner.Gold = 0

        AwardMercenariesToPlayer(Form1.CurrentMercOffer, winner)
        'Debug.WriteLine($"Mercenaries hired by Player {winner.PlayerNumber} ({winner.Race}) for {amount} gold!")

        ' === 6. Market inflation: raise price level by 50 ===
        MercPriceLevel += 50
        'If MercPriceLevel > 2000 Then MercPriceLevel = 2000  ' Don't want a cap.

        ' === 7. Clear current offer ===
        Form1.CurrentMercOffer = Nothing
    End Sub

    Public Sub ResolveBiddingPhase()
        ' Resolve last turn's mercenary auction
        If Form1.CurrentMercOffer IsNot Nothing Then
            Dim bids As New Dictionary(Of Form1.Player, Integer)

            For Each p In CurrentForm.Players
                ' AI generates its bid now if none set
                If p.AIControlled AndAlso p.CurrentBid <= 0 Then
                    p.CurrentBid = GenerateAIBid(p, Form1.CurrentMercOffer)
                End If

                ' Only accept valid bids
                If p.CurrentBid > 0 AndAlso p.CurrentBid <= p.Gold Then
                    ' --- Wage cap check ---
                    If CanAffordMercenaries(p, Form1.CurrentMercOffer) Then
                        bids(p) = p.CurrentBid
                    Else
                        Debug.WriteLine($"[MERC] {p.Race} (Player {p.PlayerNumber + 1}) bid {p.CurrentBid} but cannot afford wages. Bid rejected.")
                    End If
                End If
            Next

            ResolveMercenaryAuction(bids)

            ' Reset bids
            For Each p In Form1.Players
                p.CurrentBid = 0
            Next
        End If
    End Sub

    Private Function CanAffordMercenaries(p As Form1.Player, offer As Form1.MercenaryArmy) As Boolean
        If p Is Nothing OrElse offer Is Nothing Then Return False

        ' Current ongoing wages for this player
        Dim currentWages As Integer = CalculateMercenaryWages(p)

        ' Wages for the offered army (sum of Power × Count)
        Dim newWages As Integer = 0
        For Each s In offer.Units
            If s Is Nothing Then Continue For

            If s.Hero IsNot Nothing Then
                ' Hero wage = their Power value
                ' newWages += s.Hero.Power
            ElseIf s.Template IsNot Nothing Then
                newWages += s.Template.Power * s.Count
            End If
        Next

        ' Projected total wages if this army is added
        Dim projected As Integer = currentWages + newWages

        ' Gold income per turn = population / 10
        Dim income As Integer = p.Population \ 10

        ' Can only afford if projected wages fit into income
        Return projected <= income
    End Function

    Private Function GenerateAIBid(p As Form1.Player, offer As Form1.MercenaryArmy) As Integer
        If p Is Nothing OrElse offer Is Nothing OrElse offer.Units.Count = 0 Then
            Return 0
        End If

        ' === 1. Skip if broke ===
        If p.Gold <= 0 Then Return 0

        Dim rnd As New Random()

        ' === 2. Enforce minimum bid requirement ===
        Dim minBid As Integer = Math.Max(offer.MinBid, 1)

        ' If AI cannot meet the minimum bid, skip bidding
        If p.Gold < minBid Then
            'Debug.WriteLine($"[MERC] {p.Race} (Player {p.PlayerNumber + 1}) cannot afford MinBid {minBid}.")
            Return 0
        End If

        ' === 3. Add a small random aggression multiplier (10–30%) ===
        Dim aggression As Double = 1.0 + (rnd.NextDouble() * 0.3)  ' between 1.0 and 1.3
        Dim desiredBid As Integer = CInt(minBid * aggression)

        ' === 4. Cap at available gold ===
        Dim finalBid As Integer = Math.Min(desiredBid, p.Gold)

        ' Ensure at least the MinBid
        If finalBid < minBid Then finalBid = minBid

        'Debug.WriteLine($"[MERC] {p.Race} (Player {p.PlayerNumber + 1}) bids {finalBid} gold (MinBid {minBid}, aggression {aggression:F2}).")

        Return finalBid
    End Function

    Public Function CalculateMercenaryWages(p As Form1.Player) As Integer
        If p Is Nothing OrElse p.Armies Is Nothing Then Return 0

        Dim wages As Integer = 0

        For Each a In p.Armies
            If a.Units Is Nothing Then Continue For
            For Each u In a.Units
                If u IsNot Nothing AndAlso u.IsMercenary Then
                    wages += u.Power * u.Size
                    'Debug.WriteLine($"[WAGE-CHECK] {p.Race} army at ({a.X},{a.Y}): {u.Name} x{u.Size} costing {u.Power * u.Size}")
                End If
            Next
        Next

        Return wages
    End Function

    Public Sub PayMercenaryWages()
        For Each p In CurrentForm.Players
            If Not CurrentForm.CanAct(p) Then Continue For
            Dim wages As Integer = CalculateMercenaryWages(p)
            p.LastMercWages = wages

            If wages > 0 Then
                p.Gold -= wages
                p.WagesPaidThisTurn = wages
                If p.Gold < 0 Then p.Gold = 0 ' prevent negative gold

                'Debug.WriteLine($"[WAGES] {p.Race} (Player {p.PlayerNumber + 1}) paid {wages} gold in mercenary wages. Remaining gold: {p.Gold}")
            End If
        Next
    End Sub

    ' === Helper: calculate wages for a mercenary offer before hire ===
    Public Function CalculateMercenaryOfferWages(offer As Form1.MercenaryArmy) As Integer
        If offer Is Nothing OrElse offer.Units Is Nothing Then Return 0

        Dim wages As Integer = 0
        For Each s In offer.Units
            If s Is Nothing Then Continue For

            If s.Hero IsNot Nothing Then
                wages += s.Hero.Power
            ElseIf s.Template IsNot Nothing Then
                wages += s.Template.Power * s.Count
            End If
        Next

        Return wages
    End Function

End Module
