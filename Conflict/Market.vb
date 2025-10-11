
Imports Conflict.Form1
Imports Conflict.UIModule
Module Market

    Public TheMarket As New Market()

    ' === Market Class (nested) ===
    Public Class Market
        ' Prices (Double so we can use decimals)
        Public Property GemPrice As Double = 20.0
        Public Property AmberPrice As Double = 15.0
        Public Property WinePrice As Double = 10.0
        Public Property FurPrice As Double = 5.0
        Public Property IronPrice As Double = 2.0
        Public Property WoodPrice As Double = 2.0

        ' Optional: track history or supply/demand stats later
        ' Public Property TradeLog As New List(Of String)

        ' === Market trading ===
        Private Const FeeRate As Double = 0.1 ' 10% fee on trades

        Public Sub BuyGoods(p As Player, good As String, amount As Integer)
            If p Is Nothing OrElse amount <= 0 OrElse p.IsEliminated Then Exit Sub

            Dim price As Double = GetPrice(good)
            Dim cost As Double = price * amount
            cost *= (1 + FeeRate) ' add fee

            If p.Gold < cost Then
                'Debug.WriteLine($"[MARKET] {p.Race} cannot afford to buy {amount} {good}. Needed {cost}, has {p.Gold}")
                Exit Sub
            End If

            p.Gold -= CInt(Math.Round(cost))

            Select Case good.ToLower()
                Case "gems" : p.Gems += amount
                Case "amber" : p.Amber += amount
                Case "wine" : p.Wine += amount
                Case "furs" : p.Furs += amount
                Case "iron" : p.Iron += amount
                Case "wood" : p.Wood += amount
            End Select

            demand(good) += amount

            'Debug.WriteLine($"[MARKET] {p.Race} bought {amount} {good} for {Math.Round(cost, 2)} gold.")
        End Sub

        Public Sub SellGoods(p As Player, good As String, amount As Integer)
            If p Is Nothing OrElse amount <= 0 Then Exit Sub

            ' check stock
            Dim hasEnough As Boolean = False
            Select Case good.ToLower()
                Case "gems" : hasEnough = (p.Gems >= amount)
                Case "amber" : hasEnough = (p.Amber >= amount)
                Case "wine" : hasEnough = (p.Wine >= amount)
                Case "furs" : hasEnough = (p.Furs >= amount)
                Case "iron" : hasEnough = (p.Iron >= amount)
                Case "wood" : hasEnough = (p.Wood >= amount)
            End Select

            If Not hasEnough Then
                'Debug.WriteLine($"[MARKET] {p.Race} tried to sell {amount} {good} but doesn’t have enough.")
                Exit Sub
            End If

            Dim price As Double = GetPrice(good)
            Dim gross As Double = price * amount
            gross *= (1 - FeeRate) ' subtract fee

            p.Gold += CInt(Math.Round(gross))

            Select Case good.ToLower()
                Case "gems" : p.Gems -= amount
                Case "amber" : p.Amber -= amount
                Case "wine" : p.Wine -= amount
                Case "furs" : p.Furs -= amount
                Case "iron" : p.Iron -= amount
                Case "wood" : p.Wood -= amount
            End Select

            supply(good) += amount

            'Debug.WriteLine($"[MARKET] {p.Race} sold {amount} {good} for {Math.Round(gross, 2)} gold.")
        End Sub

        Public Function GetPrice(good As String) As Double
            Select Case good.ToLower()
                Case "gems" : Return GemPrice
                Case "amber" : Return AmberPrice
                Case "wine" : Return WinePrice
                Case "furs" : Return FurPrice
                Case "iron" : Return IronPrice
                Case "wood" : Return WoodPrice
                Case Else : Return 1.0
            End Select
        End Function


        Private Function AdjustPrice(oldPrice As Double, demand As Integer, supply As Integer, factor As Integer) As Double
            Dim change As Double = (demand - supply) / factor
            Dim newPrice As Double = oldPrice * (1 + change)
            newPrice = Math.Max(oldPrice * 0.75, Math.Min(oldPrice * 1.25, newPrice))
            Return Math.Round(newPrice, 2) ' 2 decimal places
        End Function

        ' Track per-turn trade volumes
        Private demand As New Dictionary(Of String, Integer)(StringComparer.OrdinalIgnoreCase) From {
            {"gems", 0}, {"amber", 0}, {"wine", 0}, {"furs", 0}, {"iron", 0}, {"wood", 0}
}

        Private supply As New Dictionary(Of String, Integer)(StringComparer.OrdinalIgnoreCase) From {
            {"gems", 0}, {"amber", 0}, {"wine", 0}, {"furs", 0}, {"iron", 0}, {"wood", 0}
}

        Public Sub ResetTradeLogs()
            For Each key In demand.Keys.ToList()
                demand(key) = 0
                supply(key) = 0
            Next
        End Sub
        Public Sub UpdatePrices()
            GemPrice = AdjustPrice(GemPrice, demand("gems"), supply("gems"), 500)
            AmberPrice = AdjustPrice(AmberPrice, demand("amber"), supply("amber"), 500)
            WinePrice = AdjustPrice(WinePrice, demand("wine"), supply("wine"), 500)
            FurPrice = AdjustPrice(FurPrice, demand("furs"), supply("furs"), 500)
            IronPrice = AdjustPrice(IronPrice, demand("iron"), supply("iron"), 2000)
            WoodPrice = AdjustPrice(WoodPrice, demand("wood"), supply("wood"), 2000)
        End Sub

        Public Function DebugDemand() As Dictionary(Of String, Integer)
            Return New Dictionary(Of String, Integer)(demand)
        End Function

        Public Function DebugSupply() As Dictionary(Of String, Integer)
            Return New Dictionary(Of String, Integer)(supply)
        End Function

    End Class

    Public Sub AIHandleMarketTurn(players As List(Of Player))
        Dim Rnd As New Random()
        For Each p In players
            If p Is Nothing OrElse Not p.AIControlled OrElse p.IsEliminated Then Continue For

            Dim log As String = $"{p.Race} (Player {p.PlayerNumber + 1}):" & vbCrLf
            Dim transactionDone As Boolean = False

            ' ===========================================================
            ' === STEP 1: Check next-turn resource needs for recruitment ==
            ' ===========================================================
            Dim expectedRecruits As Integer = 0
            For Each a In p.Armies
                If a.TotalSoldiers < 500 Then
                    expectedRecruits += Math.Min(CInt(p.Population * 0.05), 500 - a.TotalSoldiers)
                End If
            Next

            ' Estimate rough material requirements
            Dim avgIronCost As Double = 0.5
            Dim avgWoodCost As Double = 0.3
            Dim ironNeeded As Integer = CInt(expectedRecruits * avgIronCost)
            Dim woodNeeded As Integer = CInt(expectedRecruits * avgWoodCost)

            Dim ironShort As Integer = Math.Max(0, ironNeeded - p.Iron)
            Dim woodShort As Integer = Math.Max(0, woodNeeded - p.Wood)

            ' ===========================================================
            ' === STEP 2: Prioritise buying missing iron/wood ===========
            ' ===========================================================
            If (ironShort > 0 OrElse woodShort > 0) AndAlso p.Gold > 0 Then
                Dim ironRatio As Double = If(ironNeeded > 0, p.Iron / Math.Max(1, ironNeeded), 1)
                Dim woodRatio As Double = If(woodNeeded > 0, p.Wood / Math.Max(1, woodNeeded), 1)

                Dim buyGood As String = If(ironRatio < woodRatio, "iron", "wood")
                Dim shortage As Integer = If(buyGood = "iron", ironShort, woodShort)
                Dim price As Double = TheMarket.GetPrice(buyGood)
                Dim maxAffordable As Integer = CInt(p.Gold / (price * 1.1)) ' 10% fee margin
                Dim amountToBuy As Integer = Math.Min(shortage, Math.Min(2000, maxAffordable))

                If amountToBuy > 0 Then
                    TheMarket.BuyGoods(p, buyGood, amountToBuy)
                    log &= $"   [Prep] Bought {amountToBuy} {buyGood} to cover next-turn recruits." & vbCrLf
                    transactionDone = True
                End If
            End If

            ' ===========================================================
            ' === STEP 3: Normal market behaviour (only if free) ========
            ' ===========================================================
            If Not transactionDone Then
                Dim roll As Integer = Rnd.Next(100)
                Dim didSomething As Boolean = False

                ' === SELL logic ===
                Dim sellChance As Integer = 20
                If p.Gold < 500 Then sellChance += 30
                If TheMarket.GemPrice > 25 OrElse TheMarket.AmberPrice > 20 OrElse
               TheMarket.WinePrice > 15 OrElse TheMarket.FurPrice > 10 Then
                    sellChance += 20
                End If

                If roll < sellChance Then
                    Dim choice As String = PickTradeGoodForRace(p.Race)
                    Dim stock As Integer = GetPlayerStock(p, choice)
                    If stock > 0 Then
                        Dim amount As Integer = Math.Min(stock, Rnd.Next(5, 16))
                        TheMarket.SellGoods(p, choice, amount)
                        log &= $"   Sold {amount} {choice} at {TheMarket.GetPrice(choice):F2}" & vbCrLf
                        didSomething = True
                    End If
                End If

                ' === BUY logic ===
                If Not didSomething Then
                    roll = Rnd.Next(100)
                    Dim buyChance As Integer = 10
                    If TheMarket.GemPrice < 15 OrElse TheMarket.AmberPrice < 12 OrElse
                   TheMarket.WinePrice < 8 OrElse TheMarket.FurPrice < 4 Then
                        buyChance += 30
                    End If
                    If p.Gold > 1000 Then buyChance += 20

                    If roll < buyChance Then
                        Dim goods() As String = {"gems", "amber", "wine", "furs", "iron", "wood"}
                        Dim choice As String = goods(Rnd.Next(goods.Length))
                        Dim budget As Integer = Math.Min(p.Gold \ 5, Rnd.Next(50, 151))
                        If budget > 0 Then
                            Dim price As Double = TheMarket.GetPrice(choice)
                            Dim amount As Integer = CInt(Math.Floor(budget / price))
                            If amount > 0 Then
                                TheMarket.BuyGoods(p, choice, amount)
                                log &= $"   Bought {amount} {choice} at {TheMarket.GetPrice(choice):F2}" & vbCrLf
                                didSomething = True
                            End If
                        End If
                    End If
                End If

                If Not didSomething Then
                    log &= "   No trades this turn." & vbCrLf
                End If

                transactionDone = True
            End If

        Next
    End Sub


    Private Function PickTradeGoodForRace(race As String) As String
        Select Case race.ToLower()
            Case "dwarf" : Return "gems"
            Case "elf" : Return "amber"
            Case "human" : Return "wine"
            Case "orc" : Return "furs"
            Case Else : Return "gems"
        End Select
    End Function

    Private Function GetPlayerStock(p As Player, good As String) As Integer
        Select Case good.ToLower()
            Case "gems" : Return p.Gems
            Case "amber" : Return p.Amber
            Case "wine" : Return p.Wine
            Case "furs" : Return p.Furs
            Case "iron" : Return p.Iron
            Case "wood" : Return p.Wood
            Case Else : Return 0
        End Select
    End Function

    Public Sub ApplyMarketTransactions()
        ' Safety check
        If CurrentForm Is Nothing OrElse CurrentForm.Players Is Nothing Then Exit Sub

        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For

            Dim suffix As String = Char.ToUpper(p.Race(0)) & p.Race.Substring(1).ToLower()

            Dim cmb As ComboBox = TryCast(CurrentForm.Controls("cmbMarketResource" & suffix), ComboBox)
            Dim num As NumericUpDown = TryCast(CurrentForm.Controls("numMarketAmount" & suffix), NumericUpDown)

            If cmb Is Nothing OrElse num Is Nothing Then Continue For

            Dim good As String = If(cmb.SelectedItem, "").ToString().Trim()
            Dim amt As Integer = CInt(num.Value)

            If String.IsNullOrWhiteSpace(good) OrElse amt = 0 Then Continue For

            If amt > 0 Then
                TheMarket.BuyGoods(p, good, amt)
            Else
                TheMarket.SellGoods(p, good, Math.Abs(amt))
            End If

            ' Reset the controls
            num.Value = 0
            cmb.SelectedIndex = -1
        Next
    End Sub



End Module
