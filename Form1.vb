Option Strict On
Option Explicit On

Imports System.Drawing.Printing
Imports System.Reflection
Imports System.Linq

Public Class Form1

#Region "=== Constants and Enums ==="

    Private Const Rows As Integer = 25
    Private Const Cols As Integer = 25
    Private Const InitialSquaresPerPlayer As Integer = 50
    Private Const LegendSpriteSize As Single = 32
    Private Const LegendLabelSpacing As Single = 10
    Private Const GridFontSize As Single = 6

    Private Enum TerrainType
        Plains
        Forest
        Hills
        Mountains
    End Enum

    Public Enum ResourceType
        Food
        Iron
        Timber
    End Enum

#End Region

#Region "=== Nested Classes ==="

    Public Class Army
        Public Property Position As Point
        Public Property Soldiers As Integer
        Public Property Weapons As Integer

        Public ReadOnly Property Strength As Integer
            Get
                Return Soldiers + (Weapons * 2)
            End Get
        End Property

        Public Sub New(startPos As Point)
            Position = startPos
            Soldiers = 0
            Weapons = 0
        End Sub
    End Class

    Public Class PlayerResources
        ' --- Use fields for dictionaries ---
        Public Current As Dictionary(Of ResourceType, Integer)
        Public CollectedThisTurn As Dictionary(Of ResourceType, Integer)

        ' --- Population ---
        Public Property Population As Integer

        ' --- Reporting fields for food upkeep ---
        Public LastPopulationUpkeep As Integer
        Public LastArmyUpkeep As Integer
        Public LastFoodAfterUpkeep As Integer

        ' --- Constructor ---
        Public Sub New()
            ' Initialize dictionaries
            Current = New Dictionary(Of ResourceType, Integer) From {
            {ResourceType.Food, 0},
            {ResourceType.Iron, 0},
            {ResourceType.Timber, 0}
        }

            CollectedThisTurn = New Dictionary(Of ResourceType, Integer) From {
            {ResourceType.Food, 0},
            {ResourceType.Iron, 0},
            {ResourceType.Timber, 0}
        }

            ' Set starting population
            Population = 1000

            ' Initialize reporting fields
            LastPopulationUpkeep = 0
            LastArmyUpkeep = 0
            LastFoodAfterUpkeep = 0
        End Sub
    End Class


#End Region

#Region "=== Fields ==="

    Private resourceTable As Dictionary(Of Integer, Dictionary(Of TerrainType, Dictionary(Of ResourceType, Integer)))
    Private playerRaces As String() = {"Elf", "Dwarf", "Orc", "Human"}

    Private playerArmies(3) As Army
    Private playersResources(3) As PlayerResources

    Private terrainMap(Rows - 1, Cols - 1) As TerrainType
    Private playerMap(Rows - 1, Cols - 1) As Integer

    Private playerSquares As List(Of Point)() = {
        New List(Of Point)(), New List(Of Point)(),
        New List(Of Point)(), New List(Of Point)()
    }

    Private terrainSprites As Dictionary(Of TerrainType, Image)

    Private playerColors As Color() = {
        Color.LightGreen,   ' Elf
        Color.LightSkyBlue, ' Dwarf
        Color.LightCoral,   ' Orc
        Color.Yellow        ' Human
    }

    Private WithEvents printDoc As New PrintDocument

    Private currentPlayerToPrint As Integer = 0 ' default = Elf (player 0)

    Private lastTurnFoodUpkeep(3) As (PopulationUpkeep As Integer, ArmyUpkeep As Integer, Collected As Integer, Remaining As Integer)

    Private lastTurnPopulationGrowth(3) As Integer


#End Region

#Region "=== Form Lifecycle ==="

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        terrainSprites = LoadAllTerrainSprites()
        GenerateTerrainMap()
        InitializePlayerMap()

        For i As Integer = 0 To 3
            playersResources(i) = New PlayerResources()
        Next

        InitializeResourceTable()
        InitializeArmies()
    End Sub

#End Region

#Region "=== Initialization ==="

    Private Sub InitializeResourceTable()
        resourceTable = New Dictionary(Of Integer, Dictionary(Of TerrainType, Dictionary(Of ResourceType, Integer)))()

        ' --- Elf ---
        resourceTable(0) = New Dictionary(Of TerrainType, Dictionary(Of ResourceType, Integer)) From {
            {TerrainType.Plains, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 0}, {ResourceType.Timber, 0}}},
            {TerrainType.Hills, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 1}, {ResourceType.Timber, 1}}},
            {TerrainType.Mountains, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 2}, {ResourceType.Timber, 1}}},
            {TerrainType.Forest, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 5}, {ResourceType.Iron, 0}, {ResourceType.Timber, 3}}}
        }

        ' --- Dwarf ---
        resourceTable(1) = New Dictionary(Of TerrainType, Dictionary(Of ResourceType, Integer)) From {
            {TerrainType.Plains, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 1}, {ResourceType.Timber, 0}}},
            {TerrainType.Hills, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 1}, {ResourceType.Timber, 1}}},
            {TerrainType.Mountains, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 5}, {ResourceType.Iron, 3}, {ResourceType.Timber, 0}}},
            {TerrainType.Forest, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 0}, {ResourceType.Timber, 1}}}
        }

        ' --- Orc ---
        resourceTable(2) = New Dictionary(Of TerrainType, Dictionary(Of ResourceType, Integer)) From {
            {TerrainType.Plains, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 0}, {ResourceType.Timber, 0}}},
            {TerrainType.Hills, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 5}, {ResourceType.Iron, 2}, {ResourceType.Timber, 1}}},
            {TerrainType.Mountains, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 2}, {ResourceType.Timber, 0}}},
            {TerrainType.Forest, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 0}, {ResourceType.Timber, 2}}}
        }

        ' --- Human ---
        resourceTable(3) = New Dictionary(Of TerrainType, Dictionary(Of ResourceType, Integer)) From {
            {TerrainType.Plains, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 5}, {ResourceType.Iron, 0}, {ResourceType.Timber, 0}}},
            {TerrainType.Hills, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 2}, {ResourceType.Timber, 1}}},
            {TerrainType.Mountains, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 2}, {ResourceType.Timber, 0}}},
            {TerrainType.Forest, New Dictionary(Of ResourceType, Integer) From {{ResourceType.Food, 2}, {ResourceType.Iron, 0}, {ResourceType.Timber, 2}}}
        }
    End Sub

    Private Sub InitializeArmies()
        ' Place each army in the corner start position and assign 1000 soldiers
        playerArmies(0) = New Army(New Point(0, 0))                ' Elf
        playerArmies(0).Soldiers = 1000

        playerArmies(1) = New Army(New Point(Cols - 1, 0))         ' Dwarf
        playerArmies(1).Soldiers = 1000

        playerArmies(2) = New Army(New Point(0, Rows - 1))         ' Orc
        playerArmies(2).Soldiers = 1000

        playerArmies(3) = New Army(New Point(Cols - 1, Rows - 1))  ' Human
        playerArmies(3).Soldiers = 1000
    End Sub


#End Region

#Region "=== Gameplay Logic ==="

    Private Sub CollectResources()
        ' --- Reset "collected this turn" and growth for all players ---
        For i As Integer = 0 To 3
            For Each res As ResourceType In [Enum].GetValues(GetType(ResourceType))
                playersResources(i).CollectedThisTurn(res) = 0
            Next
            lastTurnFoodUpkeep(i) = (PopulationUpkeep:=0, ArmyUpkeep:=0, Collected:=0, Remaining:=0)
            lastTurnPopulationGrowth(i) = 0
        Next

        ' --- Loop through all map cells to collect resources ---
        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                Dim playerId As Integer = playerMap(r, c)
                If playerId >= 0 Then
                    Dim terrain As TerrainType = terrainMap(r, c)

                    ' Calculate number of collectors per square: total pop / number of squares
                    Dim numSquares As Integer = playerSquares(playerId).Count
                    If numSquares = 0 Then Continue For
                    Dim collectorsPerSquare As Double = playersResources(playerId).Population / numSquares

                    ' Lookup resources in the table
                    If resourceTable.ContainsKey(playerId) AndAlso resourceTable(playerId).ContainsKey(terrain) Then
                        For Each kvp As KeyValuePair(Of ResourceType, Integer) In resourceTable(playerId)(terrain)
                            ' Multiply collection rate by number of collectors
                            Dim totalCollected As Integer = CInt(kvp.Value * collectorsPerSquare)
                            AddResource(playerId, kvp.Key, totalCollected)
                        Next
                    End If
                End If
            Next
        Next

        ' --- Calculate food upkeep for population and army, and leftover for growth ---
        For i As Integer = 0 To 3
            Dim popUpkeep As Integer = playersResources(i).Population
            Dim armyUpkeep As Integer = playerArmies(i).Soldiers
            Dim totalFoodRequired As Integer = popUpkeep + armyUpkeep

            Dim collectedFood As Integer = playersResources(i).CollectedThisTurn(ResourceType.Food)
            Dim remainingFood As Integer = collectedFood - totalFoodRequired
            If remainingFood < 0 Then remainingFood = 0

            ' Deduct upkeep from current food
            playersResources(i).Current(ResourceType.Food) -= totalFoodRequired
            If playersResources(i).Current(ResourceType.Food) < 0 Then playersResources(i).Current(ResourceType.Food) = 0

            ' Store values for reporting
            lastTurnFoodUpkeep(i) = (PopulationUpkeep:=popUpkeep, ArmyUpkeep:=armyUpkeep, Collected:=collectedFood, Remaining:=remainingFood)

            ' Convert leftover food into new population: 1 new pop per 10 food
            Dim newPop As Integer = remainingFood \ 10
            playersResources(i).Population += newPop
            lastTurnPopulationGrowth(i) = newPop
        Next
    End Sub


    Private Sub ApplyFoodUpkeepAndGrowth()
        For playerId As Integer = 0 To 3
            Dim pr As PlayerResources = playersResources(playerId)
            Dim army As Army = playerArmies(playerId)

            Dim totalUpkeep As Integer = pr.Population + army.Soldiers
            Dim foodCollected As Integer = pr.CollectedThisTurn(ResourceType.Food)

            ' --- Subtract upkeep ---
            Dim remainingFood As Integer = foodCollected - totalUpkeep
            If remainingFood < 0 Then remainingFood = 0  ' cannot go negative

            ' --- Update Current food total after upkeep ---
            pr.Current(ResourceType.Food) = remainingFood

            ' --- Record upkeep for reporting ---
            pr.LastPopulationUpkeep = pr.Population
            pr.LastArmyUpkeep = army.Soldiers
            pr.LastFoodAfterUpkeep = remainingFood

            ' --- Convert leftover food into new population ---
            Dim newPopulation As Integer = remainingFood \ 10   ' integer division
            pr.Population += newPopulation
        Next
    End Sub



    Private Sub AddResource(playerId As Integer, resType As ResourceType, amount As Integer)
        playersResources(playerId).Current(resType) += amount
        playersResources(playerId).CollectedThisTurn(resType) += amount
    End Sub

    Private Sub UpdatePlayerSquares()
        For playerId As Integer = 0 To 3
            playerSquares(playerId).Clear()
        Next

        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                Dim playerId As Integer = playerMap(r, c)
                If playerId >= 0 Then
                    playerSquares(playerId).Add(New Point(c, r))
                End If
            Next
        Next
    End Sub

#End Region

#Region "=== Map Generation ==="

    Private Sub GenerateTerrainMap()
        Dim rnd As New Random()
        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                terrainMap(r, c) = CType(rnd.Next(0, 4), TerrainType)
            Next
        Next
    End Sub

    Private Sub InitializePlayerMap()
        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                playerMap(r, c) = -1
            Next
        Next

        Dim rnd As New Random()
        Dim ClaimOrganic = Sub(startRow As Integer, startCol As Integer, playerId As Integer, count As Integer)
                               Dim frontier As New List(Of Point) From {New Point(startCol, startRow)}
                               Dim claimed As Integer = 0

                               While claimed < count AndAlso frontier.Count > 0
                                   Dim idx As Integer = rnd.Next(frontier.Count)
                                   Dim cell As Point = frontier(idx)
                                   frontier.RemoveAt(idx)

                                   If playerMap(cell.Y, cell.X) = -1 Then
                                       playerMap(cell.Y, cell.X) = playerId
                                       claimed += 1

                                       Dim directions As Point() = {
                                           New Point(0, -1), New Point(1, 0),
                                           New Point(0, 1), New Point(-1, 0)
                                       }

                                       For Each d As Point In directions
                                           Dim nr As Integer = cell.Y + d.Y
                                           Dim nc As Integer = cell.X + d.X
                                           If nr >= 0 AndAlso nr < Rows AndAlso nc >= 0 AndAlso nc < Cols Then
                                               Dim neighbor As New Point(nc, nr)
                                               If playerMap(nr, nc) = -1 AndAlso Not frontier.Contains(neighbor) Then
                                                   frontier.Add(neighbor)
                                               End If
                                           End If
                                       Next
                                   End If
                               End While
                           End Sub

        ClaimOrganic(0, 0, 0, InitialSquaresPerPlayer)
        ClaimOrganic(0, Cols - 1, 1, InitialSquaresPerPlayer)
        ClaimOrganic(Rows - 1, 0, 2, InitialSquaresPerPlayer)
        ClaimOrganic(Rows - 1, Cols - 1, 3, InitialSquaresPerPlayer)

        UpdatePlayerSquares()
    End Sub

#End Region

#Region "=== Drawing and Printing ==="

    Private Function LoadAllTerrainSprites() As Dictionary(Of TerrainType, Image)
        Return New Dictionary(Of TerrainType, Image) From {
            {TerrainType.Plains, LoadEmbeddedImageByName("plains.png")},
            {TerrainType.Forest, LoadEmbeddedImageByName("forest.png")},
            {TerrainType.Hills, LoadEmbeddedImageByName("hills.png")},
            {TerrainType.Mountains, LoadEmbeddedImageByName("mountain.png")}
        }
    End Function

    Private Function LoadEmbeddedImageByName(fileName As String) As Image
        Dim asm As Reflection.Assembly = Reflection.Assembly.GetExecutingAssembly()
        Dim fullName As String = asm.GetManifestResourceNames().
            FirstOrDefault(Function(n) n.EndsWith(fileName, StringComparison.OrdinalIgnoreCase))

        If fullName IsNot Nothing Then
            Using stream = asm.GetManifestResourceStream(fullName)
                Return Image.FromStream(stream)
            End Using
        Else
            Throw New Exception("Embedded resource not found: " & fileName)
        End If
    End Function

    Private Sub btnPrint_Click(sender As Object, e As EventArgs) Handles btnPrint.Click
        Using dlg As New PrintDialog()
            dlg.Document = printDoc
            If dlg.ShowDialog() = DialogResult.OK Then
                printDoc.Print()
            End If
        End Using
    End Sub


    Private Sub DrawGridNumbers(g As Graphics, startX As Single, startY As Single, cellSize As Single)
        Using font As New Font("Arial", GridFontSize)
            ' Columns
            For c As Integer = 0 To Cols - 1
                Dim numX As Single = startX + c * cellSize + cellSize / 2
                Dim numY As Single = startY - 12
                g.DrawString((c + 1).ToString(), font, Brushes.Black, numX, numY,
                             New StringFormat() With {.Alignment = StringAlignment.Center})
            Next

            ' Rows
            For r As Integer = 0 To Rows - 1
                Dim numX As Single = startX - 12
                Dim numY As Single = startY + r * cellSize + cellSize / 2 - 4
                g.DrawString((r + 1).ToString(), font, Brushes.Black, numX, numY,
                             New StringFormat() With {.Alignment = StringAlignment.Far})
            Next
        End Using
    End Sub

    Private Sub DrawPlayerTerrains(g As Graphics, startX As Single, startY As Single, cellSize As Single)
        ' Render quality
        g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
        g.InterpolationMode = Drawing2D.InterpolationMode.HighQualityBicubic
        g.PixelOffsetMode = Drawing2D.PixelOffsetMode.Half

        Dim gridPen As New Pen(Color.LightGray, 1.5F)
        Dim borderPen As New Pen(Color.Black, 3) With {.Alignment = Drawing2D.PenAlignment.Inset}

        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                Dim cellX As Single = startX + c * cellSize
                Dim cellY As Single = startY + r * cellSize
                Dim playerId As Integer = playerMap(r, c)

                ' Player-colored background (semi-transparent)
                If playerId >= 0 Then
                    Using brush As New SolidBrush(Color.FromArgb(160, playerColors(playerId)))
                        g.FillRectangle(brush, cellX, cellY, cellSize, cellSize)
                    End Using
                End If

                ' Thin grid
                g.DrawRectangle(gridPen, cellX, cellY, cellSize, cellSize)

                ' Terrain sprite (with small padding)
                Dim terrain As TerrainType = terrainMap(r, c)
                Dim sprite As Image = terrainSprites(terrain)
                Dim padding As Single = 2
                Dim drawWidth As Single = Math.Max(0.0F, cellSize - 2 * padding)
                Dim drawHeight As Single = Math.Max(0.0F, cellSize - 2 * padding)
                g.DrawImage(sprite, cellX + padding, cellY + padding, drawWidth, drawHeight)

                ' Empire borders (only where adjacent cell not same owner)
                If playerId >= 0 Then
                    Dim halfPen As Single = borderPen.Width / 2
                    Dim drawTop As Boolean = (r = 0 OrElse playerMap(r - 1, c) <> playerId)
                    Dim drawLeft As Boolean = (c = 0 OrElse playerMap(r, c - 1) <> playerId)
                    Dim drawRight As Boolean = (c = Cols - 1 OrElse playerMap(r, c + 1) <> playerId)
                    Dim drawBottom As Boolean = (r = Rows - 1 OrElse playerMap(r + 1, c) <> playerId)

                    If drawTop Then g.DrawLine(borderPen, cellX + halfPen, cellY + halfPen, cellX + cellSize - halfPen, cellY + halfPen)
                    If drawLeft Then g.DrawLine(borderPen, cellX + halfPen, cellY + halfPen, cellX + halfPen, cellY + cellSize - halfPen)
                    If drawRight Then g.DrawLine(borderPen, cellX + cellSize - halfPen, cellY + halfPen, cellX + cellSize - halfPen, cellY + cellSize - halfPen)
                    If drawBottom Then g.DrawLine(borderPen, cellX + halfPen, cellY + cellSize - halfPen, cellX + cellSize - halfPen, cellY + cellSize - halfPen)
                End If
            Next
        Next
    End Sub

    Private Sub DrawLegend(g As Graphics, startX As Single, startY As Single, gridWidth As Single, gridHeight As Single)
        Using font As New Font("Arial", GridFontSize)
            Dim legendItems As New List(Of Tuple(Of Image, String))
            For Each kvp As KeyValuePair(Of TerrainType, Image) In terrainSprites
                legendItems.Add(Tuple.Create(kvp.Value, kvp.Key.ToString()))
            Next

            Dim totalLegendWidth As Single = legendItems.Count * LegendSpriteSize + (legendItems.Count - 1) * LegendLabelSpacing
            Dim legendStartX As Single = startX + (gridWidth - totalLegendWidth) / 2
            Dim legendStartY As Single = startY + gridHeight + 20

            For i As Integer = 0 To legendItems.Count - 1
                Dim img As Image = legendItems(i).Item1
                Dim text As String = legendItems(i).Item2
                Dim x As Single = legendStartX + i * (LegendSpriteSize + LegendLabelSpacing)
                g.DrawImage(img, x, legendStartY, LegendSpriteSize, LegendSpriteSize)
                g.DrawString(text, font, Brushes.Black, x + LegendSpriteSize / 2, legendStartY + LegendSpriteSize + 2, New StringFormat() With {.Alignment = StringAlignment.Center})
            Next
        End Using
    End Sub

    Private Sub DrawEmpireSummary(g As Graphics, playerId As Integer, startX As Single, startY As Single, gridWidth As Single, gridHeight As Single)
        Dim raceName As String = playerRaces(playerId).ToString()
        Dim playerName As String = $"Player {playerId + 1} ({raceName})"

        ' --- calculate empire size and terrain counts ---
        Dim terrainCounts As New Dictionary(Of TerrainType, Integer) From {
        {TerrainType.Plains, 0},
        {TerrainType.Hills, 0},
        {TerrainType.Mountains, 0},
        {TerrainType.Forest, 0}
    }

        Dim empireSize As Integer = 0
        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                If playerMap(r, c) = playerId Then
                    empireSize += 1
                    terrainCounts(terrainMap(r, c)) += 1
                End If
            Next
        Next

        ' --- resources ---
        Dim resources = playersResources(playerId)

        ' --- layout ---
        Dim font As New Font("Arial", 10)
        Dim lineHeight As Integer = 18
        Dim textY As Single = startY + gridHeight + 80
        Dim textX As Single = startX

        ' --- header ---
        g.DrawString(playerName & " Empire Report", New Font("Arial", 12, FontStyle.Bold), Brushes.Black, textX, textY)
        textY += lineHeight * 2

        ' --- empire size ---
        g.DrawString($"Empire size: {empireSize} squares", font, Brushes.Black, textX, textY)
        textY += lineHeight

        ' --- terrain breakdown ---
        g.DrawString("Terrains:", font, Brushes.Black, textX, textY)
        textY += lineHeight
        For Each kvp In terrainCounts
            g.DrawString($"{kvp.Key}: {kvp.Value}", font, Brushes.Black, textX + 20, textY)
            textY += lineHeight
        Next

        ' --- resources ---
        g.DrawString("Resources:", font, Brushes.Black, textX, textY)
        textY += lineHeight
        For Each resType As ResourceType In [Enum].GetValues(GetType(ResourceType))
            g.DrawString($"{resType}: {resources.Current(resType)} (collected this turn: {resources.CollectedThisTurn(resType)})",
                     font, Brushes.Black, textX + 20, textY)
            textY += lineHeight
        Next

        ' --- population ---
        g.DrawString($"Population: {resources.Population}", font, Brushes.Black, textX, textY)
    End Sub

    Private Sub printDoc_PrintPage(sender As Object, e As PrintPageEventArgs) Handles printDoc.PrintPage
        Dim g As Graphics = e.Graphics
        Dim bounds As RectangleF = e.MarginBounds
        Dim cellSize As Single = bounds.Width / Cols
        Dim gridWidth As Single = cellSize * Cols
        Dim gridHeight As Single = cellSize * Rows
        Dim startX As Single = bounds.Left
        Dim startY As Single = bounds.Top + 10

        ' Draw the map
        DrawGridNumbers(g, startX, startY, cellSize)
        DrawPlayerTerrains(g, startX, startY, cellSize)
        DrawLegend(g, startX, startY, gridWidth, gridHeight)

        ' Draw empire report for player 0
        Dim reportStartY As Single = startY + gridHeight + LegendSpriteSize + 40
        PrintPlayerEmpire(g, 0, startX, reportStartY)
    End Sub


    Private Sub PrintPlayerEmpire(g As Graphics, playerId As Integer, startX As Single, startY As Single)
        Dim pr As PlayerResources = playersResources(playerId)
        Dim army As Army = playerArmies(playerId)
        Dim font As New Font("Arial", 10)
        Dim brush As Brush = Brushes.Black
        Dim lineHeight As Single = font.GetHeight(g) + 2

        ' Empire size
        g.DrawString($"Empire Size: {playerSquares(playerId).Count} squares", font, brush, startX, startY)
        startY += lineHeight

        ' Terrain counts
        For Each t As TerrainType In [Enum].GetValues(GetType(TerrainType))
            Dim count As Integer = playerSquares(playerId).Where(Function(p) terrainMap(p.Y, p.X) = t).Count()
            g.DrawString($"{t}: {count}", font, brush, startX, startY)
            startY += lineHeight
        Next

        ' Resources collected and used
        Dim lastFood = lastTurnFoodUpkeep(playerId)
        g.DrawString($"Food Collected: {lastFood.Collected}", font, brush, startX, startY)
        startY += lineHeight
        g.DrawString($"Population Upkeep: {lastFood.PopulationUpkeep}", font, brush, startX, startY)
        startY += lineHeight
        g.DrawString($"Soldiers Upkeep: {lastFood.ArmyUpkeep}", font, brush, startX, startY)
        startY += lineHeight
        g.DrawString($"Remaining Food: {lastFood.Remaining}", font, brush, startX, startY)
        startY += lineHeight

        ' Population growth from remaining food
        g.DrawString($"Population Growth: {lastTurnPopulationGrowth(playerId)}", font, brush, startX, startY)
        startY += lineHeight

        ' Current population
        g.DrawString($"Current Population: {pr.Population}", font, brush, startX, startY)
        startY += lineHeight

        ' Army
        g.DrawString($"Army: Soldiers {army.Soldiers}, Weapons {army.Weapons}, Strength {army.Strength}", font, brush, startX, startY)
    End Sub






#End Region


#Region "=== UI Events ==="

    Private Sub btnNextTurn_Click(sender As Object, e As EventArgs) Handles btnNextTurn.Click
        CollectResources()
        ApplyFoodUpkeepAndGrowth()
        DisplayPlayerResources()
    End Sub

    Private Sub DisplayPlayerResources()
        lstResources.Items.Clear()

        For i As Integer = 0 To 3
            Dim pr As PlayerResources = playersResources(i)
            Dim raceName As String = playerRaces(i)
            Dim army As Army = playerArmies(i)

            ' Header line: Player, Race, Population
            lstResources.Items.Add($"Player {i + 1} ({raceName}) - Population: {pr.Population}")

            ' Resources line
            Dim lastFood = lastTurnFoodUpkeep(i)
            lstResources.Items.Add($"  Food Collected: {lastFood.Collected}, Population Upkeep: {lastFood.PopulationUpkeep}, Soldiers Upkeep: {lastFood.ArmyUpkeep}, Remaining: {lastFood.Remaining}")
            ' Population growth
            lstResources.Items.Add($"  Population Growth: {lastTurnPopulationGrowth(i)}")

            ' Army line
            lstResources.Items.Add($"  Army at ({army.Position.X + 1},{army.Position.Y + 1}): Soldiers {army.Soldiers}, Weapons {army.Weapons}, Strength {army.Strength}")

            ' Terrain counts
            lstResources.Items.Add("  Terrain:")
            For Each t As TerrainType In [Enum].GetValues(GetType(TerrainType))
                Dim count As Integer = playerSquares(i).Where(Function(p) terrainMap(p.Y, p.X) = t).Count()
                lstResources.Items.Add($"    {t}: {count} squares")
            Next

            lstResources.Items.Add("") ' blank line between players
        Next
    End Sub


#End Region

End Class
