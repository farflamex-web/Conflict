Option Strict On
Option Explicit On


Imports System.Drawing.Printing
Imports System.IO
Imports System.Linq
Imports System.Reflection

Public Class Form1

#Region "=== Constants and Enums ==="

    Private Const Rows As Integer = 25
    Private Const Cols As Integer = 25
    Private Const LegendSpriteSize As Single = 32
    Private Const LegendLabelSpacing As Single = 10
    Private Const GridFontSize As Single = 6

    Private Map(Rows - 1, Cols - 1, 2) As Integer

    Private Races() As String = {"Elf", "Dwarf", "Orc", "Human"}

    Private terrainCache As New Dictionary(Of String, Image)


#End Region

#Region "=== Nested Classes ==="

    Public Class Army
        Public Property X As Integer                 ' Current X coordinate
        Public Property Y As Integer                 ' Current Y coordinate
        Public Property Soldiers As Integer          ' Number of soldiers
        Public Property Weapons As Integer           ' Number of weapons (future use)

        ' Queue of planned directions for the turn, max 5 moves
        ' Each entry is either a string ("N", "NW", etc.) or a special action ("Recruit", "Train", etc.)
        Public Property MoveQueue As New List(Of String)

        ' Optional flag to track if the special move has been used
        Public Property HasUsedSpecial As Boolean = False
    End Class


    Public Class Player
        Public Property PlayerNumber As Integer
        Public Property Race As String ' Could be created on-the-fly whenever by using 'Races(PlayerNumber)' but storing it in advance for simplicity later.
        Public Property Population As Integer
        Public Property Armies As List(Of Army)
        Public Property Food As Integer
        Public Property FoodCollectedThisTurn As Integer
        Public Property Iron As Integer
        Public Property IronCollectedThisTurn As Integer
        Public Property Wood As Integer
        Public Property WoodCollectedThisTurn As Integer

        Public Property AIControlled As Boolean
    End Class


#End Region

#Region "=== Fields ==="

    Public Players As List(Of Player)

    Private playerColors As Color() = {
        Color.LightGreen,   ' Elf
        Color.LightSkyBlue, ' Dwarf
        Color.LightCoral,   ' Orc
        Color.Yellow        ' Human
    }

    Private WithEvents printDoc As New PrintDocument

#End Region

#Region "=== Form Lifecycle ==="

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        CreateTerrainCache()


        InitializePlayers()

        GenerateMap()


    End Sub

#End Region



#Region "=== Initialization ==="

    Private Sub CreateTerrainCache()
        ' Clear existing cache (in case called multiple times)
        terrainCache.Clear()

        Dim terrainNames As String() = {"plains.png", "forest.png", "hills.png", "mountain.png"}
        For Each terrainName In terrainNames
            Dim img As Image = GetEmbeddedImage(terrainName)
            If img IsNot Nothing Then
                terrainCache.Add(terrainName, img)
            End If
        Next
    End Sub

    Public Sub InitializePlayers()
        Players = New List(Of Player)

        Dim mapWidth As Integer = Map.GetLength(0)
        Dim mapHeight As Integer = Map.GetLength(1)
        Dim cornerRange As Integer = 10 ' spread armies within 10x10 area

        Dim rnd As New Random()

        ' Create players
        For i As Integer = 0 To 3
            Dim p As New Player()
            p.PlayerNumber = i
            p.Race = Races(i)
            p.Population = 5000
            p.Armies = New List(Of Army)()
            p.AIControlled = True

            ' Determine top-left corner for the player's area
            Dim cornerX As Integer = 0
            Dim cornerY As Integer = 0
            Select Case i
                Case 0 ' Elf, top-left
                    cornerX = 0
                    cornerY = 0
                Case 1 ' Dwarf, top-right
                    cornerX = mapWidth - cornerRange
                    cornerY = 0
                Case 2 ' Orc, bottom-left
                    cornerX = 0
                    cornerY = mapHeight - cornerRange
                Case 3 ' Human, bottom-right
                    cornerX = mapWidth - cornerRange
                    cornerY = mapHeight - cornerRange
            End Select

            ' Create 3 armies per player, randomly within their corner area
            For a As Integer = 1 To 3
                Dim army As New Army()
                army.X = cornerX + rnd.Next(0, cornerRange)
                army.Y = cornerY + rnd.Next(0, cornerRange)
                army.Soldiers = 500
                army.Weapons = 0
                p.Armies.Add(army)
            Next

            Players.Add(p)
        Next
    End Sub


#End Region

#Region "=== Process Turn ==="
    Public Sub ProcessTurn()
        Dim maxSteps As Integer = 5
        Dim mapSize As Integer = Map.GetLength(0)

        ' Loop over each move step (1 to 5)
        For stepIndex As Integer = 0 To maxSteps - 1

            ' Loop through all players
            For Each p In Players
                If p.Armies IsNot Nothing Then
                    For Each a In p.Armies

                        ' --- Fill MoveQueue if AI-controlled ---
                        If p.AIControlled AndAlso a.MoveQueue.Count = 0 Then
                            ' Generate up to 5 moves automatically
                            a.MoveQueue = GenerateAIMoves(a, p)
                        End If

                        ' --- Execute move for this step if exists ---
                        If stepIndex < a.MoveQueue.Count Then
                            Dim moveCommand As String = a.MoveQueue(stepIndex).ToUpper().Trim()
                            Dim dx As Integer = 0
                            Dim dy As Integer = 0
                            Dim isSpecial As Boolean = False

                            ' Determine if this is a special action (5th move)
                            If stepIndex = maxSteps - 1 Then
                                ' Treat any non-direction string as special action
                                Dim validDirs As String() = {"N", "NE", "E", "SE", "S", "SW", "W", "NW"}
                                If Not validDirs.Contains(moveCommand) Then
                                    isSpecial = True
                                End If
                            End If

                            If isSpecial Then
                                a.HasUsedSpecial = True
                                ' Handle special action here (Recruit, Train, Patrol, etc.)
                                ' Example: just log for now
                                Console.WriteLine($"{p.Race}'s army at ({a.X},{a.Y}) performs special action: {moveCommand}")
                            Else
                                ' Convert direction to dx/dy
                                Dim offset As Point = DirectionToOffset(moveCommand)
                                dx = offset.X
                                dy = offset.Y

                                ' Compute new coordinates with bounds check
                                Dim newX As Integer = Math.Max(0, Math.Min(mapSize - 1, a.X + dx))
                                Dim newY As Integer = Math.Max(0, Math.Min(mapSize - 1, a.Y + dy))

                                ' Update army position
                                a.X = newX
                                a.Y = newY

                                ' Capture tile if Manhattan-adjacent
                                Dim captured As Boolean = False
                                Dim directions As Point() = {New Point(0, -1), New Point(1, 0), New Point(0, 1), New Point(-1, 0)}
                                For Each d In directions
                                    Dim adjX As Integer = newX + d.X
                                    Dim adjY As Integer = newY + d.Y
                                    If adjX >= 0 AndAlso adjX < mapSize AndAlso adjY >= 0 AndAlso adjY < mapSize Then
                                        If Map(adjX, adjY, 1) = p.PlayerNumber Then
                                            Map(newX, newY, 1) = p.PlayerNumber
                                            captured = True
                                            Exit For
                                        End If
                                    End If
                                Next

                                ' Optional: log move
                                Console.WriteLine($"{p.Race}'s army moves {moveCommand} to ({newX},{newY})" &
                                              If(captured, " and captures tile", ""))
                            End If
                        End If

                    Next
                End If
            Next

            ' --- Optional: After each step, handle combat resolution if multiple armies occupy same tile ---
            ResolveCombat()
        Next

        ' After processing all 5 steps, clear all MoveQueues for next turn
        For Each p In Players
            For Each a In p.Armies
                a.MoveQueue.Clear()
                a.HasUsedSpecial = False
            Next
        Next
    End Sub

    Private Function GenerateAIMoves(army As Army, player As Player) As List(Of String)
        Dim moves As New List(Of String)
        Dim rnd As New Random()
        Dim directions As String() = {"N", "NE", "E", "SE", "S", "SW", "W", "NW"}
        Dim mapSize As Integer = Map.GetLength(0)

        ' Determine preferred terrain
        Dim preferredTerrain As Integer
        Select Case player.Race.ToLower()
            Case "elf" : preferredTerrain = 1
            Case "dwarf" : preferredTerrain = 3
            Case "orc" : preferredTerrain = 2
            Case "human" : preferredTerrain = 0
            Case Else : preferredTerrain = -1
        End Select

        ' Hypothetical position for planning moves
        Dim currentX As Integer = army.X
        Dim currentY As Integer = army.Y

        ' Generate 5 moves
        For stepIndex As Integer = 1 To 5
            Dim candidateDirs As New List(Of String)
            Dim dirScores As New Dictionary(Of String, Integer)

            ' Evaluate all 8 directions for immediate capture
            For Each direction In directions
                Dim offset As Point = DirectionToOffset(direction)
                Dim newX As Integer = Math.Max(0, Math.Min(mapSize - 1, currentX + offset.X))
                Dim newY As Integer = Math.Max(0, Math.Min(mapSize - 1, currentY + offset.Y))

                ' Check for enemy in 5x5 radius
                Dim enemyNearby As Boolean = False
                For dx As Integer = -2 To 2
                    For dy As Integer = -2 To 2
                        Dim checkX As Integer = newX + dx
                        Dim checkY As Integer = newY + dy
                        If checkX >= 0 AndAlso checkX < mapSize AndAlso checkY >= 0 AndAlso checkY < mapSize Then
                            For Each otherPlayer In Players
                                If otherPlayer.PlayerNumber <> player.PlayerNumber Then
                                    If otherPlayer.Armies IsNot Nothing Then
                                        For Each enemyArmy In otherPlayer.Armies
                                            If enemyArmy.X = checkX AndAlso enemyArmy.Y = checkY Then
                                                enemyNearby = True
                                                Exit For
                                            End If
                                        Next
                                    End If
                                End If
                                If enemyNearby Then Exit For
                            Next
                        End If
                        If enemyNearby Then Exit For
                    Next
                    If enemyNearby Then Exit For
                Next

                ' Only consider directions without nearby enemies for immediate capture
                If enemyNearby Then Continue For

                ' Check if tile is capturable
                Dim captureValid As Boolean = IsCaptureValid(newX, newY, player.PlayerNumber, Map)
                If captureValid Then
                    ' Base score 1 for any valid capture (even non-preferred terrain)
                    Dim score As Integer = 1
                    ' Bonus for preferred terrain
                    If Map(newX, newY, 0) = preferredTerrain Then score += 1

                    candidateDirs.Add(direction)
                    dirScores(direction) = score
                End If
            Next

            Dim chosenMove As String = ""

            If candidateDirs.Count > 0 Then
                ' Pick best scoring immediate capture
                Dim bestScore As Integer = dirScores.Values.Max()
                Dim bestDirs = dirScores.Where(Function(kvp) kvp.Value = bestScore).Select(Function(kvp) kvp.Key).ToList()
                chosenMove = bestDirs(rnd.Next(bestDirs.Count))
            Else
                ' No immediate capture: frontier-wandering through any tile (including enemy)
                Dim nearestTile As Point? = Nothing
                Dim minDistance As Integer = Integer.MaxValue

                ' Find nearest capturable tile
                For x As Integer = 0 To mapSize - 1
                    For y As Integer = 0 To mapSize - 1
                        If IsCaptureValid(x, y, player.PlayerNumber, Map) Then
                            Dim dist As Integer = Math.Abs(currentX - x) + Math.Abs(currentY - y)
                            If dist < minDistance Then
                                minDistance = dist
                                nearestTile = New Point(x, y)
                            End If
                        End If
                    Next
                Next

                If nearestTile.HasValue Then
                    ' Evaluate all directions including enemy tiles
                    Dim frontierDirs As New List(Of String)
                    Dim frontierScores As New Dictionary(Of String, Integer)

                    For Each direction In directions
                        Dim offset As Point = DirectionToOffset(direction)
                        Dim newX As Integer = currentX + offset.X
                        Dim newY As Integer = currentY + offset.Y
                        If newX < 0 OrElse newX >= mapSize OrElse newY < 0 OrElse newY >= mapSize Then Continue For

                        Dim newDist As Integer = Math.Abs(nearestTile.Value.X - newX) + Math.Abs(nearestTile.Value.Y - newY)
                        If newDist < minDistance Then
                            frontierDirs.Add(direction)
                            ' Slight penalty if tile is owned by enemy to prefer safer paths when possible
                            Dim score As Integer = 1
                            If Map(newX, newY, 1) <> -1 AndAlso Map(newX, newY, 1) <> player.PlayerNumber Then score -= 1
                            frontierScores(direction) = score
                        End If
                    Next

                    If frontierDirs.Count > 0 Then
                        ' Pick best scoring direction toward frontier
                        Dim bestScore As Integer = frontierScores.Values.Max()
                        Dim bestDirs = frontierScores.Where(Function(kvp) kvp.Value = bestScore).Select(Function(kvp) kvp.Key).ToList()
                        chosenMove = bestDirs(rnd.Next(bestDirs.Count))
                    Else
                        chosenMove = "Recruit"
                    End If
                Else
                    chosenMove = "Recruit"
                End If
            End If

            moves.Add(chosenMove)

            ' Update hypothetical position
            Dim moveOffset As Point = DirectionToOffset(chosenMove)
            currentX = Math.Max(0, Math.Min(mapSize - 1, currentX + moveOffset.X))
            currentY = Math.Max(0, Math.Min(mapSize - 1, currentY + moveOffset.Y))
        Next

        Return moves
    End Function


    ''' <summary>
    ''' Checks if a tile at (x, y) can be captured by the given player on the given map.
    ''' Capture only valid if tile is N/E/S/W adjacent to a tile owned by the player.
    ''' </summary>
    ''' <param name="x">Tile X coordinate</param>
    ''' <param name="y">Tile Y coordinate</param>
    ''' <param name="playerNumber">Player number attempting capture</param>
    ''' <param name="checkMap">Map array to check (hypothetical or real)</param>
    ''' <returns>True if capture is valid</returns>
    Private Function IsCaptureValid(x As Integer, y As Integer, playerNumber As Integer, checkMap(,,) As Integer) As Boolean
        ' Cannot capture if already owned by this player
        If checkMap(x, y, 1) = playerNumber Then Return False

        Dim mapSizeX As Integer = checkMap.GetLength(0)
        Dim mapSizeY As Integer = checkMap.GetLength(1)

        ' Check N/E/S/W neighbors
        Dim directions As Point() = {New Point(0, -1), New Point(1, 0), New Point(0, 1), New Point(-1, 0)}
        For Each d In directions
            Dim nx As Integer = x + d.X
            Dim ny As Integer = y + d.Y
            If nx >= 0 AndAlso nx < mapSizeX AndAlso ny >= 0 AndAlso ny < mapSizeY Then
                If checkMap(nx, ny, 1) = playerNumber Then
                    Return True
                End If
            End If
        Next

        ' No adjacent owned tile, cannot capture
        Return False
    End Function



    ' --- Helper: convert direction string to dx/dy ---
    Private Function DirectionToOffset(direction As String) As Point
        Select Case direction.ToUpper()
            Case "N" : Return New Point(0, -1)
            Case "NE" : Return New Point(1, -1)
            Case "E" : Return New Point(1, 0)
            Case "SE" : Return New Point(1, 1)
            Case "S" : Return New Point(0, 1)
            Case "SW" : Return New Point(-1, 1)
            Case "W" : Return New Point(-1, 0)
            Case "NW" : Return New Point(-1, -1)
            Case Else : Return New Point(0, 0) ' invalid input, no move
        End Select
    End Function

    ' --- Placeholder for combat resolution ---
    Private Sub ResolveCombat()
        ' TODO: Implement combat logic here if multiple armies occupy the same tile
    End Sub


#End Region


#Region "=== Gameplay Logic ==="



    Public Sub CollectResources()
        ' Loop through each player
        For Each p In Players
            ' Reset food for this turn
            p.Food = 0
            p.FoodCollectedThisTurn = 0
            p.IronCollectedThisTurn = 0
            p.WoodCollectedThisTurn = 0

            Dim foodThisTurn As Integer = 0
            Dim ironThisTurn As Integer = 0
            Dim woodThisTurn As Integer = 0

            ' Total squares
            Dim rows As Integer = 25
            Dim cols As Integer = 25
            Dim totalSquares As Integer = rows * cols

            ' Population per square
            Dim popPerSquare As Integer = p.Population \ totalSquares

            ' Determine player's preferred terrain (integer encoding)
            Dim preferredTerrain As Integer
            Select Case p.Race.ToLower()
                Case "elf"
                    preferredTerrain = 1  ' Forest
                Case "dwarf"
                    preferredTerrain = 3  ' Mountain
                Case "orc"
                    preferredTerrain = 2  ' Hills
                Case "human"
                    preferredTerrain = 0  ' Plains
                Case Else
                    preferredTerrain = -1 ' No bonus
            End Select

            ' Loop through map squares
            For x As Integer = 0 To rows - 1
                For y As Integer = 0 To cols - 1
                    Dim terrain As Integer = Map(x, y, 0)

                    ' Base resources per square
                    Dim foodPerSquare As Integer = 1
                    Dim ironPerSquare As Integer = 1
                    Dim woodPerSquare As Integer = 1

                    ' Apply racial bonus
                    If terrain = preferredTerrain Then
                        foodPerSquare += 1
                    End If

                    ' Multiply by population per square
                    foodThisTurn += foodPerSquare * popPerSquare
                    ironThisTurn += ironPerSquare * popPerSquare
                    woodThisTurn += woodPerSquare * popPerSquare
                Next
            Next

            ' Update player resources
            p.Food = foodThisTurn
            p.FoodCollectedThisTurn = foodThisTurn
            p.IronCollectedThisTurn = ironThisTurn
            p.WoodCollectedThisTurn = woodThisTurn

            ' Add to cumulative totals
            p.Iron += ironThisTurn
            p.Wood += woodThisTurn
        Next
    End Sub

    Public Sub GrowPopulationAndFeedEverybody()
        ' Loop through all players
        For Each p In Players
            ' Calculate total food required for armies
            Dim armyFoodRequirement As Integer = 0
            If p.Armies IsNot Nothing Then
                For Each a In p.Armies
                    armyFoodRequirement += a.Soldiers ' 1-to-1 food per soldier
                Next
            End If

            ' Total population to feed = civilian population + army food requirement
            Dim totalToFeed As Integer = p.Population + armyFoodRequirement

            ' Remaining food after feeding population + armies
            Dim remainingFood As Integer = p.FoodCollectedThisTurn - totalToFeed

            ' Population growth is remaining food ÷ 20
            Dim growth As Integer = 0
            If remainingFood > 0 Then
                growth = remainingFood \ 20
            End If

            ' Increase civilian population
            p.Population += growth
        Next
    End Sub


#End Region

#Region "=== Map Generation ==="

    Public Sub GenerateMap()
        Dim rnd As New Random()
        Dim width As Integer = Map.GetLength(0)
        Dim height As Integer = Map.GetLength(1)
        Dim blockSize As Integer = 5
        Dim terrainTypes As Integer() = {0, 1, 2, 3} ' Plains, Forest, Hills, Mountain

        ' --- Define player corners ---
        Dim playerCorners As New Dictionary(Of Integer, Point) From {
        {0, New Point(0, 0)},    ' Elf top-left
        {1, New Point(4, 0)},    ' Dwarf top-right
        {2, New Point(0, 4)},    ' Orc bottom-left
        {3, New Point(4, 4)}     ' Human bottom-right
    }

        Dim playerFavTerrain As New Dictionary(Of Integer, Integer) From {
        {0, 1}, ' Elf → Forest
        {1, 3}, ' Dwarf → Mountain
        {2, 2}, ' Orc → Hills
        {3, 0}  ' Human → Plains
    }

        ' --- Fill entire map with neutral tiles ---
        For x As Integer = 0 To width - 1
            For y As Integer = 0 To height - 1
                Map(x, y, 0) = terrainTypes(rnd.Next(0, 4))
                Map(x, y, 1) = -1 ' unowned
            Next
        Next

        ' --- Loop over all 5x5 blocks ---
        For bx As Integer = 0 To 4
            For by As Integer = 0 To 4
                Dim startX As Integer = bx * blockSize
                Dim startY As Integer = by * blockSize

                ' Find nearest corner using Chebyshev distance
                Dim closestPlayer As Integer = -1
                Dim minDistance As Integer = 100
                For Each kvp In playerCorners
                    Dim dist As Integer = Math.Max(Math.Abs(kvp.Value.X - bx), Math.Abs(kvp.Value.Y - by))
                    If dist < minDistance Then
                        minDistance = dist
                        closestPlayer = kvp.Key
                    End If
                Next

                ' Determine terrain distribution
                Dim terrainList As New List(Of Integer)
                Dim favTerrain As Integer = playerFavTerrain(closestPlayer)

                Select Case minDistance
                    Case 0 ' Starting corner heavily favored
                        For i As Integer = 1 To 19 : terrainList.Add(favTerrain) : Next
                        For t As Integer = 0 To 3
                            If t <> favTerrain Then For i As Integer = 1 To 2 : terrainList.Add(t) : Next
                        Next
                    Case 1 ' Slightly favored adjacent or diagonal blocks
                        For i As Integer = 1 To 3 : terrainList.Add(favTerrain) : Next
                        For t As Integer = 0 To 3
                            If t <> favTerrain Then terrainList.Add(t)
                        Next
                        While terrainList.Count < blockSize * blockSize
                            terrainList.AddRange(terrainList)
                        End While
                        terrainList = terrainList.Take(blockSize * blockSize).ToList()
                    Case Else ' Neutral
                        For i As Integer = 1 To 6 : For t As Integer = 0 To 3 : terrainList.Add(t) : Next : Next
                        terrainList.Add(terrainTypes(rnd.Next(0, 4))) ' 25th tile
                End Select

                ' Shuffle terrain
                For i As Integer = terrainList.Count - 1 To 1 Step -1
                    Dim j As Integer = rnd.Next(0, i + 1)
                    Dim temp As Integer = terrainList(i)
                    terrainList(i) = terrainList(j)
                    terrainList(j) = temp
                Next

                ' Fill the block
                Dim index As Integer = 0
                For x As Integer = startX To startX + blockSize - 1
                    For y As Integer = startY To startY + blockSize - 1
                        Map(x, y, 0) = terrainList(index)
                        ' Only starting corners owned
                        If minDistance = 0 Then
                            Map(x, y, 1) = closestPlayer
                        Else
                            Map(x, y, 1) = -1
                        End If
                        index += 1
                    Next
                Next
            Next
        Next
    End Sub


    ' Private helper to fill a 5x5 block with exact terrain distribution for a player
    Private Sub FillStartingBlock(startX As Integer, startY As Integer, playerID As Integer, favTerrain As Integer)
        Dim rnd As New Random() ' Local random generator for shuffling
        Dim terrainList As New List(Of Integer)

        ' 19 favored terrain
        For i As Integer = 1 To 19
            terrainList.Add(favTerrain)
        Next
        ' 2 of each remaining terrain
        For t As Integer = 0 To 3
            If t <> favTerrain Then
                For i As Integer = 1 To 2
                    terrainList.Add(t)
                Next
            End If
        Next

        ' Shuffle the list
        For i As Integer = terrainList.Count - 1 To 1 Step -1
            Dim j As Integer = rnd.Next(0, i + 1)
            Dim temp As Integer = terrainList(i)
            terrainList(i) = terrainList(j)
            terrainList(j) = temp
        Next

        ' Fill the block
        Dim index As Integer = 0
        For x As Integer = startX To startX + 4
            For y As Integer = startY To startY + 4
                Map(x, y, 0) = terrainList(index)
                Map(x, y, 1) = playerID
                index += 1
            Next
        Next
    End Sub

    ' Returns the most common terrain type among neighbours (including itself)
    Private Function GetDominantNeighbour(x As Integer, y As Integer) As Integer
        Dim counts(3) As Integer ' 0 = Plains, 1 = Forest, 2 = Hills, 3 = Mountain
        Dim width As Integer = Map.GetLength(0)
        Dim height As Integer = Map.GetLength(1)

        ' Check all neighbours in a 3x3 area
        For dx As Integer = -1 To 1
            For dy As Integer = -1 To 1
                Dim nx As Integer = x + dx
                Dim ny As Integer = y + dy
                If nx >= 0 AndAlso nx < width AndAlso ny >= 0 AndAlso ny < height Then
                    counts(Map(nx, ny, 0)) += 1
                End If
            Next
        Next

        ' Find terrain type with max count
        Dim maxCount As Integer = -1
        Dim dominantType As Integer = 0
        For t As Integer = 0 To 3
            If counts(t) > maxCount Then
                maxCount = counts(t)
                dominantType = t
            End If
        Next

        Return dominantType
    End Function


#End Region


#Region "=== UI Events ==="

    Private Sub pnlMap_Paint(sender As Object, e As PaintEventArgs) Handles pnlMap.Paint
        DrawMap(e.Graphics, pnlMap.ClientSize.Width, pnlMap.ClientSize.Height)
    End Sub

    Private Sub btn_Show_Click(sender As Object, e As EventArgs) Handles btn_Show.Click
        pnlMap.Invalidate()
    End Sub


    Private Function GetEmbeddedImage(terrainName As String) As Image
        ' terrainName: e.g., "forest.png", "plains.png"
        Dim asm As Reflection.Assembly = Reflection.Assembly.GetExecutingAssembly()
        Dim resourceName As String = "Conflict." & terrainName ' must match your embedded resource name

        Using stream As IO.Stream = asm.GetManifestResourceStream(resourceName)
            If stream IsNot Nothing Then
                Return Image.FromStream(stream)
            Else
                Return Nothing
            End If
        End Using
    End Function

    Private Sub btnProcessTurn_Click(sender As Object, e As EventArgs) Handles btnProcessTurn.Click
        ' --- 1. Collect resources for all players ---
        CollectResources()

        ' --- 2. Grow population and feed armies/civilians ---
        GrowPopulationAndFeedEverybody()

        ' --- 3. Execute army movements step by step ---
        ' This uses the ProcessTurn() function we wrote earlier
        ProcessTurn()

        ' --- 4. Refresh map display so GM can see new positions ---
        pnlMap.Invalidate()

        ' --- Optional: update any UI labels or stats ---
        ' For example, display player resources or army counts
        ' DisplayPlayerResources()
    End Sub



#End Region

    ' --- PrintDocument handler ---
    Private Sub printDoc_PrintPage(sender As Object, e As PrintPageEventArgs) Handles printDoc.PrintPage
        Dim g As Graphics = e.Graphics
        g.Clear(Color.White)

        ' --- Draw header and player info ---
        DrawFrontPageHeaderAndPlayerInfo(g, e)

        ' --- Draw map below header/player box ---
        Dim headerHeight As Single = 60
        Dim playerBoxHeight As Single = 80
        Dim spacing As Single = 20

        Dim topOffset As Single = e.MarginBounds.Top + headerHeight + playerBoxHeight + spacing


        DrawMap(g, e.MarginBounds.Width, e.MarginBounds.Height - topOffset, False, e, topOffset)

        ' Only one page
        e.HasMorePages = False
    End Sub

    ' --- Helper to draw heading and player info ---
    Private Sub DrawFrontPageHeaderAndPlayerInfo(g As Graphics, e As PrintPageEventArgs)
        ' --- Heading ---
        Using font As New Font("Arial", 36, FontStyle.Bold)
            Dim headingRect As New RectangleF(e.MarginBounds.Left, e.MarginBounds.Top, e.MarginBounds.Width, 50)
            Dim sf As New StringFormat() With {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center}
            g.DrawString("CONFLICT", font, Brushes.Black, headingRect, sf)
        End Using

        ' --- Player info box ---
        Dim playerBoxHeight As Single = 80
        Dim playerBox As New RectangleF(e.MarginBounds.Left + 20, e.MarginBounds.Top + 60, 200, playerBoxHeight)
        g.DrawRectangle(Pens.Black, Rectangle.Round(playerBox))

        Using font As New Font("Arial", 12, FontStyle.Regular)
            g.DrawString("Player Name:", font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 5)
            g.DrawString("Address:", font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 25)
            g.DrawString("Race: " & Players(0).Race, font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 45)
            g.DrawString("Population: " & Players(0).Population.ToString(), font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 65)
        End Using
    End Sub


#Region "=== Printing: Front Page ==="

    ' --- Print button ---
    Private Sub btnPrint_Click(sender As Object, e As EventArgs) Handles btnPrint.Click
        printDoc.Print()
    End Sub


    ' --- Draw front page ---
    Private Sub DrawFrontPage(g As Graphics, e As PrintPageEventArgs)
        g.Clear(Color.White)

        ' --- Heading ---
        Using font As New Font("Arial", 36, FontStyle.Bold)
            Dim headingRect As New RectangleF(e.MarginBounds.Left, e.MarginBounds.Top, e.MarginBounds.Width, 50)
            Dim sf As New StringFormat() With {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center}
            g.DrawString("CONFLICT", font, Brushes.Black, headingRect, sf)
        End Using

        ' --- Player info box ---
        Dim playerBoxHeight As Single = 80
        Dim playerBox As New RectangleF(e.MarginBounds.Left + 20, e.MarginBounds.Top + 60, 200, playerBoxHeight)
        g.DrawRectangle(Pens.Black, Rectangle.Round(playerBox))
        Using font As New Font("Arial", 12, FontStyle.Regular)
            g.DrawString("Player Name:", font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 5)
            g.DrawString("Address:", font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 25)
            g.DrawString("Race: " & Players(0).Race, font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 45)
            g.DrawString("Population: " & Players(0).Population.ToString(), font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 65)
        End Using

        ' --- Map area below heading/player box ---
        Dim topMargin As Single = e.MarginBounds.Top + 60 + playerBoxHeight + 20
        Dim mapHeight As Single = e.MarginBounds.Bottom - topMargin
        Dim mapWidth As Single = e.MarginBounds.Width

        DrawMap(g, mapWidth, mapHeight, False, e, topOffset:=topMargin)
    End Sub

#End Region

    Private Sub DrawMap(g As Graphics,
                     Optional width As Single = -1,
                     Optional height As Single = -1,
                     Optional isPanel As Boolean = True,
                     Optional e As PrintPageEventArgs = Nothing,
                     Optional topOffset As Single = 0)

        Dim mapSize As Integer = 25
        Dim numberMargin As Single = 20

        If width <= 0 Then width = pnlMap.ClientSize.Width
        If height <= 0 Then height = pnlMap.ClientSize.Height

        ' Compute tile size to fit map
        Dim tileSize As Single
        If isPanel Then
            tileSize = Math.Min((width - 2 * numberMargin) / mapSize, (height - 2 * numberMargin) / mapSize)
        Else
            tileSize = Math.Min((width - 2 * numberMargin) / mapSize, (height - 12) / mapSize)
        End If

        Dim totalMapWidth As Single = tileSize * mapSize
        Dim totalMapHeight As Single = tileSize * mapSize

        ' Calculate offsets
        Dim xOffset As Single
        Dim yOffset As Single
        If isPanel Then
            xOffset = numberMargin + (width - 2 * numberMargin - totalMapWidth) / 2
            yOffset = numberMargin + (height - 2 * numberMargin - totalMapHeight) / 2 + topOffset

            ' Only clear panel drawing
            g.Clear(Color.White)
        Else
            ' Printing: center map horizontally, push down by topOffset (includes header + player info)
            xOffset = e.PageBounds.Left + (e.PageBounds.Width - totalMapWidth) / 2
            yOffset = e.PageBounds.Top + topOffset
            ' Do NOT clear printing graphics; header and player info already drawn
        End If

        ' --- Draw terrain and ownership ---
        For x As Integer = 0 To mapSize - 1
            For y As Integer = 0 To mapSize - 1
                Dim terrainValue As Integer = Map(x, y, 0)
                Dim terrainImage As Image = Nothing
                Select Case terrainValue
                    Case 0 : terrainImage = terrainCache("plains.png")
                    Case 1 : terrainImage = terrainCache("forest.png")
                    Case 2 : terrainImage = terrainCache("hills.png")
                    Case 3 : terrainImage = terrainCache("mountain.png")
                End Select

                Dim ownerIndex As Integer = Map(x, y, 1)
                Dim ownerColor As Color = Color.White
                If ownerIndex >= 0 AndAlso ownerIndex < playerColors.Length Then ownerColor = playerColors(ownerIndex)

                Dim xPos As Single = xOffset + x * tileSize
                Dim yPos As Single = yOffset + y * tileSize
                Dim w As Single = tileSize
                Dim h As Single = tileSize

                If isPanel AndAlso x = mapSize - 1 Then w = Math.Min(w, pnlMap.ClientSize.Width - (xOffset + x * tileSize))
                If isPanel AndAlso y = mapSize - 1 Then h = Math.Min(h, pnlMap.ClientSize.Height - (yOffset + y * tileSize))

                Using brush As New SolidBrush(ownerColor)
                    g.FillRectangle(brush, xPos, yPos, w, h)
                End Using

                If terrainImage IsNot Nothing Then
                    g.DrawImage(terrainImage, xPos, yPos, w, h)
                End If
            Next
        Next

        ' --- Draw grid numbers ---
        Using font As New Font("Arial", 8)
            Using brush As New SolidBrush(Color.Black)
                Dim leftOffset As Single = xOffset - 18
                Dim rightOffset As Single = xOffset + totalMapWidth + 4

                For x As Integer = 0 To mapSize - 1
                    Dim xNumPos As Single = xOffset + x * tileSize + tileSize / 2
                    g.DrawString((x + 1).ToString(), font, brush, xNumPos, yOffset - 12, New StringFormat() With {.Alignment = StringAlignment.Center})
                    g.DrawString((x + 1).ToString(), font, brush, xNumPos, yOffset + totalMapHeight + 2, New StringFormat() With {.Alignment = StringAlignment.Center})
                Next

                For y As Integer = 0 To mapSize - 1
                    Dim yNumPos As Single = yOffset + y * tileSize + tileSize / 2
                    g.DrawString((y + 1).ToString(), font, brush, leftOffset, yNumPos, New StringFormat() With {.LineAlignment = StringAlignment.Center})
                    g.DrawString((y + 1).ToString(), font, brush, rightOffset, yNumPos, New StringFormat() With {.LineAlignment = StringAlignment.Center})
                Next
            End Using
        End Using

        ' --- Draw grid lines ---
        Using pen As New Pen(Color.Gray)
            For i As Integer = 0 To mapSize
                Dim yLine As Single = yOffset + i * tileSize
                Dim xLine As Single = xOffset + i * tileSize

                If isPanel AndAlso i = mapSize Then
                    yLine = Math.Min(yLine, pnlMap.ClientSize.Height - 1)
                    xLine = Math.Min(xLine, pnlMap.ClientSize.Width - 1)
                End If

                g.DrawLine(pen, xOffset, yLine, xOffset + totalMapWidth, yLine)
                g.DrawLine(pen, xLine, yOffset, xLine, yOffset + totalMapHeight)
            Next
        End Using

        ' --- Draw armies ---
        ' (Remains unchanged)
        Dim tileTotals As New Dictionary(Of Point, Integer)
        Dim tileOwner As New Dictionary(Of Point, Integer)

        For Each p In Players
            If p.Armies IsNot Nothing Then
                For Each a In p.Armies
                    Dim pt As New Point(a.X, a.Y)
                    If tileTotals.ContainsKey(pt) Then
                        tileTotals(pt) += a.Soldiers
                    Else
                        tileTotals(pt) = a.Soldiers
                        tileOwner(pt) = p.PlayerNumber
                    End If
                Next
            End If
        Next

        For Each kvp In tileTotals
            Dim x = kvp.Key.X
            Dim y = kvp.Key.Y
            Dim totalSoldiers = kvp.Value

            Dim xPos As Single = xOffset + x * tileSize
            Dim yPos As Single = yOffset + y * tileSize

            ' Dim the terrain
            Using dimBrush As New SolidBrush(Color.FromArgb(80, 0, 0, 0))
                g.FillRectangle(dimBrush, xPos, yPos, tileSize, tileSize)
            End Using

            ' Highlight rectangle using army owner
            Dim playerIndex As Integer = tileOwner(kvp.Key)
            Dim borderColor As Color
            Select Case playerIndex
                Case 0 : borderColor = Color.FromArgb(173, 255, 47) ' Elf
                Case 1 : borderColor = Color.Cyan ' Dwarf
                Case 2 : borderColor = Color.Red ' Orc
                Case 3 : borderColor = Color.Orange ' Human
                Case Else : borderColor = Color.Gray
            End Select

            Using pen As New Pen(borderColor, Math.Max(1, tileSize * 0.1F))
                g.DrawRectangle(pen, xPos + 0.5F, yPos + 0.5F, tileSize - 1, tileSize - 1)
            End Using

            ' Army number
            Dim armyText As String
            Dim baseFontSize As Single = Math.Min(tileSize * 0.5F, 12)
            Dim fontSize As Single = baseFontSize

            If totalSoldiers >= 1000000 Then
                If totalSoldiers < 10000000 Then
                    armyText = (totalSoldiers / 1000000.0).ToString("0.#") & "M"
                Else
                    armyText = (totalSoldiers \ 1000000).ToString() & "M"
                End If
            Else
                armyText = Math.Max(1, CInt(Math.Round(totalSoldiers / 1000.0))).ToString()
                If armyText.Length > 3 Then fontSize *= 0.75F
            End If

            Using font As New Font("Arial", fontSize, FontStyle.Bold)
                Using brush As New SolidBrush(Color.White)
                    Dim sf As New StringFormat() With {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center}
                    g.DrawString(armyText, font, brush, xPos + tileSize / 2, yPos + tileSize / 2, sf)
                End Using
            End Using
        Next
    End Sub



End Class
