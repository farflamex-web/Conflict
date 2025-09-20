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
        Public Property X As Integer
        Public Property Y As Integer
        Public Property Soldiers As Integer
        Public Property Weapons As Integer
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

        ' Add 4 Player objects
        For i As Integer = 0 To 3
            Dim p As New Player()
            p.PlayerNumber = i
            p.Race = Races(i)
            p.Population = 5000
            Players.Add(p)
        Next

    End Sub

#End Region

#Region "=== Gameplay Logic ==="

    Private Sub NextTurn()
        CollectResources()
        GrowPopulationAndFeedEverybody()
    End Sub

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

    ' Main terrain generation method
    Public Sub GenerateMap()
        Dim rnd As New Random() ' Local random generator
        Dim width As Integer = Map.GetLength(0)
        Dim height As Integer = Map.GetLength(1)

        ' Step 1: Random initial terrain
        For x As Integer = 0 To width - 1
            For y As Integer = 0 To height - 1
                Map(x, y, 0) = rnd.Next(0, 4) ' 0 = Plains, 1 = Forest, 2 = Hills, 3 = Mountain
                Map(x, y, 1) = -1 ' Initialize ownership as unowned
            Next
        Next

        ' Step 2: Smooth the terrain to create clumps
        Dim iterations As Integer = 3
        For i As Integer = 1 To iterations
            Dim newMap(width - 1, height - 1) As Integer
            For x As Integer = 0 To width - 1
                For y As Integer = 0 To height - 1
                    newMap(x, y) = GetDominantNeighbour(x, y)
                Next
            Next

            ' Copy back the smoothed map
            For x As Integer = 0 To width - 1
                For y As Integer = 0 To height - 1
                    Map(x, y, 0) = newMap(x, y)
                Next
            Next
        Next

        ' Step 3: Assign starting blocks for each player
        Dim blockSize As Integer = 5
        ' Player 0: Elf (Forest = 1) - top-left
        FillStartingBlock(0, 0, 0, 1)
        ' Player 1: Dwarf (Mountain = 3) - top-right
        FillStartingBlock(width - blockSize, 0, 1, 3)
        ' Player 2: Orc (Hills = 2) - bottom-left
        FillStartingBlock(0, height - blockSize, 2, 2)
        ' Player 3: Human (Plains = 0) - bottom-right
        FillStartingBlock(width - blockSize, height - blockSize, 3, 0)
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

#Region "=== Drawing and Printing ==="



#End Region


#Region "=== UI Events ==="

    Private Sub btnNextTurn_Click(sender As Object, e As EventArgs) Handles btnNextTurn.Click
        NextTurn()
        'DisplayPlayerResources()
    End Sub

    Private Sub pnlMap_Paint(sender As Object, e As PaintEventArgs) Handles pnlMap.Paint
        DrawMap(e.Graphics, pnlMap.ClientSize.Width, pnlMap.ClientSize.Height)
    End Sub

    Private Sub btn_Show_Click(sender As Object, e As EventArgs) Handles btn_Show.Click
        pnlMap.Invalidate()
    End Sub


    Private Sub btn_Print_Click(sender As Object, e As EventArgs) Handles btn_Print.Click
        printDoc.Print()
    End Sub

    Private Sub printDoc_PrintPage(sender As Object, e As Printing.PrintPageEventArgs) Handles printDoc.PrintPage
        DrawMap(e.Graphics, e.MarginBounds.Width, e.MarginBounds.Height, False, e)
    End Sub


    Private Sub DrawMap(g As Graphics, Optional width As Single = -1, Optional height As Single = -1, Optional isPanel As Boolean = True, Optional e As Printing.PrintPageEventArgs = Nothing)
        Dim mapSize As Integer = 25
        Dim numberMargin As Single = 20 ' space for row/column numbers

        ' Determine drawing area
        If width <= 0 Then width = pnlMap.ClientSize.Width
        If height <= 0 Then height = pnlMap.ClientSize.Height

        ' Determine tile size
        Dim tileSize As Single
        If isPanel Then
            tileSize = Math.Min((width - 2 * numberMargin) / mapSize, (height - 2 * numberMargin) / mapSize)
        Else
            tileSize = Math.Min((width - 2 * numberMargin) / mapSize, (height - 12) / mapSize) ' top margin for printing
        End If

        ' Total map size
        Dim totalMapWidth As Single = tileSize * mapSize
        Dim totalMapHeight As Single = tileSize * mapSize

        ' Offsets
        Dim xOffset As Single
        Dim yOffset As Single
        If isPanel Then
            ' Panel: centered within margins
            xOffset = numberMargin + (width - 2 * numberMargin - totalMapWidth) / 2
            yOffset = numberMargin + (height - 2 * numberMargin - totalMapHeight) / 2
        Else
            ' Printer: horizontally centered on page with small left adjustment, top margin
            Dim leftAdjustment As Single = -3
            Dim adjustedTopMargin As Single = 12
            xOffset = e.PageBounds.Left + (e.PageBounds.Width - totalMapWidth) / 2 + leftAdjustment
            yOffset = e.PageBounds.Top + adjustedTopMargin
        End If

        g.Clear(Color.White)

        ' Draw tiles and ownership colors
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
                If ownerIndex >= 0 And ownerIndex < playerColors.Length Then ownerColor = playerColors(ownerIndex)

                Dim xPos As Single = xOffset + x * tileSize
                Dim yPos As Single = yOffset + y * tileSize
                Dim w As Single = tileSize
                Dim h As Single = tileSize

                ' Panel only: adjust last pixel to prevent clipping
                If isPanel AndAlso x = mapSize - 1 Then w = Math.Min(w, pnlMap.ClientSize.Width - (xOffset + x * tileSize))
                If isPanel AndAlso y = mapSize - 1 Then h = Math.Min(h, pnlMap.ClientSize.Height - (yOffset + y * tileSize))

                ' Draw background
                Using brush As New SolidBrush(ownerColor)
                    g.FillRectangle(brush, xPos, yPos, w, h)
                End Using

                ' Draw terrain
                If terrainImage IsNot Nothing Then g.DrawImage(terrainImage, xPos, yPos, w, h)
            Next
        Next

        ' Draw row and column numbers
        Using font As New Font("Arial", 8)
            Using brush As New SolidBrush(Color.Black)
                Dim leftOffset As Single = xOffset - 18
                Dim rightOffset As Single = xOffset + totalMapWidth + 4

                ' Columns: top and bottom
                For x As Integer = 0 To mapSize - 1
                    Dim xNumPos As Single = xOffset + x * tileSize + tileSize / 2
                    g.DrawString((x + 1).ToString(), font, brush, xNumPos, yOffset - 12, New StringFormat() With {.Alignment = StringAlignment.Center})
                    g.DrawString((x + 1).ToString(), font, brush, xNumPos, yOffset + totalMapHeight + 2, New StringFormat() With {.Alignment = StringAlignment.Center})
                Next

                ' Rows: left and right
                For y As Integer = 0 To mapSize - 1
                    Dim yNumPos As Single = yOffset + y * tileSize + tileSize / 2
                    g.DrawString((y + 1).ToString(), font, brush, leftOffset, yNumPos, New StringFormat() With {.LineAlignment = StringAlignment.Center})
                    g.DrawString((y + 1).ToString(), font, brush, rightOffset, yNumPos, New StringFormat() With {.LineAlignment = StringAlignment.Center})
                Next
            End Using
        End Using

        ' Draw grid lines
        Using pen As New Pen(Color.Gray)
            For i As Integer = 0 To mapSize
                Dim yLine As Single = yOffset + i * tileSize
                Dim xLine As Single = xOffset + i * tileSize

                ' Panel: adjust last line to avoid clipping
                If isPanel AndAlso i = mapSize Then
                    yLine = Math.Min(yLine, pnlMap.ClientSize.Height - 1)
                    xLine = Math.Min(xLine, pnlMap.ClientSize.Width - 1)
                End If

                g.DrawLine(pen, xOffset, yLine, xOffset + totalMapWidth, yLine)
                g.DrawLine(pen, xLine, yOffset, xLine, yOffset + totalMapHeight)
            Next
        End Using
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



#End Region

End Class
