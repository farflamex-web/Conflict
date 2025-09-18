Option Strict On
Option Explicit On

Imports System.Drawing.Printing
Imports System.Reflection

Public Class Form1

    ' --- Constants ---
    Private Const Rows As Integer = 25
    Private Const Cols As Integer = 25
    Private Const InitialSquaresPerPlayer As Integer = 50
    Private Const LegendSpriteSize As Single = 32
    Private Const LegendLabelSpacing As Single = 10
    Private Const GridFontSize As Single = 6

    ' --- Terrain types ---
    Private Enum TerrainType
        Plains
        Forest
        Hills
        Mountains
    End Enum

    ' --- Maps ---
    Private terrainMap(Rows - 1, Cols - 1) As TerrainType
    Private playerMap(Rows - 1, Cols - 1) As Integer ' -1 = unowned

    ' --- Player squares ---
    Private playerSquares As List(Of Point)() = {
        New List(Of Point)(), ' Player 0
        New List(Of Point)(), ' Player 1
        New List(Of Point)(), ' Player 2
        New List(Of Point)()  ' Player 3
    }

    ' --- Sprites ---
    Private terrainSprites As Dictionary(Of TerrainType, Image)

    ' --- Player colors (pastel/light for top 3, original yellow for human) ---
    Private playerColors As Color() = {
    Color.LightGreen,   ' Player 0: Elf
    Color.LightSkyBlue, ' Player 1: Dwarf
    Color.LightCoral,   ' Player 2: Orc
    Color.Yellow        ' Player 3: Human
}



    ' --- Print document ---
    Private WithEvents printDoc As New PrintDocument

    ' --- Form load ---
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        terrainSprites = LoadAllTerrainSprites()

        ' Generate terrain and initialize player areas
        GenerateTerrainMap()
        InitializePlayerMap()
    End Sub

    ' --- Load all terrain sprites ---
    Private Function LoadAllTerrainSprites() As Dictionary(Of TerrainType, Image)
        Return New Dictionary(Of TerrainType, Image) From {
            {TerrainType.Plains, LoadEmbeddedImageByName("plains.png")},
            {TerrainType.Forest, LoadEmbeddedImageByName("forest.png")},
            {TerrainType.Hills, LoadEmbeddedImageByName("hills.png")},
            {TerrainType.Mountains, LoadEmbeddedImageByName("mountain.png")}
        }
    End Function

    ' --- Load embedded image helper ---
    Private Function LoadEmbeddedImageByName(fileName As String) As Image
        Dim asm As Reflection.Assembly = Reflection.Assembly.GetExecutingAssembly()
        Dim fullName As String = asm.GetManifestResourceNames() _
                                .FirstOrDefault(Function(n) n.EndsWith(fileName, StringComparison.OrdinalIgnoreCase))

        If fullName IsNot Nothing Then
            Using stream = asm.GetManifestResourceStream(fullName)
                Return Image.FromStream(stream)
            End Using
        Else
            Throw New Exception("Embedded resource not found: " & fileName)
        End If
    End Function

    ' --- Generate random terrain ---
    Private Sub GenerateTerrainMap()
        Dim rnd As New Random()
        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                terrainMap(r, c) = CType(rnd.Next(0, 4), TerrainType)
            Next
        Next
    End Sub

    ' --- Initialize organic player areas (cardinal expansion only) ---
    Private Sub InitializePlayerMap()
        ' Reset all squares
        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                playerMap(r, c) = -1
            Next
        Next

        Dim rnd As New Random()

        ' Helper: claim 'count' squares organically from a start point
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

                                       ' Add neighbors only in cardinal directions
                                       Dim directions As Point() = {
                                           New Point(0, -1),  ' North
                                           New Point(1, 0),   ' East
                                           New Point(0, 1),   ' South
                                           New Point(-1, 0)   ' West
                                       }

                                       For Each d As Point In directions
                                           Dim nr As Integer = cell.Y + d.Y
                                           Dim nc As Integer = cell.X + d.X
                                           If nr >= 0 AndAlso nr < Rows AndAlso nc >= 0 AndAlso nc < Cols Then
                                               Dim neighbor As Point = New Point(nc, nr)
                                               If playerMap(nr, nc) = -1 AndAlso Not frontier.Contains(neighbor) Then
                                                   frontier.Add(neighbor)
                                               End If
                                           End If
                                       Next
                                   End If
                               End While
                           End Sub

        ' Claim areas for 4 players
        ClaimOrganic(0, 0, 0, InitialSquaresPerPlayer)
        ClaimOrganic(0, Cols - 1, 1, InitialSquaresPerPlayer)
        ClaimOrganic(Rows - 1, 0, 2, InitialSquaresPerPlayer)
        ClaimOrganic(Rows - 1, Cols - 1, 3, InitialSquaresPerPlayer)

        UpdatePlayerSquares()
    End Sub

    ' --- Update playerSquares list from playerMap ---
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

    ' --- Print button click ---
    Private Sub btnPrint_Click(sender As Object, e As EventArgs) Handles btnPrint.Click
        Using dlg As New PrintDialog()
            dlg.Document = printDoc
            If dlg.ShowDialog() = DialogResult.OK Then
                printDoc.Print()
            End If
        End Using
    End Sub

    ' --- PrintPage event ---
    Private Sub printDoc_PrintPage(sender As Object, e As PrintPageEventArgs) Handles printDoc.PrintPage
        Dim g As Graphics = e.Graphics
        Dim bounds As RectangleF = e.MarginBounds
        Dim cellSize As Single = bounds.Width / Cols
        Dim gridWidth As Single = cellSize * Cols
        Dim gridHeight As Single = cellSize * Rows
        Dim startX As Single = bounds.Left
        Dim startY As Single = bounds.Top + 10

        DrawGridNumbers(g, startX, startY, cellSize)
        DrawPlayerTerrains(g, startX, startY, cellSize)
        DrawLegend(g, startX, startY, gridWidth, gridHeight)
    End Sub

    ' --- Draw row and column numbers ---
    Private Sub DrawGridNumbers(g As Graphics, startX As Single, startY As Single, cellSize As Single)
        Using font As New Font("Arial", GridFontSize)
            ' Columns
            For c As Integer = 0 To Cols - 1
                Dim numX As Single = startX + c * cellSize + cellSize / 2
                Dim numY As Single = startY - 12
                g.DrawString((c + 1).ToString(), font, Brushes.Black, numX, numY, New StringFormat() With {.Alignment = StringAlignment.Center})
            Next

            ' Rows
            For r As Integer = 0 To Rows - 1
                Dim numX As Single = startX - 12
                Dim numY As Single = startY + r * cellSize + cellSize / 2 - 4
                g.DrawString((r + 1).ToString(), font, Brushes.Black, numX, numY, New StringFormat() With {.Alignment = StringAlignment.Far})
            Next
        End Using
    End Sub

    Private Sub DrawPlayerTerrains(g As Graphics, startX As Single, startY As Single, cellSize As Single)
        ' Set pixel offset mode to half to avoid tube effect
        g.PixelOffsetMode = Drawing2D.PixelOffsetMode.Half

        ' Thin gray grid pen
        Dim gridPen As New Pen(Color.LightGray, 1.5F)

        For r As Integer = 0 To Rows - 1
            For c As Integer = 0 To Cols - 1
                Dim cellX As Single = startX + c * cellSize
                Dim cellY As Single = startY + r * cellSize
                Dim playerId As Integer = playerMap(r, c)

                ' --- Player-colored background ---
                If playerId >= 0 Then
                    Using brush As New SolidBrush(playerColors(playerId))
                        g.FillRectangle(brush, cellX, cellY, cellSize, cellSize)
                    End Using
                End If

                ' --- Thin gray grid ---
                g.DrawRectangle(gridPen, cellX, cellY, cellSize, cellSize)

                ' --- Terrain sprite with slight padding ---
                Dim terrain As TerrainType = terrainMap(r, c)
                Dim sprite As Image = terrainSprites(terrain)
                Dim padding As Single = 2
                Dim drawWidth As Single = cellSize - 2 * padding
                Dim drawHeight As Single = cellSize - 2 * padding
                g.DrawImage(sprite, cellX + padding, cellY + padding, drawWidth, drawHeight)

                ' --- Thick black border for outer edges of the empire ---
                If playerId >= 0 Then
                    Using borderPen As New Pen(Color.Black, 3)
                        Dim halfPen As Single = borderPen.Width / 2

                        Dim drawTop As Boolean = (r = 0 OrElse playerMap(r - 1, c) <> playerId)
                        Dim drawLeft As Boolean = (c = 0 OrElse playerMap(r, c - 1) <> playerId)
                        Dim drawRight As Boolean = (c = Cols - 1 OrElse playerMap(r, c + 1) <> playerId)
                        Dim drawBottom As Boolean = (r = Rows - 1 OrElse playerMap(r + 1, c) <> playerId)

                        ' Draw thick lines with half-pen offset; corners may be slightly dotted
                        If drawTop Then g.DrawLine(borderPen, cellX + halfPen, cellY + halfPen, cellX + cellSize - halfPen, cellY + halfPen)
                        If drawLeft Then g.DrawLine(borderPen, cellX + halfPen, cellY + halfPen, cellX + halfPen, cellY + cellSize - halfPen)
                        If drawRight Then g.DrawLine(borderPen, cellX + cellSize - halfPen, cellY + halfPen, cellX + cellSize - halfPen, cellY + cellSize - halfPen)
                        If drawBottom Then g.DrawLine(borderPen, cellX + halfPen, cellY + cellSize - halfPen, cellX + cellSize - halfPen, cellY + cellSize - halfPen)
                    End Using
                End If
            Next
        Next
    End Sub


    ' --- Draw legend below grid ---
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

End Class
