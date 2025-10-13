' ============================================================
'  Printouts.vb
'  Handles player report printing and HTML report generation.
'  Each player’s report will include header, resources, map,
'  armies, and recent events. Both printed and HTML versions
'  will be generated together for consistency.
' ============================================================

Imports System.Drawing
Imports System.IO
Imports System.Windows.Forms
Imports Conflict.Form1
Imports Conflict.UIModule   ' for CurrentForm, etc.
Imports System.Drawing.Printing


Module Printouts

    Private printDoc As PrintDocument

    Public terrainCache As New Dictionary(Of String, Image)

    Public Sub CreateTerrainCache()
        ' Clear existing cache (in case called multiple times)
        terrainCache.Clear()

        Dim terrainNames As String() = {"plains.png", "forest.png", "hills.png", "mountain.png", "elfcitadel.png", "dwarfcitadel.png", "orccitadel.png", "humancitadel.png"}
        For Each terrainName In terrainNames
            Dim img As Image = GetEmbeddedImage(terrainName)
            If img IsNot Nothing Then
                terrainCache.Add(terrainName, img)
            End If
        Next
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

    Public Sub SetupPrinting()
        printDoc = New PrintDocument()
        AddHandler printDoc.PrintPage, AddressOf printDoc_PrintPage
    End Sub

    ' ------------------------------------------------------------
    ' Entry point for generating reports for all non-AI players
    ' ------------------------------------------------------------
    Public Sub GenerateAllPlayerReports()
        If CurrentForm Is Nothing OrElse CurrentForm.Players Is Nothing Then Exit Sub

        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated OrElse p.AIControlled Then Continue For
            GenerateSinglePlayerReport(p)
        Next
    End Sub


    Private Sub printDoc_PrintPage(sender As Object, e As PrintPageEventArgs)
        Dim g As Graphics = e.Graphics
        g.Clear(Color.White)

        ' === Safety net for monster updates ===
        Try
            If CurrentForm.Players IsNot Nothing AndAlso CurrentForm.Players.Count > 0 Then
                UpdateSeenMonstersForAllPlayers(CurrentForm.Players)
            End If
        Catch ex As Exception
        End Try

        ' === Draw report content ===
        DrawFrontPageHeaderAndPlayerInfo(g, e)

        Dim headerHeight As Single = 60
        Dim playerBoxHeight As Single = 80
        Dim spacing As Single = 20
        Dim topOffset As Single = e.MarginBounds.Top + headerHeight + playerBoxHeight + spacing

        DrawMap(g, e.MarginBounds.Width, e.MarginBounds.Height - topOffset, False, e, topOffset)

        e.HasMorePages = False
    End Sub


    Private Sub DrawCapitalForOwner(g As Graphics, ownerIndex As Integer, xPos As Single, yPos As Single, tileSize As Single)
        Dim spriteName As String = Nothing

        Select Case ownerIndex
            Case 0 : spriteName = "elfcitadel.png"
            Case 1 : spriteName = "dwarfcitadel.png"
            Case 2 : spriteName = "orccitadel.png"
            Case 3 : spriteName = "humancitadel.png"
        End Select

        If spriteName Is Nothing Then Exit Sub
        If Not terrainCache.ContainsKey(spriteName) Then Exit Sub

        Dim img As Image = terrainCache(spriteName)
        g.DrawImage(img, xPos, yPos, tileSize, tileSize)
    End Sub

    Public Sub DrawMap(g As Graphics,
                    Optional width As Single = -1,
                    Optional height As Single = -1,
                    Optional isPanel As Boolean = True,
                    Optional e As PrintPageEventArgs = Nothing,
                    Optional topOffset As Single = 0)

        ' === Validate references ===
        If CurrentForm Is Nothing Then Exit Sub
        If terrainCache Is Nothing OrElse terrainCache.Count = 0 Then Exit Sub
        If CurrentForm.Map Is Nothing OrElse CurrentForm.Players Is Nothing Then Exit Sub

        Dim mapSize As Integer = 25
        Dim numberMargin As Single = 20

        ' --- Determine dimensions ---
        If width <= 0 Then width = CurrentForm.pnlMap.ClientSize.Width
        If height <= 0 Then height = CurrentForm.pnlMap.ClientSize.Height

        ' Compute tile size to fit map
        Dim tileSize As Single
        If isPanel Then
            tileSize = Math.Min((width - 2 * numberMargin) / mapSize, (height - 2 * numberMargin) / mapSize)
        Else
            tileSize = Math.Min((width - 2 * numberMargin) / mapSize, (height - 12) / mapSize)
        End If

        Dim totalMapWidth As Single = tileSize * mapSize
        Dim totalMapHeight As Single = tileSize * mapSize

        ' --- Calculate offsets ---
        Dim xOffset As Single
        Dim yOffset As Single

        If isPanel Then
            xOffset = numberMargin + (width - 2 * numberMargin - totalMapWidth) / 2
            yOffset = numberMargin + (height - 2 * numberMargin - totalMapHeight) / 2 + topOffset
            g.Clear(Color.White)
        Else
            xOffset = e.PageBounds.Left + (e.PageBounds.Width - totalMapWidth) / 2
            yOffset = e.PageBounds.Top + topOffset
        End If

        ' --- Draw terrain and ownership ---
        For x As Integer = 0 To mapSize - 1
            For y As Integer = 0 To mapSize - 1
                Dim terrainValue As Integer = CurrentForm.Map(x, y, 0)
                Dim terrainImage As Image = Nothing
                Select Case terrainValue
                    Case 0 : terrainImage = terrainCache("plains.png")
                    Case 1 : terrainImage = terrainCache("forest.png")
                    Case 2 : terrainImage = terrainCache("hills.png")
                    Case 3 : terrainImage = terrainCache("mountain.png")
                End Select

                Dim ownerIndex As Integer = CurrentForm.Map(x, y, 1)
                Dim ownerColor As Color = Color.White
                If ownerIndex >= 0 AndAlso ownerIndex < CurrentForm.playerColors.Length Then
                    ownerColor = CurrentForm.playerColors(ownerIndex)
                End If

                Dim xPos As Single = xOffset + x * tileSize
                Dim yPos As Single = yOffset + y * tileSize
                Dim w As Single = tileSize
                Dim h As Single = tileSize

                If isPanel AndAlso x = mapSize - 1 Then
                    w = Math.Min(w, CurrentForm.pnlMap.ClientSize.Width - (xOffset + x * tileSize))
                End If
                If isPanel AndAlso y = mapSize - 1 Then
                    h = Math.Min(h, CurrentForm.pnlMap.ClientSize.Height - (yOffset + y * tileSize))
                End If

                Using brush As New SolidBrush(ownerColor)
                    g.FillRectangle(brush, xPos, yPos, w, h)
                End Using

                ' === Draw either terrain or citadel ===
                Dim drewSomething As Boolean = False
                If CurrentForm.IsCapital(x, y) Then
                    If ownerIndex >= 0 AndAlso ownerIndex < CurrentForm.Players.Count Then
                        Dim ownerPlayer As Player = CurrentForm.Players(ownerIndex)
                        If ownerPlayer IsNot Nothing AndAlso Not ownerPlayer.IsEliminated Then
                            DrawCapitalForOwner(g, ownerIndex, xPos, yPos, tileSize)
                            drewSomething = True
                        End If
                    End If
                End If

                If Not drewSomething AndAlso terrainImage IsNot Nothing Then
                    g.DrawImage(terrainImage, xPos, yPos, w, h)
                End If
            Next
        Next

        ' --- Grid numbers ---
        Using font As New Font("Arial", 8)
            Using brush As New SolidBrush(Color.Black)
                Dim leftOffset As Single = xOffset - 18
                Dim rightOffset As Single = xOffset + totalMapWidth + 4
                For x As Integer = 0 To mapSize - 1
                    Dim xNumPos As Single = xOffset + x * tileSize + tileSize / 2
                    g.DrawString(x.ToString(), font, brush, xNumPos, yOffset - 12, New StringFormat() With {.Alignment = StringAlignment.Center})
                    g.DrawString(x.ToString(), font, brush, xNumPos, yOffset + totalMapHeight + 2, New StringFormat() With {.Alignment = StringAlignment.Center})
                Next
                For y As Integer = 0 To mapSize - 1
                    Dim yNumPos As Single = yOffset + y * tileSize + tileSize / 2
                    g.DrawString(y.ToString(), font, brush, leftOffset, yNumPos, New StringFormat() With {.LineAlignment = StringAlignment.Center})
                    g.DrawString(y.ToString(), font, brush, rightOffset, yNumPos, New StringFormat() With {.LineAlignment = StringAlignment.Center})
                Next
            End Using
        End Using

        ' --- Grid lines ---
        Using pen As New Pen(Color.Gray)
            For i As Integer = 0 To mapSize
                Dim yLine As Single = yOffset + i * tileSize
                Dim xLine As Single = xOffset + i * tileSize

                If isPanel AndAlso i = mapSize Then
                    yLine = Math.Min(yLine, CurrentForm.pnlMap.ClientSize.Height - 1)
                    xLine = Math.Min(xLine, CurrentForm.pnlMap.ClientSize.Width - 1)
                End If

                g.DrawLine(pen, xOffset, yLine, xOffset + totalMapWidth, yLine)
                g.DrawLine(pen, xLine, yOffset, xLine, yOffset + totalMapHeight)
            Next
        End Using

        ' --- Armies ---
        Dim tileTotals As New Dictionary(Of Point, Integer)
        Dim tileOwner As New Dictionary(Of Point, Integer)

        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated OrElse p.Armies Is Nothing Then Continue For
            For Each a In p.Armies
                Dim pt As New Point(a.X, a.Y)
                If tileTotals.ContainsKey(pt) Then
                    tileTotals(pt) += a.TotalSoldiers
                Else
                    tileTotals(pt) = a.TotalSoldiers
                    tileOwner(pt) = p.PlayerNumber
                End If
            Next
        Next

        For Each kvp In tileTotals
            Dim x = kvp.Key.X
            Dim y = kvp.Key.Y
            Dim totalSoldiers = kvp.Value

            Dim xPos As Single = xOffset + x * tileSize
            Dim yPos As Single = yOffset + y * tileSize

            Using dimBrush As New SolidBrush(Color.FromArgb(80, 0, 0, 0))
                g.FillRectangle(dimBrush, xPos, yPos, tileSize, tileSize)
            End Using

            Dim playerIndex As Integer = tileOwner(kvp.Key)
            Dim borderColor As Color
            Select Case playerIndex
                Case 0 : borderColor = Color.FromArgb(173, 255, 47)   ' Elf
                Case 1 : borderColor = Color.Cyan                     ' Dwarf
                Case 2 : borderColor = Color.Red                      ' Orc
                Case 3 : borderColor = Color.Orange                   ' Human
                Case Else : borderColor = Color.Gray
            End Select

            Using pen As New Pen(borderColor, Math.Max(1, tileSize * 0.1F))
                g.DrawRectangle(pen, xPos + 0.5F, yPos + 0.5F, tileSize - 1, tileSize - 1)
            End Using

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
            g.DrawString("Race: " & CurrentForm.Players(0).Race, font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 45)
            g.DrawString("Population: " & CurrentForm.Players(0).Population.ToString(), font, Brushes.Black, playerBox.Left + 5, playerBox.Top + 65)
        End Using
    End Sub


    ' ------------------------------------------------------------
    ' Main routine for one player: handles both print + HTML
    ' ------------------------------------------------------------
    Private Sub GenerateSinglePlayerReport(p As Player)
        Try
            ' Debug feedback
            Debug.WriteLine($"[REPORT] Generating report for {p.Nickname} ({p.Race})")

            ' === Prepare folder ===
            Dim gameName As String = $"Game{CurrentForm.GameNumber:D3}"
            Dim folder As String = Path.Combine(Application.StartupPath, "HTMLPrintouts", gameName)
            If Not Directory.Exists(folder) Then Directory.CreateDirectory(folder)

            ' === Construct file name ===
            Dim turnNum As Integer = CurrentForm.TurnNumber
            Dim safeNick As String = p.Nickname.ToLower().Replace(" ", "_")
            Dim fileName As String = $"{safeNick}_{gameName.ToLower()}_turn{turnNum:D3}.html"
            Dim filePath As String = Path.Combine(folder, fileName)

            ' === Build HTML ===
            Using writer As New StreamWriter(filePath, append:=False)
                writer.WriteLine("<html>")
                writer.WriteLine("<head><title>" & p.Nickname & " - Turn " & turnNum & "</title></head>")
                writer.WriteLine("<body style='font-family:Segoe UI, sans-serif;'>")
                writer.WriteLine("<h1>" & p.Nickname & " (" & p.Race & ")</h1>")
                writer.WriteLine("<h3>Game: " & gameName & " | Turn " & turnNum & "</h3>")
                writer.WriteLine("<hr>")
                writer.WriteLine("<p>Report generation placeholder. Sections will follow.</p>")
                writer.WriteLine("</body></html>")
            End Using

            ' === Printer output stub ===
            ' Later we’ll add a PrintDocument or PDF option here using the same data.
            Debug.WriteLine($"[REPORT] HTML saved: {filePath}")

        Catch ex As Exception
            Debug.WriteLine($"[REPORT ERROR] {ex.Message}")
        End Try
    End Sub

End Module
