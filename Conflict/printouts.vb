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

    Public printDoc As PrintDocument

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

    ' ============================================================
    '  Diagnostic Print Page and Header Routine
    ' ============================================================

    Private Sub printDoc_PrintPage(sender As Object, e As PrintPageEventArgs)
        Static pageIndex As Integer = 1

        Dim g As Graphics = e.Graphics
        g.Clear(Color.White)

        ' === Safety ===
        If CurrentForm Is Nothing OrElse CurrentForm.Players Is Nothing Then
            e.HasMorePages = False
            pageIndex = 1
            Return
        End If

        ' === Identify current player ===
        Dim p As Player = CurrentForm.Players.FirstOrDefault(Function(pp) pp IsNot Nothing AndAlso Not pp.AIControlled)
        If p Is Nothing Then p = CurrentForm.Players.FirstOrDefault(Function(pp) pp IsNot Nothing)
        If p Is Nothing Then
            g.DrawString("(No player data found)", New Font("Arial", 12), Brushes.Black, 100, 100)
            e.HasMorePages = False
            pageIndex = 1
            Return
        End If

        ' ------------------------------------------------------------
        ' PAGE 1: Header + Map  (TEMPORARILY DISABLED to save ink/paper)
        ' ------------------------------------------------------------
        If pageIndex = 1 Then
            ' === SKIPPED FOR TESTING ===
            ' To re-enable later, just uncomment the next three blocks:

            'DrawFrontPageHeaderAndPlayerInfo(g, e)
            'If CurrentForm.Map IsNot Nothing Then
            '    Dim headerHeight As Single = 60
            '    Dim playerBoxHeight As Single = 80
            '    Dim spacing As Single = 20
            '    Dim topOffset As Single = e.MarginBounds.Top + headerHeight + playerBoxHeight + spacing + 100
            '    DrawMap(g, e.MarginBounds.Width, e.MarginBounds.Height - topOffset, False, e, topOffset)
            'End If

            ' still signal there’s another page so page 2 prints correctly
            e.HasMorePages = True
            pageIndex = 2
            Return

            ' ------------------------------------------------------------
            ' PAGE 2: Empire Report
            ' ------------------------------------------------------------
        ElseIf pageIndex = 2 Then
            DrawEmpireReport(g, e, p)
            e.HasMorePages = False
            pageIndex = 1
            Return
        End If
    End Sub


    Private Sub DrawFrontPageHeaderAndPlayerInfo(g As Graphics, e As PrintPageEventArgs)
        If CurrentForm Is Nothing Then Exit Sub
        If CurrentForm.Players Is Nothing OrElse CurrentForm.Players.Count = 0 Then Exit Sub

        Dim marginLeft As Single = e.MarginBounds.Left
        Dim marginTop As Single = e.MarginBounds.Top
        Dim pageWidth As Single = e.MarginBounds.Width

        ' === Identify current player ===
        Dim p As Player = CurrentForm.Players.FirstOrDefault(Function(pp) pp IsNot Nothing AndAlso Not pp.AIControlled)
        If p Is Nothing Then p = CurrentForm.Players.FirstOrDefault(Function(pp) pp IsNot Nothing)
        If p Is Nothing Then
            g.DrawString("(No player data found)", New Font("Arial", 12), Brushes.Black, marginLeft + 40, marginTop + 40)
            Exit Sub
        End If

        ' === Try to find matching customer ===
        Dim cust = CurrentForm.Customers?.FirstOrDefault(Function(c) _
        c.Nickname IsNot Nothing AndAlso c.Nickname.Equals(p.Nickname, StringComparison.OrdinalIgnoreCase))

        ' ------------------------------------------------------------
        ' 1. COLOUR FANTASY TITLE
        ' ------------------------------------------------------------
        Dim titleRect As New RectangleF(marginLeft, marginTop, pageWidth, 60)
        Dim sf As New StringFormat() With {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center}

        ' Choose a fantasy font if installed
        Dim fontName As String = "Papyrus"
        If Not FontFamily.Families.Any(Function(f) f.Name.Equals(fontName, StringComparison.OrdinalIgnoreCase)) Then
            fontName = "Old English Text MT"
            If Not FontFamily.Families.Any(Function(f) f.Name.Equals(fontName, StringComparison.OrdinalIgnoreCase)) Then
                fontName = "Times New Roman"
            End If
        End If

        Using fontTitle As New Font(fontName, 48, FontStyle.Bold, GraphicsUnit.Point)
            ' Soft shadow
            Using shadowBrush As New SolidBrush(Color.FromArgb(160, 60, 0, 0))
                g.DrawString("CONFLICT", fontTitle, shadowBrush,
                         New RectangleF(titleRect.Left + 3, titleRect.Top + 3, titleRect.Width, titleRect.Height), sf)
            End Using

            ' Red → Gold gradient main fill
            Using gradBrush As New Drawing2D.LinearGradientBrush(titleRect,
                                                             Color.DarkRed,
                                                             Color.Gold,
                                                             Drawing2D.LinearGradientMode.Horizontal)
                g.DrawString("CONFLICT", fontTitle, gradBrush, titleRect, sf)
            End Using
        End Using

        ' ------------------------------------------------------------
        ' 2. PUBLISHER ADDRESS
        ' ------------------------------------------------------------
        Using fontAddr As New Font("Arial", 12, FontStyle.Italic)
            g.DrawString("7 Trent Drive, Hucknall, Notts. NG15 6GR", fontAddr, Brushes.Black,
                     New RectangleF(marginLeft, marginTop + 65, pageWidth, 20),
                     New StringFormat() With {.Alignment = StringAlignment.Center})
        End Using

        Dim yPosition As Single = marginTop + 110

        ' ------------------------------------------------------------
        ' 3. CUSTOMER NAME + ADDRESS
        ' ------------------------------------------------------------
        Using fontBody As New Font("Arial", 12)
            Dim y As Single = yPosition
            If cust IsNot Nothing Then
                If Not String.IsNullOrWhiteSpace(cust.Name) Then
                    g.DrawString(cust.Name, fontBody, Brushes.Black, marginLeft + 40, y)
                    y += 20
                End If
                If Not String.IsNullOrWhiteSpace(cust.Address) Then
                    Dim parts = cust.Address.Split({","c}, StringSplitOptions.RemoveEmptyEntries)
                    For Each line In parts
                        g.DrawString(line.Trim(), fontBody, Brushes.Black, marginLeft + 40, y)
                        y += 20
                    Next
                End If
            Else
                g.DrawString("(Customer not found in file)", fontBody, Brushes.Black, marginLeft + 40, y)
                y += 20
            End If
            yPosition = y + 10
        End Using

        ' ------------------------------------------------------------
        ' 4. GAME DETAILS (RIGHT SIDE)
        ' ------------------------------------------------------------
        Using fontDetails As New Font("Arial", 12)
            Dim rightX As Single = marginLeft + pageWidth - 260
            Dim y As Single = marginTop + 120
            g.DrawString($"Game No: {CurrentForm.GameNumber}", fontDetails, Brushes.Black, rightX, y) : y += 20
            g.DrawString($"Turn No: {CurrentForm.TurnNumber}", fontDetails, Brushes.Black, rightX, y) : y += 20
            g.DrawString($"Date: {DateTime.Now:dd MMM yyyy}", fontDetails, Brushes.Black, rightX, y) : y += 20
            g.DrawString($"Race: {p.Race}", fontDetails, Brushes.Black, rightX, y) : y += 20
        End Using

        ' ------------------------------------------------------------
        ' 5. DIVIDER LINE BEFORE MAP
        ' ------------------------------------------------------------
        Using pen As New Pen(Color.Black, 1)
            g.DrawLine(pen, marginLeft, yPosition + 5, marginLeft + pageWidth, yPosition + 5)
        End Using
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
            ' PANEL RENDERING (unchanged)
            xOffset = numberMargin + (width - 2 * numberMargin - totalMapWidth) / 2
            yOffset = numberMargin + (height - 2 * numberMargin - totalMapHeight) / 2 + topOffset
            g.Clear(Color.White)
        Else
            ' PRINT / EXPORT RENDERING
            ' Safe guards for e = Nothing (e.g. HTML export)
            Dim pageLeft As Single = If(e IsNot Nothing, e.PageBounds.Left, 0)
            Dim pageTop As Single = If(e IsNot Nothing, e.PageBounds.Top, 0)
            Dim pageWidth As Single = If(e IsNot Nothing, e.PageBounds.Width, width)
            xOffset = pageLeft + (pageWidth - totalMapWidth) / 2
            yOffset = pageTop + topOffset
            ' Do NOT clear for print/export; caller handles background
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


    ' ------------------------------------------------------------
    '  Generate HTML turn reports for all active (non-AI) players
    ' ------------------------------------------------------------
    Public Sub GenerateAllHTMLReports()
        If CurrentForm Is Nothing OrElse CurrentForm.Players Is Nothing Then Exit Sub

        ' === Create flat HTML folder next to Saves ===
        Dim htmlRoot As String = Path.Combine(Application.StartupPath, "HTML")
        If Not Directory.Exists(htmlRoot) Then Directory.CreateDirectory(htmlRoot)

        Dim gameNum As Integer = CurrentForm.GameNumber
        Dim turnNum As Integer = CurrentForm.TurnNumber

        For Each p In CurrentForm.Players
            If p Is Nothing OrElse p.IsEliminated OrElse p.AIControlled Then Continue For
            GenerateSinglePlayerReport(p, htmlRoot, gameNum, turnNum)
        Next
    End Sub

    ' ------------------------------------------------------------
    '  Generate one player's HTML turn skeleton (with embedded map)
    ' ------------------------------------------------------------
    Private Sub GenerateSinglePlayerReport(p As Player, htmlRoot As String, gameNum As Integer, turnNum As Integer)
        Try
            ' === Build file name ===
            Dim safeNick As String = p.Nickname.ToLower().Replace(" ", "_").Replace("""", "").Replace("'", "")
            Dim fileName As String = $"{safeNick}_game{gameNum:D3}_turn{turnNum:D3}.html"
            Dim filePath As String = Path.Combine(htmlRoot, fileName)

            ' === Generate map image ===
            Dim base64Map As String = GetMapImageBase64(p, gameNum, turnNum)

            ' === Basic HTML skeleton ===
            Using writer As New StreamWriter(filePath, append:=False)
                writer.WriteLine("<!DOCTYPE html>")
                writer.WriteLine("<html lang='en'>")
                writer.WriteLine("<head>")
                writer.WriteLine($"<meta charset='UTF-8'>")
                writer.WriteLine($"<title>Conflict Turn {turnNum} – {p.Nickname} ({p.Race})</title>")
                writer.WriteLine("<style>")
                writer.WriteLine("body { font-family: Segoe UI, Arial, sans-serif; background-color: #fafafa; margin: 30px; }")
                writer.WriteLine("h1 { color: #333; } h2 { border-bottom: 1px solid #ccc; padding-bottom: 4px; }")
                writer.WriteLine("table { border-collapse: collapse; width: 100%; margin-top: 8px; }")
                writer.WriteLine("th, td { border: 1px solid #ddd; padding: 6px 8px; text-align: left; }")
                writer.WriteLine(".section { margin-bottom: 30px; }")
                writer.WriteLine("</style>")
                writer.WriteLine("</head>")
                writer.WriteLine("<body>")

                writer.WriteLine($"<h1>Conflict – Turn {turnNum}</h1>")
                writer.WriteLine($"<h2>{p.Nickname} ({p.Race})</h2>")
                writer.WriteLine("<hr>")

                ' === Section skeletons ===
                ' === Empire and Resource Report ===
                AppendEmpireReportHTML(writer, p)

                ' === Map section ===
                If base64Map <> "" Then
                    writer.WriteLine("<div class='section'><h2>Map Overview</h2>")
                    writer.WriteLine($"<img src='data:image/png;base64,{base64Map}' alt='Map for {p.Nickname}' style='border:1px solid #999; width:600px; height:600px;'>")
                    writer.WriteLine("</div>")
                Else
                    writer.WriteLine("<div class='section'><h2>Map Overview</h2><p>[Map unavailable]</p></div>")
                End If

                writer.WriteLine("<div class='section'><h2>Armies</h2><p>[Army list placeholder]</p></div>")
                writer.WriteLine("<div class='section'><h2>Events</h2><p>[Recent events placeholder]</p></div>")
                writer.WriteLine("<div class='section'><h2>Notes</h2><p>[Additional notes placeholder]</p></div>")

                writer.WriteLine($"<p style='font-size:11px;color:#888;'>Generated automatically on {DateTime.Now:dd MMM yyyy HH:mm}</p>")
                writer.WriteLine("</body></html>")
            End Using

            Debug.WriteLine($"[HTML TURN] Created: {filePath}")

        Catch ex As Exception
            Debug.WriteLine($"[HTML TURN ERROR] {p.Nickname}: {ex.Message}")
        End Try
    End Sub

    Private Function GetMapImageBase64(p As Player, gameNum As Integer, turnNum As Integer) As String
        Try
            Const IMAGE_SIZE As Integer = 800

            Using bmp As New Bitmap(IMAGE_SIZE, IMAGE_SIZE)
                Using g As Graphics = Graphics.FromImage(bmp)
                    g.Clear(Color.White)

                    ' Draw in printer layout mode, but with e = Nothing (now handled safely)
                    DrawMap(g, IMAGE_SIZE, IMAGE_SIZE, False, Nothing, 0)
                End Using

                Using ms As New MemoryStream()
                    bmp.Save(ms, Imaging.ImageFormat.Png)
                    Return Convert.ToBase64String(ms.ToArray())
                End Using
            End Using

        Catch ex As Exception
            Debug.WriteLine($"[HTML MAP ERROR] {ex.Message}")
            Return ""
        End Try
    End Function

    ' ============================================================
    '  PAGE 2 – Empire Report
    ' ============================================================

    Private Sub DrawEmpireReport(g As Graphics, e As PrintPageEventArgs, p As Player)
        Dim marginLeft As Single = e.MarginBounds.Left
        Dim marginTop As Single = e.MarginBounds.Top
        Dim pageWidth As Single = e.MarginBounds.Width
        Dim y As Single = marginTop

        Using titleFont As New Font("Arial", 22, FontStyle.Bold)
            g.DrawString("Empire Report", titleFont, Brushes.DarkBlue, marginLeft + 10, y)
        End Using
        y += 40

        Using bodyFont As New Font("Arial", 12)
            ' === RACE / TERRAIN SECTION ===
            g.DrawString($"Race: {p.Race}", bodyFont, Brushes.Black, marginLeft + 10, y) : y += 25

            ' --- Count owned squares + terrain ---
            Dim counts As New Dictionary(Of Integer, Integer) From {{0, 0}, {1, 0}, {2, 0}, {3, 0}}
            For x = 0 To 24
                For y2 = 0 To 24
                    If CurrentForm.Map(x, y2, 1) = p.PlayerNumber Then
                        counts(CurrentForm.Map(x, y2, 0)) += 1
                    End If
                Next
            Next
            Dim totalOwned As Integer = counts.Values.Sum()
            g.DrawString($"Empire Size: {totalOwned} squares", bodyFont, Brushes.Black, marginLeft + 10, y) : y += 25
            g.DrawString($"   Plains: {counts(0)}   Forest: {counts(1)}   Hills: {counts(2)}   Mountain: {counts(3)}", bodyFont, Brushes.Black, marginLeft + 20, y)
            y += 40

            ' === POPULATION SECTION ===
            g.DrawString("Population:", bodyFont, Brushes.Black, marginLeft + 10, y)
            y += 25

            ' --- Calculate percentage growth ---
            Dim growthPercent As Double = 0
            If p.Population - p.PopulationGrowthThisTurn <> 0 Then
                growthPercent = (p.PopulationGrowthThisTurn / Math.Max(1, (p.Population - p.PopulationGrowthThisTurn))) * 100.0
            End If

            Using monoFont As New Font("Consolas", 12)
                Dim sign As String = If(p.PopulationGrowthThisTurn >= 0, "+", "")
                g.DrawString(
                $"Total : {p.Population,10:N0}     Growth this Turn : {p.PopulationGrowthThisTurn,10:N0}  ({sign}{growthPercent,5:F1}%)",
                monoFont, Brushes.Black, marginLeft + 40, y)
                y += 40
            End Using

            ' === RESOURCE SECTION ===
            g.DrawString("Resources:", bodyFont, Brushes.Black, marginLeft + 10, y)
            y += 25

            Using monoFont As New Font("Consolas", 12)
                Dim lines As String() = {
                $"Wood Total : {p.Wood,10:N0}     Collected : {p.WoodCollectedThisTurn,10:N0}",
                $"Iron Total : {p.Iron,10:N0}     Collected : {p.IronCollectedThisTurn,10:N0}",
                $"Gold Total : {p.Gold,10:N0}     Collected : {p.GoldCollectedThisTurn,10:N0}"
            }

                For Each line In lines
                    g.DrawString(line, monoFont, Brushes.Black, marginLeft + 40, y)
                    y += 22
                Next
            End Using
        End Using
    End Sub


    ' ============================================================
    '  Empire Report for HTML
    ' ============================================================

    Private Sub AppendEmpireReportHTML(writer As StreamWriter, p As Player)
        writer.WriteLine("<div class='section'><h2>Empire Report</h2>")
        writer.WriteLine($"<p><strong>Race:</strong> {p.Race}</p>")

        ' --- Terrain counts ---
        Dim counts As New Dictionary(Of Integer, Integer) From {{0, 0}, {1, 0}, {2, 0}, {3, 0}}
        For x = 0 To 24
            For y = 0 To 24
                If CurrentForm.Map(x, y, 1) = p.PlayerNumber Then
                    counts(CurrentForm.Map(x, y, 0)) += 1
                End If
            Next
        Next
        Dim totalOwned As Integer = counts.Values.Sum()
        writer.WriteLine($"<p><strong>Empire Size:</strong> {totalOwned} squares<br>")
        writer.WriteLine($"Plains: {counts(0)}, Forest: {counts(1)}, Hills: {counts(2)}, Mountain: {counts(3)}</p>")

        ' === POPULATION TABLE ===
        Dim growthPercent As Double = 0
        If p.Population - p.PopulationGrowthThisTurn <> 0 Then
            growthPercent = (p.PopulationGrowthThisTurn / Math.Max(1, (p.Population - p.PopulationGrowthThisTurn))) * 100.0
        End If
        Dim sign As String = If(p.PopulationGrowthThisTurn >= 0, "+", "")

        writer.WriteLine("<h3>Population</h3>")
        writer.WriteLine("<table>")
        writer.WriteLine("<tr><th>Total</th><th>Growth This Turn</th><th>% Change</th></tr>")
        writer.WriteLine($"<tr><td>{p.Population:N0}</td><td>{p.PopulationGrowthThisTurn:N0}</td><td>{sign}{growthPercent:F1}%</td></tr>")
        writer.WriteLine("</table>")

        ' === RESOURCE TABLE ===
        writer.WriteLine("<h3>Resources</h3>")
        writer.WriteLine("<table>")
        writer.WriteLine("<tr><th>Resource</th><th>Total</th><th>Collected This Turn</th></tr>")
        writer.WriteLine($"<tr><td>Wood</td><td>{p.Wood:N0}</td><td>{p.WoodCollectedThisTurn:N0}</td></tr>")
        writer.WriteLine($"<tr><td>Iron</td><td>{p.Iron:N0}</td><td>{p.IronCollectedThisTurn:N0}</td></tr>")
        writer.WriteLine($"<tr><td>Gold</td><td>{p.Gold:N0}</td><td>{p.GoldCollectedThisTurn:N0}</td></tr>")
        writer.WriteLine("</table>")
        writer.WriteLine("</div>")
    End Sub


End Module
