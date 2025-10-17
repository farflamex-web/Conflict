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
Imports System.Text

Module Printouts

    ' Resume position inside the current battle (line index)
    Private battleLineIndex As Integer = 0
    Private lastPrintedBattleIndex As Integer = -1

    Public printDoc As PrintDocument

    Public terrainCache As New Dictionary(Of String, Image)

    Private hasDrawnBattleHeader As Boolean = False

    ' Track print flow/state
    Private pageIndex As Integer = 1
    Private battleReportIndex As Integer = 0
    Private currentPrintPlayer As Player = Nothing

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
        AddHandler printDoc.BeginPrint, AddressOf printDoc_BeginPrint
        AddHandler printDoc.PrintPage, AddressOf printDoc_PrintPage
    End Sub

    Private Sub printDoc_BeginPrint(sender As Object, e As PrintEventArgs)
        pageIndex = 1
        battleReportIndex = 0
        battleLineIndex = 0
        currentPrintPlayer = Nothing
        hasDrawnBattleHeader = False
    End Sub



    ' ============================================================
    '  Diagnostic Print Page and Header Routine
    ' ============================================================

    Private Sub printDoc_PrintPage(sender As Object, e As PrintPageEventArgs)
        Dim g As Graphics = e.Graphics
        g.Clear(Color.White)

        ' Emergency runaway page limiter
        If pageIndex > 10 Then
            g.DrawString("Printing aborted: too many pages.", New Font("Arial", 12), Brushes.Black, 100, 100)
            e.HasMorePages = False
            pageIndex = 1
            battleReportIndex = 0
            currentPrintPlayer = Nothing
            Return
        End If

        ' === Safety ===
        If CurrentForm Is Nothing OrElse CurrentForm.Players Is Nothing Then
            e.HasMorePages = False
            pageIndex = 1
            battleReportIndex = 0
            currentPrintPlayer = Nothing
            Return
        End If

        ' === Identify current player once per job ===
        If currentPrintPlayer Is Nothing Then
            currentPrintPlayer = CurrentForm.Players.
            FirstOrDefault(Function(pp) pp IsNot Nothing AndAlso Not pp.AIControlled)
            If currentPrintPlayer Is Nothing Then
                currentPrintPlayer = CurrentForm.Players.FirstOrDefault(Function(pp) pp IsNot Nothing)
            End If
        End If

        If currentPrintPlayer Is Nothing Then
            g.DrawString("(No player data found)", New Font("Arial", 12), Brushes.Black, 100, 100)
            e.HasMorePages = False
            pageIndex = 1
            battleReportIndex = 0
            Return
        End If

        ' === Detect if there are any battles to print ===
        Dim hasBattles As Boolean =
        (CurrentForm.CurrentReports IsNot Nothing AndAlso
         CurrentForm.CurrentReports.BattleReports IsNot Nothing AndAlso
         CurrentForm.CurrentReports.BattleReports.Count > 0)

        Select Case pageIndex
            Case 1
                ' --- PAGE 1: HEADER + MAP ---
                DrawFrontPageHeaderAndPlayerInfo(g, e)
                If CurrentForm.Map IsNot Nothing Then
                    Dim headerHeight As Single = 60
                    Dim playerBoxHeight As Single = 80
                    Dim spacing As Single = 20
                    Dim topOffset As Single = e.MarginBounds.Top + headerHeight + playerBoxHeight + spacing + 100
                    DrawMap(g, e.MarginBounds.Width, e.MarginBounds.Height - topOffset, False, e, topOffset)
                End If

                ' Move on to page 2
                e.HasMorePages = True
                pageIndex = 2
                Return

            Case 2
                ' --- PAGE 2: EMPIRE REPORT ---
                DrawEmpireReport(g, e, currentPrintPlayer)

                ' Decide next page based on battles
                e.HasMorePages = True
                If hasBattles Then
                    pageIndex = 3
                Else
                    pageIndex = 4   ' Skip battles if none
                End If
                Return

            Case 3
                ' --- PAGE 3+: BATTLE REPORTS (may span multiple pages) ---
                Dim morePages As Boolean = DrawBattleReportsPage(g, e, currentPrintPlayer)

                If morePages Then
                    ' Still more battle pages to print
                    e.HasMorePages = True
                    pageIndex = 3
                Else
                    ' All battle reports done — continue to armies
                    e.HasMorePages = True
                    pageIndex = 4
                End If
                Return

            Case 4
                ' --- PAGE 4: ARMIES ---
                If DrawArmiesPage(g, e, currentPrintPlayer) Then
                    e.HasMorePages = True
                Else
                    ' Done printing — reset for next time
                    e.HasMorePages = False
                    pageIndex = 1
                    battleReportIndex = 0
                    currentPrintPlayer = Nothing
                End If
                Return

            Case Else
                ' --- Safety fallback ---
                e.HasMorePages = False
                pageIndex = 1
                battleReportIndex = 0
                currentPrintPlayer = Nothing
                Return
        End Select
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

        Dim mapSize As Integer = CurrentForm.Map.GetLength(0)
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
        Dim mapWidth As Integer = CurrentForm.Map.GetLength(0)
        Dim mapHeight As Integer = CurrentForm.Map.GetLength(1)

        For x = 0 To mapWidth - 1
            For y = 0 To mapHeight - 1
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
            ' === Build file name with fallback for empty nickname ===
            Dim rawNick As String = If(String.IsNullOrWhiteSpace(p.Nickname), $"player{p.PlayerNumber}", p.Nickname)
            Dim safeNick As String = System.Text.RegularExpressions.Regex.Replace(rawNick.ToLower(), "[^a-z0-9_]", "")
            Dim fileName As String = $"{safeNick}_game{gameNum:D3}_turn{turnNum:D3}.html"
            Dim filePath As String = Path.Combine(htmlRoot, fileName)

            ' === Generate map image ===
            Dim base64Map As String = GetMapImageBase64(p, gameNum, turnNum)

            ' === HTML encoding helper ===
            Dim HtmlEncode As Func(Of String, String) = Function(s)
                                                            If s Is Nothing Then Return ""
                                                            Return System.Net.WebUtility.HtmlEncode(s)
                                                        End Function

            ' === Basic HTML skeleton ===
            Using writer As New StreamWriter(filePath, append:=False)
                writer.WriteLine("<!DOCTYPE html>")
                writer.WriteLine("<html lang='en'>")
                writer.WriteLine("<head>")
                writer.WriteLine($"<meta charset='UTF-8'>")
                writer.WriteLine($"<title>Conflict Turn {turnNum} – {HtmlEncode(p.Nickname)} ({HtmlEncode(p.Race)})</title>")
                writer.WriteLine("<style>")
                writer.WriteLine("body { font-family: Segoe UI, Arial, sans-serif; background-color: #fafafa; margin: 30px; }")
                writer.WriteLine("h1 { color: #333; } h2 { border-bottom: 1px solid #ccc; padding-bottom: 4px; }")
                writer.WriteLine("table { border-collapse: collapse; width: 100%; margin-top: 8px; }")
                writer.WriteLine("th, td { border: 1px solid #ddd; padding: 6px 8px; text-align: left; }")
                writer.WriteLine(".section { margin-bottom: 30px; }")
                writer.WriteLine("pre { white-space: pre-wrap; word-wrap: break-word; overflow-x: auto; }")
                writer.WriteLine("</style>")
                writer.WriteLine("</head>")
                writer.WriteLine("<body>")

                writer.WriteLine($"<h1>Conflict – Turn {turnNum}</h1>")
                writer.WriteLine($"<h2>{HtmlEncode(rawNick)} ({HtmlEncode(p.Race)})</h2>")
                writer.WriteLine("<hr>")

                ' === Section skeletons ===
                ' === Empire and Resource Report ===
                AppendEmpireReportHTML(writer, p)

                ' === Map section ===
                If base64Map <> "" Then
                    writer.WriteLine("<div class='section'><h2>Map Overview</h2>")
                    writer.WriteLine($"<img src='data:image/png;base64,{base64Map}' alt='Map for {HtmlEncode(rawNick)}' style='border:1px solid #999; width:600px; height:600px;'>")
                    writer.WriteLine("</div>")
                Else
                    writer.WriteLine("<div class='section'><h2>Map Overview</h2><p>[Map unavailable]</p></div>")
                End If

                writer.WriteLine("<div class='section'><h2>Armies</h2><p>[Army list placeholder]</p></div>")
                writer.WriteLine("<div class='section'><h2>Events</h2><p>[Recent events placeholder]</p></div>")
                ' === Battle Reports Section ===
                ' === Battle Reports Section ===
                If CurrentForm.CurrentReports IsNot Nothing AndAlso
   CurrentForm.CurrentReports.BattleReports IsNot Nothing AndAlso
   CurrentForm.CurrentReports.BattleReports.Count > 0 Then

                    writer.WriteLine("<div class='section'><h2>Battle Reports</h2>")

                    ' --- Race colour dictionary (HTML-safe) ---
                    Dim raceColor As New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase) From {
                        {"Elf", "#228B22"},      ' forest green
                        {"Dwarf", "#4169E1"},    ' royal blue
                        {"Orc", "#B22222"},      ' firebrick
                        {"Human", "#DAA520"}     ' goldenrod
                    }

                    For Each battleText In CurrentForm.CurrentReports.BattleReports
                        ' Determine involved races
                        Dim involved As New List(Of String)
                        If battleText.Contains("Elf") Then involved.Add("Elf")
                        If battleText.Contains("Dwarf") Then involved.Add("Dwarf")
                        If battleText.Contains("Human") Then involved.Add("Human")
                        If battleText.Contains("Orc") Then involved.Add("Orc")

                        Dim htmlText As String = GetBattleReportForPlayer(p, battleText, involved)
                        Dim lines = htmlText.Replace(vbCrLf, vbLf).Split({vbLf}, StringSplitOptions.None)
                        Dim sbHtml As New StringBuilder()

                        For Each line In lines
                            Dim trimmed = line.Trim()
                            Dim colour As String = "#000000"
                            Dim bold As Boolean = False

                            ' --- Section headers ---
                            If trimmed.StartsWith("Battle at (", StringComparison.OrdinalIgnoreCase) Then
                                colour = "#4169E1" : bold = True
                            ElseIf {"Ranged Phase", "Charge Phase", "Melee Phase", "Chase Phase"}.Contains(trimmed) Then
                                colour = "#4169E1" : bold = True
                            ElseIf trimmed.StartsWith("Start of Battle", StringComparison.OrdinalIgnoreCase) _
                                OrElse trimmed.Equals("Final Army Status", StringComparison.OrdinalIgnoreCase) _
                                OrElse trimmed.Equals("End of Battle", StringComparison.OrdinalIgnoreCase) Then
                                colour = "#B22222" : bold = True
                            Else
                                ' --- Race lines ---
                                Dim mArmy = System.Text.RegularExpressions.Regex.Match(trimmed, "^(Elf|Dwarf|Orc|Human)\s+Army\b", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
                                If mArmy.Success Then
                                    colour = raceColor(mArmy.Groups(1).Value) : bold = True
                                End If
                                Dim mVic = System.Text.RegularExpressions.Regex.Match(trimmed, "^(Elf|Dwarf|Orc|Human)\s+Victory\b", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
                                If mVic.Success Then
                                    colour = raceColor(mVic.Groups(1).Value) : bold = True
                                End If
                            End If

                            ' --- Separator lines (---- / ====) ---
                            If trimmed.Length >= 5 AndAlso (trimmed.All(Function(c) c = "-"c) OrElse trimmed.All(Function(c) c = "="c)) Then
                                sbHtml.AppendLine("<hr style='border:0;border-top:1px solid #ccc;margin:6px 0;'>")
                            Else
                                Dim safeLine = System.Net.WebUtility.HtmlEncode(line)
                                If bold Then safeLine = $"<strong><span style='color:{colour}'>{safeLine}</span></strong>"
                                sbHtml.AppendLine(safeLine & "<br>")
                            End If
                        Next

                        writer.WriteLine($"<pre style='font-family:Consolas;font-size:12px;background:#f7f7f7;padding:10px;border:1px solid #ddd;white-space:pre-wrap;word-wrap:break-word;overflow-x:auto;'>{sbHtml}</pre>")
                    Next

                    writer.WriteLine("</div>")
                Else
                    writer.WriteLine("<div class='section'><h2>Battle Reports</h2><p>No battles occurred this turn.</p></div>")
                End If

                writer.WriteLine($"<p style='font-size:11px;color:#888;'>Generated automatically on {DateTime.Now:dd MMM yyyy HH:mm}</p>")
                writer.WriteLine("</body></html>")
            End Using

            Debug.WriteLine($"[HTML TURN] Created: {filePath}")

        Catch ex As Exception
            Debug.WriteLine($"[HTML TURN ERROR] {p.Nickname}: {ex.Message}")
        End Try
    End Sub

    ' ------------------------------------------------------------
    '  Returns a battle report text tailored to the given player.
    '  If the player was involved, they get the full report.
    '  Otherwise, they get a short summary (observer version).
    ' ------------------------------------------------------------
    Private Function GetBattleReportForPlayer(p As Player, battle As String, involvedRaces As List(Of String)) As String
        Dim sb As New System.Text.StringBuilder()

        Dim isInvolved As Boolean = involvedRaces.Any(Function(r) p.Race.Equals(r, StringComparison.OrdinalIgnoreCase))

        If isInvolved Then
            ' --- Full version for participants ---
            sb.AppendLine(battle)
        Else
            ' --- Short summary for observers ---
            Dim lines = battle.Split({vbCrLf, vbLf}, StringSplitOptions.RemoveEmptyEntries)
            Dim firstLine As String = lines.FirstOrDefault()
            Dim victoryLine As String = lines.FirstOrDefault(Function(l) l.Contains("Victory", StringComparison.OrdinalIgnoreCase))
            Dim lossLines = lines.Where(Function(l) l.StartsWith("Losses:", StringComparison.OrdinalIgnoreCase)).ToList()

            sb.AppendLine("Battle summary:")
            If Not String.IsNullOrEmpty(firstLine) Then sb.AppendLine(" " & firstLine)
            If Not String.IsNullOrEmpty(victoryLine) Then sb.AppendLine(" " & victoryLine)
            For Each l In lossLines
                sb.AppendLine(" " & l)
            Next
            sb.AppendLine()
        End If

        Return sb.ToString()
    End Function




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

            ' --- Count owned squares + terrain dynamically ---
            Dim mapSizeX As Integer = CurrentForm.Map.GetLength(0)
            Dim mapSizeY As Integer = CurrentForm.Map.GetLength(1)
            Dim counts As New Dictionary(Of Integer, Integer) From {{0, 0}, {1, 0}, {2, 0}, {3, 0}}

            For x = 0 To mapSizeX - 1
                For y2 = 0 To mapSizeY - 1
                    If CurrentForm.Map(x, y2, 1) = p.PlayerNumber Then
                        counts(CurrentForm.Map(x, y2, 0)) += 1
                    End If
                Next
            Next

            Dim totalOwned As Integer = counts.Values.Sum()
            g.DrawString($"Empire Size: {totalOwned} squares", bodyFont, Brushes.Black, marginLeft + 10, y) : y += 25
            g.DrawString($"   Plains: {counts(0)}   Forest: {counts(1)}   Hills: {counts(2)}   Mountain: {counts(3)}",
                     bodyFont, Brushes.Black, marginLeft + 20, y)
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
                ' --- Investment name (used later in ledger too) ---
                Dim investmentName As String = ""
                Select Case p.Race.ToLower()
                    Case "elf" : investmentName = "Sacred Groves"
                    Case "dwarf" : investmentName = "Silver Mines"
                    Case "orc" : investmentName = "Hunting Camps"
                    Case "human" : investmentName = "Farming Estates"
                    Case Else : investmentName = "Investments"
                End Select

                ' --- Mount name ---
                Dim mountName As String = ""
                Select Case p.Race.ToLower()
                    Case "elf" : mountName = "Forest Elks"
                    Case "dwarf" : mountName = "Mountain Rams"
                    Case "orc" : mountName = "Wargs"
                    Case "human" : mountName = "Horses"
                    Case Else : mountName = "Mounts"
                End Select

                ' --- Column-aligned width: match the longest label "Mountain Rams :" ---
                Dim labelWidth As Integer = 16

                ' --- Wood, Iron, Mounts ---
                g.DrawString($"{ "Wood Total".PadRight(labelWidth)}: {p.Wood,10:N0}     Collected : {p.WoodCollectedThisTurn,10:N0}",
                         monoFont, Brushes.Black, marginLeft + 40, y)
                y += 22
                g.DrawString($"{ "Iron Total".PadRight(labelWidth)}: {p.Iron,10:N0}     Collected : {p.IronCollectedThisTurn,10:N0}",
                         monoFont, Brushes.Black, marginLeft + 40, y)
                y += 22
                g.DrawString($"{mountName.PadRight(labelWidth)}: {p.Mounts,10:N0}     Collected : {p.MountsCollectedThisTurn,10:N0}",
                         monoFont, Brushes.Black, marginLeft + 40, y)
                y += 32

                ' === INVESTMENT SUMMARY (moved up before gold ledger) ===
                Dim totalIncome As Integer = p.Investments * 100
                g.DrawString($"{investmentName} : {p.Investments,3:N0}, Earning {totalIncome,6:N0} gold per turn",
                         monoFont, Brushes.Blue, marginLeft + 60, y)
                y += 35

                ' --- Gold Ledger Header ---
                g.DrawString("Gold Ledger:", bodyFont, Brushes.Black, marginLeft + 10, y)
                y += 25

                ' --- Ledger helper for consistent alignment ---
                Dim FormatGoldLine As Func(Of String, Integer, Boolean, String) =
                Function(label As String, value As Integer, isExpense As Boolean) As String
                    Dim absStr As String = Math.Abs(value).ToString("N0").PadLeft(10)
                    Dim sign As String = If(isExpense, "-", " ")
                    Return $"{label,-28}: {sign}{absStr}"
                End Function

                ' --- Ledger lines (using race-specific investment name) ---
                Dim goldLines As String() = {
                FormatGoldLine("Collected from Population", p.GoldCollectedThisTurn, False),
                FormatGoldLine($"{investmentName} Income", p.InvestmentIncomeThisTurn, False),
                FormatGoldLine("Summoners Purchased", p.SummonersBoughtCostThisTurn, True),
                FormatGoldLine("Mercenaries Hired", p.MercenariesHiredCostThisTurn, True),
                FormatGoldLine("Mercenary Army Wages Paid", p.WagesPaidThisTurn, True),
                New String("-"c, 55),
                FormatGoldLine("Net Change This Turn",
                    (p.GoldCollectedThisTurn + p.InvestmentIncomeThisTurn -
                     p.WagesPaidThisTurn - p.SummonersBoughtCostThisTurn -
                     p.MercenariesHiredCostThisTurn), False),
                FormatGoldLine("Gold Total After Turn", p.Gold, False)
            }

                For Each line In goldLines
                    g.DrawString(line, monoFont, Brushes.Black, marginLeft + 60, y)
                    y += 20
                Next
            End Using
        End Using
    End Sub


    Private Function DrawBattleReportsPage(g As Graphics, e As PrintPageEventArgs, p As Player) As Boolean
        Dim marginLeft As Single = e.MarginBounds.Left
        Dim marginTop As Single = e.MarginBounds.Top
        Dim pageWidth As Single = e.MarginBounds.Width
        Dim pageRight As Single = e.MarginBounds.Right
        Dim pageBottom As Single = e.MarginBounds.Bottom
        Dim y As Single = marginTop

        Using titleFont As New Font("Georgia", 14, FontStyle.Bold),
          sectionFont As New Font("Consolas", 9, FontStyle.Bold),
          bodyFont As New Font("Consolas", 8, FontStyle.Regular)

            Dim blue As Brush = Brushes.RoyalBlue
            Dim red As Brush = Brushes.Firebrick
            Dim black As Brush = Brushes.Black
            Dim gray As Pen = New Pen(Color.LightGray, 1.0F)

            ' --- Race colours (Human darker/olive tone) ---
            Dim raceBrush As New Dictionary(Of String, Brush)(StringComparer.OrdinalIgnoreCase) From {
            {"Elf", Brushes.ForestGreen},
            {"Dwarf", Brushes.RoyalBlue},
            {"Orc", Brushes.Firebrick},
            {"Human", New SolidBrush(Color.FromArgb(130, 130, 30))}
        }

            Dim fmt As New StringFormat(StringFormatFlags.LineLimit) With {.Trimming = StringTrimming.None}

            ' --- Header per battle page ---
            ' Draw only if this is the first page of a new battle
            If battleLineIndex = 0 Then
                g.DrawString("Battle Report", titleFont, blue, marginLeft, y)
                y += titleFont.GetHeight(g) + 6
            End If

            ' --- Safety: no reports ---
            If CurrentForm.CurrentReports Is Nothing _
           OrElse CurrentForm.CurrentReports.BattleReports Is Nothing _
           OrElse CurrentForm.CurrentReports.BattleReports.Count = 0 Then
                g.DrawString("No battles occurred this turn.", bodyFont, black, marginLeft, y)
                ResetBattlePrintState()
                e.HasMorePages = False
                Return False
            End If

            Dim battles = CurrentForm.CurrentReports.BattleReports
            Dim i As Integer = battleReportIndex
            If i >= battles.Count Then
                ResetBattlePrintState()
                e.HasMorePages = False
                Return False
            End If

            Dim battleText As String = battles(i)

            ' --- Detect involved races (ensures full report) ---
            Dim involved As New List(Of String)
            If battleText.IndexOf("Elf", StringComparison.OrdinalIgnoreCase) >= 0 Then involved.Add("Elf")
            If battleText.IndexOf("Dwarf", StringComparison.OrdinalIgnoreCase) >= 0 Then involved.Add("Dwarf")
            If battleText.IndexOf("Human", StringComparison.OrdinalIgnoreCase) >= 0 Then involved.Add("Human")
            If battleText.IndexOf("Orc", StringComparison.OrdinalIgnoreCase) >= 0 Then involved.Add("Orc")

            Dim reportText As String = GetBattleReportForPlayer(p, battleText, involved)
            Dim lines As String() = reportText.Replace(vbCrLf, vbLf).Split(New String() {vbLf}, StringSplitOptions.None)

            Dim lineIdx As Integer = battleLineIndex
            Dim drewSomething As Boolean = False

            Dim collectingCasualties As Boolean = False
            Dim casualtyLabel As String = ""
            Dim casualtyBuffer As New List(Of String)

            While lineIdx < lines.Length
                Dim raw As String = lines(lineIdx)
                Dim line As String = If(raw, "")
                Dim trimmed As String = line.Trim()

                Dim useFont As Font = bodyFont
                Dim useBrush As Brush = black
                Dim extraSpacing As Single = 0
                Dim isSeparator As Boolean = False

                If trimmed.Length >= 5 AndAlso
               (trimmed.All(Function(c) c = "-"c) OrElse trimmed.All(Function(c) c = "="c)) Then
                    isSeparator = True
                End If

                ' --- Section colouring ---
                If Not isSeparator AndAlso trimmed.StartsWith("Battle at (", StringComparison.OrdinalIgnoreCase) Then
                    useFont = sectionFont : useBrush = blue : extraSpacing = 4
                ElseIf Not isSeparator Then
                    Dim t As String = trimmed
                    If {"Ranged Phase", "Charge Phase", "Melee Phase", "Chase Phase"}.Contains(t, StringComparer.OrdinalIgnoreCase) Then
                        useFont = sectionFont : useBrush = blue : extraSpacing = 2
                    ElseIf t.StartsWith("Start of Battle", StringComparison.OrdinalIgnoreCase) OrElse
                       t.StartsWith("End of Battle", StringComparison.OrdinalIgnoreCase) Then
                        useFont = sectionFont : useBrush = red : extraSpacing = 4
                    End If
                End If
                If Not isSeparator Then
                    Dim mArmy = System.Text.RegularExpressions.Regex.Match(trimmed, "^(Elf|Dwarf|Orc|Human)\s+Army\b",
                    System.Text.RegularExpressions.RegexOptions.IgnoreCase)
                    If mArmy.Success Then
                        Dim race As String = mArmy.Groups(1).Value
                        If raceBrush.ContainsKey(race) Then
                            useFont = sectionFont : useBrush = raceBrush(race)
                        End If
                    End If
                End If
                If Not isSeparator Then
                    Dim mVic = System.Text.RegularExpressions.Regex.Match(trimmed, "^(Elf|Dwarf|Orc|Human)\s+Victory\b",
                    System.Text.RegularExpressions.RegexOptions.IgnoreCase)
                    If mVic.Success Then
                        Dim race As String = mVic.Groups(1).Value
                        If raceBrush.ContainsKey(race) Then
                            useFont = sectionFont : useBrush = raceBrush(race)
                            extraSpacing = 4
                        End If
                    End If
                End If

                ' --- Casualty handling ---
                If trimmed.StartsWith("Losses:", StringComparison.OrdinalIgnoreCase) Then
                    Dim neededL As SizeF = g.MeasureString(line, bodyFont, CInt(pageWidth), fmt)
                    If y + neededL.Height > pageBottom - 5 AndAlso drewSomething Then
                        battleLineIndex = lineIdx
                        battleReportIndex = i
                        e.HasMorePages = True
                        Return True
                    End If
                    g.DrawString(line, bodyFont, black, New RectangleF(marginLeft, y, pageWidth, neededL.Height), fmt)
                    y += neededL.Height + 2
                    drewSomething = True
                    lineIdx += 1
                    Continue While
                End If

                If trimmed.StartsWith("Main Casualties:", StringComparison.OrdinalIgnoreCase) Then
                    collectingCasualties = True
                    casualtyLabel = "Main Casualties:"
                    casualtyBuffer.Clear()
                    lineIdx += 1
                    Continue While
                End If

                If collectingCasualties AndAlso trimmed.StartsWith("- ") Then
                    casualtyBuffer.Add(trimmed.Substring(2))
                    lineIdx += 1
                    Continue While
                End If

                If collectingCasualties AndAlso (Not trimmed.StartsWith("- ") OrElse lineIdx = lines.Length - 1) Then
                    collectingCasualties = False
                    If casualtyBuffer.Count > 0 Then
                        Dim joined As String = $"{casualtyLabel} {String.Join("; ", casualtyBuffer)}"
                        Dim neededC As SizeF = g.MeasureString(joined, bodyFont, CInt(pageWidth), fmt)
                        If y + neededC.Height > pageBottom - 5 AndAlso drewSomething Then
                            battleLineIndex = lineIdx
                            battleReportIndex = i
                            e.HasMorePages = True
                            Return True
                        End If
                        g.DrawString(joined, bodyFont, black, New RectangleF(marginLeft + 10, y, pageWidth - 10, neededC.Height), fmt)
                        y += neededC.Height + 2
                        casualtyBuffer.Clear()
                    End If
                End If

                ' --- Regular drawing ---
                Dim needed As SizeF = If(isSeparator,
                New SizeF(pageWidth, bodyFont.GetHeight(g)),
                g.MeasureString(line, useFont, CInt(pageWidth), fmt))

                If y + needed.Height > pageBottom - 5 AndAlso drewSomething Then
                    battleLineIndex = lineIdx
                    battleReportIndex = i
                    e.HasMorePages = True
                    Return True
                End If

                If isSeparator Then
                    Dim midY As Single = y + bodyFont.GetHeight(g) * 0.5F
                    g.DrawLine(gray, marginLeft, midY, pageRight, midY)
                    y += bodyFont.GetHeight(g)
                Else
                    g.DrawString(line, useFont, useBrush, New RectangleF(marginLeft, y, pageWidth, needed.Height), fmt)
                    y += needed.Height + extraSpacing
                End If

                drewSomething = True
                lineIdx += 1
            End While

            If collectingCasualties AndAlso casualtyBuffer.Count > 0 Then
                Dim joined As String = $"{casualtyLabel} {String.Join("; ", casualtyBuffer)}"
                Dim neededC As SizeF = g.MeasureString(joined, bodyFont, CInt(pageWidth), fmt)
                g.DrawString(joined, bodyFont, black, New RectangleF(marginLeft + 10, y, pageWidth - 10, neededC.Height), fmt)
                y += neededC.Height + 2
                casualtyBuffer.Clear()
            End If

            battleLineIndex = 0
            battleReportIndex = i + 1
            e.HasMorePages = (battleReportIndex < battles.Count)
            If Not e.HasMorePages Then ResetBattlePrintState()
            Return e.HasMorePages
        End Using
    End Function

    Private Sub ResetBattlePrintState()
        battleReportIndex = 0
        battleLineIndex = 0
        lastPrintedBattleIndex = -1
        hasDrawnBattleHeader = False
    End Sub



    Private Function MeasureBattleHeight(g As Graphics, battleText As String, fmt As StringFormat, pageWidth As Single, bodyFont As Font, sectionFont As Font) As Single
        Dim totalHeight As Single = 0
        Dim lines = battleText.Replace(vbCrLf, vbLf).Split({vbLf}, StringSplitOptions.None)

        For Each line In lines
            Dim trimmed = line.Trim()
            Dim useFont As Font = bodyFont
            Dim extraSpacing As Single = 0

            If trimmed.StartsWith("Battle at (", StringComparison.OrdinalIgnoreCase) Then
                useFont = sectionFont : extraSpacing = 4
            ElseIf {"Ranged Phase", "Charge Phase", "Melee Phase", "Chase Phase"}.Contains(trimmed) Then
                useFont = sectionFont : extraSpacing = 2
            ElseIf trimmed.StartsWith("Start of Battle", StringComparison.OrdinalIgnoreCase) _
            OrElse trimmed.Equals("Final Army Status", StringComparison.OrdinalIgnoreCase) _
            OrElse trimmed.Equals("End of Battle", StringComparison.OrdinalIgnoreCase) Then
                useFont = sectionFont : extraSpacing = 4
            End If

            If trimmed.Length >= 5 AndAlso (trimmed.All(Function(c) c = "-"c) OrElse trimmed.All(Function(c) c = "="c)) Then
                totalHeight += bodyFont.GetHeight(g) + extraSpacing
            Else
                Dim needed = g.MeasureString(line, useFont, CInt(pageWidth), fmt)
                totalHeight += needed.Height + extraSpacing
            End If
        Next

        Return totalHeight + 10 ' a small gap after battle
    End Function


    ' ============================================================
    '  Empire Report for HTML
    ' ============================================================

    Private Sub AppendEmpireReportHTML(writer As StreamWriter, p As Player)
        writer.WriteLine("<div class='section'><h2>Empire Report</h2>")
        writer.WriteLine($"<p><strong>Race:</strong> {p.Race}</p>")

        ' --- Terrain counts (dynamic map size) ---
        Dim mapSizeX As Integer = CurrentForm.Map.GetLength(0)
        Dim mapSizeY As Integer = CurrentForm.Map.GetLength(1)

        Dim counts As New Dictionary(Of Integer, Integer) From {{0, 0}, {1, 0}, {2, 0}, {3, 0}}

        For x = 0 To mapSizeX - 1
            For y = 0 To mapSizeY - 1
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
        writer.WriteLine("</table>")

        ' === GOLD LEDGER ===
        writer.WriteLine("<h3>Gold Ledger</h3>")
        writer.WriteLine("<table>")
        writer.WriteLine("<tr><th>Source / Expense</th><th>Amount</th></tr>")
        writer.WriteLine($"<tr><td>Collected from Population</td><td>{p.GoldCollectedThisTurn:N0}</td></tr>")
        writer.WriteLine($"<tr><td>Investment Income</td><td>{p.InvestmentIncomeThisTurn:N0}</td></tr>")
        writer.WriteLine($"<tr><td>Summoners Purchased</td><td>-{p.SummonersBoughtCostThisTurn:N0}</td></tr>")
        writer.WriteLine($"<tr><td>Mercenaries Hired</td><td>-{p.MercenariesHiredCostThisTurn:N0}</td></tr>")
        writer.WriteLine($"<tr><td>Mercenary Army Wages Paid</td><td>-{p.WagesPaidThisTurn:N0}</td></tr>")
        writer.WriteLine("<tr><td colspan='2'><hr></td></tr>")
        Dim netChange As Integer = p.GoldCollectedThisTurn + p.InvestmentIncomeThisTurn - p.WagesPaidThisTurn - p.SummonersBoughtCostThisTurn - p.MercenariesHiredCostThisTurn
        writer.WriteLine($"<tr><td><strong>Net Change This Turn</strong></td><td><strong>{netChange:N0}</strong></td></tr>")
        writer.WriteLine($"<tr><td><strong>Gold Total After Turn</strong></td><td><strong>{p.Gold:N0}</strong></td></tr>")
        writer.WriteLine("</table>")

        writer.WriteLine("</div>")
    End Sub


    Private Function DrawArmiesPage(g As Graphics, e As PrintPageEventArgs, p As Player) As Boolean
        Dim marginLeft As Single = e.MarginBounds.Left
        Dim marginTop As Single = e.MarginBounds.Top
        Dim y As Single = marginTop

        Using titleFont As New Font("Georgia", 14, FontStyle.Bold),
          armyFont As New Font("Georgia", 10, FontStyle.Bold),
          textFont As New Font("Consolas", 9, FontStyle.Regular),
          heroFont As New Font("Consolas", 9, FontStyle.Bold)   ' bold for heroes

            ' === PAGE TITLE ===
            g.DrawString("Armies", titleFont, Brushes.Black, marginLeft, y)
            y += 35

            If p.Armies Is Nothing OrElse p.Armies.Count = 0 Then
                g.DrawString("No armies currently fielded.", textFont, Brushes.Black, marginLeft, y)
                Return False
            End If

            ' === Race colours ===
            Dim raceBrush As Brush = Brushes.Black
            Select Case p.Race.ToLower()
                Case "elf" : raceBrush = New SolidBrush(Color.ForestGreen)
                Case "dwarf" : raceBrush = New SolidBrush(Color.SteelBlue)
                Case "orc" : raceBrush = New SolidBrush(Color.DarkOliveGreen)
                Case "human" : raceBrush = New SolidBrush(Color.OrangeRed)
            End Select

            ' === Hero brush (brighter purple) ===
            Dim heroBrush As New SolidBrush(Color.HotPink)

            ' === Print each army ===
            For Each a In p.Armies
                If a Is Nothing Then Continue For

                ' --- Training level as whole percent ---
                Dim trainingPercent As Integer = CInt(Math.Round(a.TrainingLevel * 100))

                ' --- Header ---
                Dim armyHeader As String = $"{p.Race} Army at ({a.X},{a.Y}) : {a.Name} - Training Level {trainingPercent}%"
                g.DrawString(armyHeader, armyFont, raceBrush, marginLeft, y)
                y += 20

                ' --- Separate heroes from normal units ---
                Dim normalUnits = a.Units.Where(Function(u) Not u.IsHero).ToList()
                Dim heroes = a.Units.Where(Function(u) u.IsHero).ToList()

                ' --- Normal units ---
                For Each u In normalUnits
                    Dim countStr As String = u.Size.ToString("N0") ' thousand separators
                    g.DrawString($"  {u.Name} ({countStr})", textFont, Brushes.Black, marginLeft + 10, y)
                    y += 15
                Next

                ' --- Heroes (bright purple + bold) ---
                For Each h In heroes
                    Dim heroLine As String = $"  {h.Name}, Level {h.Level}"
                    g.DrawString(heroLine, heroFont, heroBrush, marginLeft + 10, y)
                    y += 15
                Next

                ' --- Total ---
                Dim totalStr As String = a.TotalSoldiers.ToString("N0")
                g.DrawString($"  Total: {totalStr}", textFont, Brushes.Black, marginLeft + 10, y)
                y += 25

                ' --- Page overflow check ---
                If y > e.MarginBounds.Bottom - 50 Then
                    e.HasMorePages = True
                    Return True
                End If
            Next
        End Using

        Return False
    End Function



End Module
