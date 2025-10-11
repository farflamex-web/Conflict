
Imports System.IO
Imports Conflict.Form1
Imports Newtonsoft.Json

Module MonsterDiscovery

    Private MonsterDescriptions As Dictionary(Of String, String)
    ' Folder that stores all "seen monster" JSON files
    Private ReadOnly SeenFolder As String =
    Path.Combine(Application.StartupPath, "CustomersSeen")

    ' ==========================================================
    '  Add a monster to the customer's seen list
    ' ==========================================================
    Public Sub MarkMonsterSeen(customerNickname As String, monsterName As String)
        If String.IsNullOrWhiteSpace(customerNickname) OrElse String.IsNullOrWhiteSpace(monsterName) Then Exit Sub
        If Not Directory.Exists(SeenFolder) Then Directory.CreateDirectory(SeenFolder)

        Dim filePath As String = Path.Combine(SeenFolder, $"{customerNickname}_seen.json")

        ' Load existing list
        Dim seenList As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        If File.Exists(filePath) Then
            Try
                Dim json As String = File.ReadAllText(filePath)
                Dim existing = JsonConvert.DeserializeObject(Of List(Of String))(json)
                If existing IsNot Nothing Then
                    For Each s In existing
                        seenList.Add(s)
                    Next
                End If
            Catch ex As Exception
                'Debug.WriteLine($"[MONSTERDISCOVERY] Failed to read seen list for {customerNickname}: {ex.Message}")
            End Try
        End If

        ' If this is new, record it and trigger description
        If Not seenList.Contains(monsterName) Then
            seenList.Add(monsterName)
            Try
                Dim newJson As String = JsonConvert.SerializeObject(seenList.ToList(), Formatting.Indented)
                File.WriteAllText(filePath, newJson)
            Catch ex As Exception
                'Debug.WriteLine($"[MONSTERDISCOVERY] Failed to save seen list for {customerNickname}: {ex.Message}")
            End Try

            ' Display or print your introduction text here
            ShowMonsterIntro(monsterName)
        End If
    End Sub

    ' ==========================================================
    '  Check if customer has already seen this monster
    ' ==========================================================
    Public Function HasSeenMonster(customerNickname As String, monsterName As String) As Boolean
        If String.IsNullOrWhiteSpace(customerNickname) OrElse String.IsNullOrWhiteSpace(monsterName) Then Return False

        Dim filePath As String = Path.Combine(SeenFolder, $"{customerNickname}_seen.json")
        If Not File.Exists(filePath) Then Return False

        Try
            Dim json As String = File.ReadAllText(filePath)
            Dim list = JsonConvert.DeserializeObject(Of List(Of String))(json)
            If list Is Nothing Then Return False
            Return list.Any(Function(s) s.Equals(monsterName, StringComparison.OrdinalIgnoreCase))
        Catch ex As Exception
            'Debug.WriteLine($"[MONSTERDISCOVERY] Error reading seen list: {ex.Message}")
            Return False
        End Try
    End Function

    ' ==========================================================
    '  Placeholder for the descriptive text / print routine
    ' ==========================================================
    Private Sub ShowMonsterIntro(monsterName As String)
        ' You can replace this later with your print / description logic.
        'Debug.WriteLine($"[NEW MONSTER DISCOVERY] {monsterName} encountered for the first time!")
    End Sub

    ' ==========================================================
    '  UpdateSeenMonstersForAllPlayers
    '  Every human player automatically learns all unit types
    '  present in any army currently in the game.
    ' ==========================================================

    Public Sub UpdateSeenMonstersForAllPlayers(players As List(Of Player))
        If players Is Nothing OrElse players.Count = 0 Then Exit Sub
        If Not Directory.Exists(SeenFolder) Then Directory.CreateDirectory(SeenFolder)

        ' ===========================================================================
        '  PURPOSE:
        '    This routine builds a global list of all units currently visible in the game
        '    and writes "seen monster" records for every human player.
        '
        '  CONTEXT:
        '    • Normally called during SaveGame() or before printing.
        '    • On Turn 0, mercenary offers exist but are not yet owned by any player,
        '      so they must be included manually (they won’t appear in player armies).
        ' ===========================================================================

        ' --- Build a global list of all unit names in play ---
        Dim globalUnits As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        ' --- STEP 1: Collect units from all player armies ---
        For Each p In players
            If p Is Nothing OrElse p.Armies Is Nothing Then Continue For
            For Each a In p.Armies
                If a Is Nothing OrElse a.Units Is Nothing Then Continue For
                For Each u In a.Units
                    If u Is Nothing OrElse String.IsNullOrWhiteSpace(u.Name) Then Continue For
                    Dim cleanName As String = NormaliseUnitName(u.Name)
                    globalUnits.Add(cleanName)
                Next
            Next
        Next

        ' --- STEP 2: Include any active mercenary offer (important for Turn 0) ---
        ' This ensures that even unowned merc armies are visible to all players
        ' right from the first printed turn. Without this, their units would not
        ' appear in anyone's Seen file until they were purchased.
        Try
            If CurrentMercOffer IsNot Nothing AndAlso CurrentMercOffer.Units IsNot Nothing Then
                For Each s In CurrentMercOffer.Units
                    If s Is Nothing Then Continue For

                    If s.Hero IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(s.Hero.Name) Then
                        globalUnits.Add(NormaliseUnitName(s.Hero.Name))
                    ElseIf s.Template IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(s.Template.Name) Then
                        globalUnits.Add(NormaliseUnitName(s.Template.Name))
                    End If
                Next
                'Debug.WriteLine("[MONSTERDISCOVERY] Added current merc offer units to discovery list.")
            End If
        Catch ex As Exception
            'Debug.WriteLine($"[MONSTERDISCOVERY] Failed to read merc offer: {ex.Message}")
        End Try

        ' --- STEP 3: Special case – Xorns (exist outside normal army structures) ---
        Try
            Dim xornVisible As Boolean = False

            For Each p In players
                If p Is Nothing Then Continue For

                ' Safely check if player has a property named "Xorn"
                Dim prop = p.GetType().GetProperty("Xorn")
                If prop IsNot Nothing Then
                    Dim value As Object = prop.GetValue(p)
                    Dim count As Integer = 0
                    If value IsNot Nothing AndAlso Integer.TryParse(value.ToString(), count) AndAlso count > 0 Then
                        xornVisible = True
                        Exit For
                    End If
                End If
            Next

            If xornVisible Then
                'Debug.WriteLine("[MONSTERDISCOVERY] Xorns detected in play — marking as seen for all players.")
                globalUnits.Add("Xorn")
            End If
        Catch ex As Exception
            'Debug.WriteLine($"[MONSTERDISCOVERY] Xorn check failed: {ex.Message}")
        End Try

        ' --- STEP 4: Mark all collected units as "seen" for every human customer ---
        For Each p In players
            If p Is Nothing OrElse String.IsNullOrWhiteSpace(p.Nickname) Then Continue For
            If p.AIControlled Then Continue For  ' Skip AI players

            For Each monsterName In globalUnits
                MarkMonsterSeen(p.Nickname, monsterName)
            Next
        Next

        'Debug.WriteLine($"[MONSTERDISCOVERY] Seen-monster list updated for {players.Count} players, total unique units: {globalUnits.Count}")
    End Sub



    Public Sub LoadMonsterDescriptions()
        Dim dataFolder As String = Path.Combine(Application.StartupPath, "Data")
        Dim filePath As String = Path.Combine(dataFolder, "MonsterDescriptions.json")

        ' Ensure folder exists
        If Not Directory.Exists(dataFolder) Then Directory.CreateDirectory(dataFolder)

        ' If file is missing, create one with starter content
        If Not File.Exists(filePath) Then
            'Debug.WriteLine("[MONSTERDISCOVERY] MonsterDescriptions.json not found, creating new template file...")

            Dim starter As New Dictionary(Of String, String) From {
            {"Elven Archers", "Elves are renowned for their unmatched archery skills, and their bowmen form the heart of every elven host. " &
                "Calm and unerring, elven archers are twice as strong as those of other races and are said to end battles before they truly begin. "},
            {"Fire Giants", "Immense warriors wreathed in fire and smoke, wielding molten blades."},
            {"Forest Knight", "Silent riders sworn to the deep woods, their steeds cloaked in moss and shadow."},
            {"Orc Berserkers", "Raging beasts of muscle and fury, painted in blood and ash."}
        }



            Try
                Dim json As String = JsonConvert.SerializeObject(starter, Formatting.Indented)
                File.WriteAllText(filePath, json)
                MonsterDescriptions = starter
                'Debug.WriteLine("[MONSTERDISCOVERY] Created MonsterDescriptions.json with starter entries.")
                Exit Sub
            Catch ex As Exception
                'Debug.WriteLine($"[MONSTERDISCOVERY] Failed to auto-create descriptions file: {ex.Message}")
            End Try
        End If

        ' Otherwise, load existing file
        Try
            Dim json As String = File.ReadAllText(filePath)
            MonsterDescriptions = JsonConvert.DeserializeObject(Of Dictionary(Of String, String))(json)
            If MonsterDescriptions Is Nothing Then
                MonsterDescriptions = New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
            End If
            'Debug.WriteLine($"[MONSTERDISCOVERY] Loaded {MonsterDescriptions.Count} monster descriptions.")
        Catch ex As Exception
            'Debug.WriteLine($"[MONSTERDISCOVERY] Error loading MonsterDescriptions.json: {ex.Message}")
            MonsterDescriptions = New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
        End Try
    End Sub

    ' ==========================================================
    '  Summoner name helpers (populated from Form1)
    ' ==========================================================
    Private SummonerBaseNames As HashSet(Of String)

    ' Called once during startup to copy all summoner names
    Public Sub InitialiseSummonerNames(definitions As List(Of SummonerInfo))
        SummonerBaseNames = New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        If definitions Is Nothing Then Exit Sub
        For Each def In definitions
            If def Is Nothing OrElse String.IsNullOrWhiteSpace(def.Name) Then Continue For
            SummonerBaseNames.Add(def.Name.Trim())
        Next

        'Debug.WriteLine($"[MONSTERDISCOVERY] Loaded {SummonerBaseNames.Count} summoner base names.")
    End Sub

    Private Function NormaliseUnitName(originalName As String) As String
        If String.IsNullOrWhiteSpace(originalName) Then Return originalName

        ' --- Check if this unit name contains any summoner base name ---
        If SummonerBaseNames IsNot Nothing Then
            For Each baseName In SummonerBaseNames
                If originalName.IndexOf(baseName, StringComparison.OrdinalIgnoreCase) >= 0 Then
                    Return baseName
                End If
            Next
        End If

        ' Otherwise return as-is
        Return originalName.Trim()
    End Function

    ' ==========================================================
    '  When a new mercenary offer appears, reveal its troops
    '  to all human players (so they get descriptions before bidding)
    ' ==========================================================
    Public Sub RevealMercenaryOffer(offer As MercenaryArmy, players As List(Of Player))
        If offer Is Nothing OrElse offer.Units Is Nothing OrElse offer.Units.Count = 0 Then Exit Sub
        If players Is Nothing OrElse players.Count = 0 Then Exit Sub

        Dim mercUnits As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

        ' --- Collect all unit names in the merc offer ---
        For Each s In offer.Units
            If s Is Nothing Then Continue For

            If s.Hero IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(s.Hero.Name) Then
                mercUnits.Add(NormaliseUnitName(s.Hero.Name))
            ElseIf s.Template IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(s.Template.Name) Then
                mercUnits.Add(NormaliseUnitName(s.Template.Name))
            End If
        Next

        ' --- Reveal to all human players ---
        For Each p In players
            If p Is Nothing OrElse String.IsNullOrWhiteSpace(p.Nickname) Then Continue For
            If p.AIControlled Then Continue For
            For Each unitName In mercUnits
                MarkMonsterSeen(p.Nickname, unitName)
            Next
        Next

        'Debug.WriteLine($"[MONSTERDISCOVERY] Revealed new mercenary offer ({offer.Faction}) with {mercUnits.Count} unit types.")
    End Sub


End Module