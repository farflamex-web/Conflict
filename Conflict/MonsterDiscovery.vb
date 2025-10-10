
Imports System.IO
Imports Conflict.Form1
Imports Newtonsoft.Json

Module MonsterDiscovery

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
                Debug.WriteLine($"[MONSTERDISCOVERY] Failed to read seen list for {customerNickname}: {ex.Message}")
            End Try
        End If

        ' If this is new, record it and trigger description
        If Not seenList.Contains(monsterName) Then
            seenList.Add(monsterName)
            Try
                Dim newJson As String = JsonConvert.SerializeObject(seenList.ToList(), Formatting.Indented)
                File.WriteAllText(filePath, newJson)
            Catch ex As Exception
                Debug.WriteLine($"[MONSTERDISCOVERY] Failed to save seen list for {customerNickname}: {ex.Message}")
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
            Debug.WriteLine($"[MONSTERDISCOVERY] Error reading seen list: {ex.Message}")
            Return False
        End Try
    End Function

    ' ==========================================================
    '  Placeholder for the descriptive text / print routine
    ' ==========================================================
    Private Sub ShowMonsterIntro(monsterName As String)
        ' You can replace this later with your print / description logic.
        Debug.WriteLine($"[NEW MONSTER DISCOVERY] {monsterName} encountered for the first time!")
    End Sub

    ' ==========================================================
    '  Scan all armies at end of turn and update each customer's
    '  "seen monsters" list based on units currently in play
    ' ==========================================================
    Public Sub UpdateSeenMonstersForAllPlayers(players As List(Of Player))
        If players Is Nothing OrElse players.Count = 0 Then Exit Sub
        If Not Directory.Exists(SeenFolder) Then Directory.CreateDirectory(SeenFolder)

        For Each p In players
            If p Is Nothing OrElse String.IsNullOrWhiteSpace(p.Nickname) Then Continue For
            If p.AIControlled Then Continue For  ' Skip AIs if you don’t want to track them

            Dim nickname As String = p.Nickname
            Dim allNames As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

            ' === Loop through all units in all armies ===
            If p.Armies IsNot Nothing Then
                For Each a In p.Armies
                    If a Is Nothing OrElse a.Units Is Nothing Then Continue For
                    For Each u In a.Units
                        If u Is Nothing OrElse String.IsNullOrWhiteSpace(u.Name) Then Continue For
                        allNames.Add(u.Name.Trim())
                    Next
                Next
            End If

            ' === Now mark each discovered monster ===
            For Each monsterName In allNames
                MarkMonsterSeen(nickname, monsterName)
            Next
        Next
    End Sub


End Module