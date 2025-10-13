Option Strict On
Option Explicit On

' ===========================================================
' PROJECT: Conflict
' ===========================================================
' TYPE: Turn-based Play-By-Mail Strategy Game
' LANGUAGE: VB.NET
' ===========================================================
' OVERVIEW
' -----------------------------------------------------------
' Conflict is a turn-based strategy game designed for play-by-mail.
' It runs on a square 25x25 tile map with four players, each
' representing a different fantasy race (Elf, Dwarf, Orc, Human).
'
' Each tile has:
'   - Terrain type (Plains, Forest, Hills, Mountain)
'   - Ownership (which player controls it)
'
' The game handles map creation, army movement, resource collection,
' combat detection, and battle resolution. Players submit turns,
' and the game engine processes results and generates reports.
'
' ===========================================================
' MAP SYSTEM
' -----------------------------------------------------------
' - Map stored as a 3D array: map(x, y, layer)
'   Layer 0: Terrain type
'   Layer 1: Ownership
'
' - GenerateTerrain():
'   Creates randomized terrain, smooths clumps, assigns
'   starting blocks for each player.
'
' - FillStartingBlock():
'   Each player starts with a fixed 5x5 block containing:
'       * 19 tiles of their favored terrain
'       * 2 tiles of each other terrain
'
' - GetDominantNeighbour():
'   Used to smooth terrain by checking 3x3 neighbourhoods.
'
' ===========================================================
' PLAYER FACTIONS
' -----------------------------------------------------------
' Race-to-corner assignment:
'   - Elf    (Player 0, Top-Left,    Favored: Forest)
'   - Dwarf  (Player 1, Top-Right,   Favored: Mountain)
'   - Orc    (Player 2, Bottom-Left, Favored: Hills)
'   - Human  (Player 3, Bottom-Right,Favored: Plains)
'
' ===========================================================
' UNITS
' -----------------------------------------------------------
' Each race has unique units with stats and costs.
'
' Elves:
'   - Archer
'   - Forest Sentinel
'   - Ranger
'   - Forest Knight
'   - Forest Archer
'
' Dwarves:
'   - Ironbolters
'   - Ironshield
'   - Ironfoot
'   - Ironhorn Riders
'   - Ironhorn Maulers
'
' Orcs:
'   - Orc Shortbows
'   - Orc Grunts
'   - Orc Berserkers
'   - Fangriders
'   - Bloodwolf Riders
'
' Humans:
'   - Archer
'   - Spearman
'   - Heavy Infantry
'   - Light Cavalry
'   - Heavy Cavalry
'
' ===========================================================
' GAME SYSTEMS
' -----------------------------------------------------------
' - Army movement between tiles
' - Tile capture rules
' - Resource collection (by terrain)
' - Population growth
' - Combat detection and resolution
'
' ===========================================================
' STATUS
' -----------------------------------------------------------
' - Current code builds in VB.NET (Visual Studio)
' - Core map generation and ownership system working
' - Units roster and production rules defined
' - Battle report system under development
'
' ===========================================================
' AI/HELP NOTES (for ChatGPT assistance)
' -----------------------------------------------------------
' - Needs refactoring of combat resolution for clarity
' - Possible automation of turn processing
' - Consider additional map features (rivers, cities)
' - Consider cleaner separation between UI (Form1) and
'   game logic (modules/classes)
' ===========================================================
'test

Imports System.ComponentModel
Imports System.Drawing.Printing
Imports System.IO
Imports System.Linq
Imports System.Reflection
Imports Microsoft.VisualBasic.FileIO
Imports Newtonsoft.Json

Public Class Form1

    ' Represents everything stored in a save file
    Public Class GameState
        Public Property TurnNumber As Integer
        Public Property Players As List(Of Player)
        Public Property Map As Integer(,,)
        Public Property MercPriceLevel As Integer
        Public Property CurrentMercOffer As MercenaryArmy
    End Class



#Region "=== Constants and Enums ==="

    Private Const Rows As Integer = 25
    Private Const Cols As Integer = 25
    Private Const LegendSpriteSize As Single = 32
    Private Const LegendLabelSpacing As Single = 10
    Private Const GridFontSize As Single = 6

    Public Map(Rows - 1, Cols - 1, 2) As Integer

    Private Races() As String = {"Elf", "Dwarf", "Orc", "Human"}

    Private CurrentState As GameState
    Private CurrentReports As TurnReports
    Private IsPopulatingTurns As Boolean = False
    ' Guards event handlers while we programmatically change selections.
    Private isRestoringCombos As Boolean = False

    ' === Name tracking for army generation ===
    Private UsedArmyNames As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)

    Public Enum UnitType
        Archer = 0
        LightInfantry = 1
        HeavyInfantry = 2
        LightCavalry = 3
        HeavyCavalry = 4
    End Enum


#End Region

#Region "=== Nested Classes ==="

    Public Class Army
        Public Property X As Integer
        Public Property Y As Integer
        Public Property Name As String
        Public Property Units As New List(Of Unit)
        Public ReadOnly Property TotalSoldiers As Integer
            Get
                Return Units.Sum(Function(u) u.Size)
            End Get
        End Property
        Public Property MoveQueue As New List(Of ArmyCommand)
        Public Property HasUsedSpecial As Boolean = False
        Public Property RetreatedThisTurn As Boolean = False
        Public Property Race As String
        Public Property TrainingLevel As Double
        Public Sub New()
            ' required for JSON deserialization
        End Sub

    End Class

    Public Class ArmyCommand
        Public Property Command As String       ' e.g. "N", "S", "RECRUIT"
        Public Property Parameter As String     ' e.g. "Ironfoot"
        Public Property Amount As String        ' e.g. "MAX" or "250"  (new)
    End Class

    Public Class ArmyListItem
        Public Property Display As String
        Public Property Index As Integer
        Public Overrides Function ToString() As String
            Return Display
        End Function
    End Class


    Public Class Unit
        ' === Properties ===
        Public Property Name As String
        Public Property ShortName As String
        Public Property HP As Integer
        Public Property Melee As Integer
        Public Property Ranged As Integer
        Public Property Power As Integer
        Public Property DefencePoints As Integer
        Public Property Type As UnitType
        Public Property FoodCost As Double
        Public Property CanCharge As Boolean
        Public Property CanChase As Boolean
        Public Property Flying As Boolean

        Public Property Race As String
        Public Property Size As Integer

        ' --- Hero / Summoner flags ---
        Public Property IsHero As Boolean = False
        Public Property HeroType As String ' e.g. "Summoner", "Champion", "Sorcerer"

        Public Property SummonerFaction As String
        Public Property Level As Integer = 0

        Public Property IsMercenary As Boolean = False

        Public Sub New()
            ' required for JSON deserialization
        End Sub


        ' === Constructors ===

        ' Normal unit from roster stats
        Public Sub New(stats As UnitStats, race As String, size As Integer)
            Me.Name = stats.Name
            Me.ShortName = stats.ShortName
            Me.HP = stats.HP
            Me.Melee = stats.Melee
            Me.Ranged = stats.Ranged
            Me.Power = stats.Power
            Me.DefencePoints = stats.DefencePoints
            Me.Type = stats.Type
            Me.FoodCost = stats.FoodCost
            Me.CanCharge = stats.CanCharge
            Me.CanChase = stats.CanChase
            Me.Flying = stats.Flying

            Me.Race = race
            Me.Size = size

            ' Normal roster units are never heroes
            Me.Level = 0
            Me.IsHero = False
            Me.HeroType = Nothing
            Me.SummonerFaction = Nothing
        End Sub

        ' Copy constructor (used in snapshots, cloning, etc.)
        Public Sub New(u As Unit)
            Me.Name = u.Name
            Me.ShortName = u.ShortName
            Me.HP = u.HP
            Me.Melee = u.Melee
            Me.Ranged = u.Ranged
            Me.Power = u.Power
            Me.DefencePoints = u.DefencePoints
            Me.Type = u.Type
            Me.FoodCost = u.FoodCost
            Me.CanCharge = u.CanCharge
            Me.CanChase = u.CanChase
            Me.Flying = u.Flying

            Me.Race = u.Race
            Me.Size = u.Size
            Me.IsHero = u.IsHero
            Me.HeroType = u.HeroType
            Me.SummonerFaction = u.SummonerFaction
            Me.Level = u.Level
        End Sub

        ' Generic hero constructor
        Public Sub New(heroType As String, displayName As String, ownerRace As String)
            Me.Name = displayName
            Me.HP = 10
            Me.Melee = 0
            Me.Ranged = 0
            Me.Power = 0
            Me.DefencePoints = 0
            Me.Size = 1
            Me.Type = UnitType.LightInfantry
            Me.Race = ownerRace
            Me.FoodCost = 1
            Me.IsHero = True
            Me.HeroType = heroType
            Me.Level = 1   ' <--- Always start at Level 1
        End Sub



        ' === Effective stat helpers ===

        Public Function GetEffectiveHP() As Integer
            If IsHero AndAlso Level > 0 Then
                Return 5 * Level
            Else
                Return HP
            End If
        End Function

        Public Function GetEffectiveDefencePoints() As Integer
            If IsHero AndAlso Level > 0 Then
                Return Math.Min(16, Level * 5)
            Else
                Return DefencePoints
            End If
        End Function

    End Class



    Public Class UnitStats
        Public Property Name As String
        Public Property Race As String
        Public Property Type As UnitType
        Public Property HP As Integer
        Public Property Melee As Integer
        Public Property Ranged As Integer
        Public Property Power As Integer ' For summoned creatures only
        Public Property DefencePoints As Integer ' For summoned creatures only
        Public Property Special As String
        Public Property Cost As String
        Public Property ShortName As String       ' <<< THIS IS REQUIRED
        Public Property FoodCost As Double = 1.0   ' default; will override for summons
        Public Property CanCharge As Boolean
        Public Property CanChase As Boolean
        Public Property Flying As Boolean
        Public Property IsSummoner As Boolean
        Public Property IsHero As Boolean
        Public Property SummonerFaction As String
        Public Property TrainingBonus As Double = 0.0    ' default 0 = untrained

    End Class


    Public Class TurnReports
        Public Property TurnNumber As Integer
        Public Property Notes As New List(Of String)

        ' Simple placeholders for now — we’ll replace or expand these later
        Public Property BattleReports As New List(Of String)
        Public Property Messages As New List(Of String)
        Public Property Summaries As New List(Of String)
    End Class


    ' === Central definition for all summoners ===
    Public Class SummonerInfo
        Public Property Name As String            ' e.g. "Kobold War Chief"
        Public Property AllowedRace As String     ' e.g. "Orc"
        Public Property RosterKey As String       ' e.g. "Kobold War Chief" or "Kobolds"
        Public Property Description As String     ' optional for UI
    End Class

    Public ReadOnly SummonerDefinitions As New List(Of SummonerInfo) From {
        New SummonerInfo With {.Name = "Druid", .AllowedRace = "Elf", .RosterKey = "Druid"},
        New SummonerInfo With {.Name = "Centaur Warchief", .AllowedRace = "Elf", .RosterKey = "Centaur Warchief"},
        New SummonerInfo With {.Name = "Archdruid", .AllowedRace = "Elf", .RosterKey = "Archdruid"},
        New SummonerInfo With {.Name = "Gnome Commander", .AllowedRace = "Dwarf", .RosterKey = "Gnome Commander"},
        New SummonerInfo With {.Name = "Runesmith", .AllowedRace = "Dwarf", .RosterKey = "Runesmith"},
        New SummonerInfo With {.Name = "Giant Overlord", .AllowedRace = "Dwarf", .RosterKey = "Giant Overlord"},
        New SummonerInfo With {.Name = "Kobold War Chief", .AllowedRace = "Orc", .RosterKey = "Kobold War Chief"},
        New SummonerInfo With {.Name = "Goblin Chieftain", .AllowedRace = "Orc", .RosterKey = "Goblin Chieftain"},
        New SummonerInfo With {.Name = "Gnoll Packlord", .AllowedRace = "Orc", .RosterKey = "Gnoll Packlord"},
        New SummonerInfo With {.Name = "Lizardman High Priest", .AllowedRace = "Orc", .RosterKey = "Lizardman High Priest"},
        New SummonerInfo With {.Name = "Ogre Warlord", .AllowedRace = "Orc", .RosterKey = "Ogre Warlord"},
        New SummonerInfo With {.Name = "Troll Tamer", .AllowedRace = "Orc", .RosterKey = "Troll Caller"},
        New SummonerInfo With {.Name = "Orc Dragonbinder", .AllowedRace = "Orc", .RosterKey = "Orc Dragonbinder"},
        New SummonerInfo With {.Name = "Cleric", .AllowedRace = "Human", .RosterKey = "Cleric"},
        New SummonerInfo With {.Name = "Beastmaster", .AllowedRace = "Human", .RosterKey = "Beastmaster"},
        New SummonerInfo With {.Name = "Dragonlord", .AllowedRace = "Human", .RosterKey = "Dragonlord"}
}

    Private ReadOnly Property SummonerRosters As Dictionary(Of String, List(Of UnitStats))
        Get
            Dim dict As New Dictionary(Of String, List(Of UnitStats))(StringComparer.OrdinalIgnoreCase)

            For Each def In SummonerDefinitions
                Dim roster = AllRaces.FirstOrDefault(Function(r) r.RaceName.Equals(def.RosterKey, StringComparison.OrdinalIgnoreCase))
                If roster IsNot Nothing AndAlso roster.Units IsNot Nothing AndAlso roster.Units.Count > 0 Then
                    dict(def.Name) = roster.Units
                End If
            Next

            Return dict
        End Get
    End Property

    ' === Summoner name parts per base race ===
    Private ReadOnly SummonerNameParts As New Dictionary(Of String, (String(), String())) From {
    {"Elf", (New String() {
        "Ela", "Syl", "Tha", "Ari", "Lora", "Vael", "Eri", "Cal", "Fina", "Ithe",
        "Luth", "Myri", "Nae", "Ola", "Phae", "Quel", "Rina", "Sae", "Tala", "Vanya"
    },
    New String() {
        "nor", "rian", "theas", "orien", "thir", "diel", "wen", "loth", "mir", "las",
        "hael", "dir", "thas", "ion", "viel", "wyn", "driel", "lith", "ros", "thal"
    })},
    {"Dwarf", (New String() {
        "Bor", "Dur", "Thra", "Grim", "Khar", "Bald", "Gim", "Thro", "Kaz", "Dain",
        "Brom", "Farn", "Rurik", "Orin", "Sten", "Varr", "Hald", "Mor", "Rag", "Thrun"
    },
    New String() {
        "in", "ar", "ek", "gar", "orn", "grim", "rak", "dun", "bar", "nor",
        "drum", "thar", "mod", "lok", "rin", "var", "dan", "grom", "tor", "lin"
    })},
    {"Orc", (New String() {
        "Mor", "Zhul", "Rok", "Gor", "Ur", "Thok", "Grash", "Krul", "Dro", "Shag",
        "Ug", "Vrok", "Harg", "Lok", "Brug", "Naz", "Orug", "Skarn", "Truk", "Zag"
    },
    New String() {
        "gar", "thak", "dok", "nak", "rash", "zug", "mok", "rak", "tash", "lok",
        "gash", "dur", "nak", "grom", "dor", "ruk", "vash", "gol", "mog", "dak"
    })},
    {"Human", (New String() {
        "Al", "Ced", "Ser", "Mar", "Rol", "Ed", "Wil", "Hen", "Rob", "Thom",
        "Geoff", "Rich", "Stev", "Paul", "Jon", "Andr", "Leon", "Phil", "Greg", "Dan"
    },
    New String() {
        "aric", "ric", "aphine", "ian", "fred", "son", "bert", "ton", "field", "well",
        "win", "ard", "den", "worth", "ham", "mond", "ard", "iel", "drick", "tine"
    })}
    }

    ' === Generate a new summoner name using the BUYER race for style ===
    Private Function GenerateSummonerName(buyerRace As String, summonerType As String) As String
        Dim rnd As New Random(Guid.NewGuid().GetHashCode())

        ' Use Orcish, Elvish, Dwarvish, or Human name style
        If Not SummonerNameParts.ContainsKey(buyerRace) Then
            Return $"Unnamed {summonerType}"
        End If

        Dim parts = SummonerNameParts(buyerRace)
        Dim prefix = parts.Item1(rnd.Next(parts.Item1.Length))
        Dim suffix = parts.Item2(rnd.Next(parts.Item2.Length))

        Return $"{prefix}{suffix} the {summonerType}"
    End Function

    Private Sub AIBuySummoners()
        For Each p In Players
            ' Skip null, human, or eliminated players
            If p Is Nothing OrElse Not p.AIControlled OrElse p.IsEliminated Then Continue For
            BuySummoner(p) ' BuySummoner will handle cost and random choice
        Next
    End Sub

    Private Sub BuySummoner(p As Player)
        If p Is Nothing OrElse p.IsEliminated Then Exit Sub

        ' === 1) Find all summoners this race can hire ===
        Dim allowed = SummonerDefinitions.
        Where(Function(s) s.AllowedRace.Equals(p.Race, StringComparison.OrdinalIgnoreCase)).
        ToList()

        If allowed.Count = 0 Then
            'Debug.WriteLine($"[SUMMONER] No summoners available for {p.Race}.")
            Exit Sub
        End If

        ' === 2) Pick a random summoner ===
        Dim chosen As SummonerInfo = allowed(rnd.Next(allowed.Count))
        Dim baseSummonerKey As String = chosen.Name
        Dim baseSummonerName As String = chosen.Name

        ' === 3) Calculate gold cost ===
        Dim ownedHeroes As Integer =
        p.Armies.SelectMany(Function(a) a.Units).
                 Count(Function(u) u IsNot Nothing AndAlso u.IsHero)

        Dim cost As Integer = CInt(1000 * Math.Pow(3, ownedHeroes))

        If p.Gold < cost Then
            'Debug.WriteLine($"[SUMMONER] {p.Race} (Player {p.PlayerNumber + 1}) cannot afford {baseSummonerName}. Needs {cost}, has {p.Gold}.")
            Exit Sub
        End If

        ' === 4) Pay ===
        p.Gold -= cost

        ' === 5) Create hero ===
        Dim fullName As String = GenerateSummonerName(p.Race, baseSummonerName)
        Dim summonerUnit As New Unit("Summoner", fullName, p.Race)
        summonerUnit.SummonerFaction = baseSummonerKey

        ' Safety if ctor doesn’t auto-fill
        'summonerUnit.IsHero = True
        'summonerUnit.HeroType = "Summoner"
        'summonerUnit.Level = 1

        ' === 6) Place into first army (or make new one) ===
        Dim targetArmy As Army = Nothing
        If p.Armies IsNot Nothing AndAlso p.Armies.Count > 0 Then
            targetArmy = p.Armies(0)
        Else
            targetArmy = New Army With {.Race = p.Race, .X = 0, .Y = 0}
            If p.Armies Is Nothing Then p.Armies = New List(Of Army)
            p.Armies.Add(targetArmy)
        End If

        targetArmy.Units.Add(summonerUnit)

        'Debug.WriteLine($"[SUMMONER] {p.Race} (Player {p.PlayerNumber + 1}) bought {fullName} (Level 1) for {cost} gold. Prior heroes: {ownedHeroes}.")
    End Sub

    ' ------------------------------------------------------------
    ' Manual (player) version of BuySummoner — uses selected values
    ' ------------------------------------------------------------
    Private Sub AttemptBuySummoner(p As Player, def As SummonerInfo, targetArmy As Army)
        'Debug.WriteLine($"[SUMMONER] AttemptBuySummoner started for {p.Race} -> {def.Name}")
        If p Is Nothing OrElse def Is Nothing OrElse targetArmy Is Nothing Then Exit Sub

        'Debug.WriteLine($"[SUMMONER] Gold={p.Gold}")
        ' === 1) Calculate cost based on existing heroes ===
        Dim ownedHeroes As Integer =
        p.Armies.SelectMany(Function(a) a.Units).
                 Count(Function(u) u IsNot Nothing AndAlso u.IsHero)

        Dim cost As Integer = CInt(1000 * Math.Pow(3, ownedHeroes))

        ' === 2) Check affordability ===
        If p.Gold < cost Then
            'Debug.WriteLine($"[SUMMONER] {p.Race} cannot afford {def.Name}. Needs {cost}, has {p.Gold}.")
            Exit Sub
        End If

        'Debug.WriteLine($"[SUMMONER] Deducting {cost} gold; heroes owned={ownedHeroes}")
        ' === 3) Pay ===
        p.Gold -= cost

        ' === 4) Create unit ===
        Dim fullName As String = GenerateSummonerName(p.Race, def.Name)
        Dim summonerUnit As New Unit("Summoner", fullName, p.Race)
        summonerUnit.SummonerFaction = def.RosterKey
        summonerUnit.IsHero = True
        summonerUnit.HeroType = "Summoner"
        summonerUnit.Level = 1

        ' === 5) Add to target army ===
        If targetArmy.Units Is Nothing Then targetArmy.Units = New List(Of Unit)
        targetArmy.Units.Add(summonerUnit)
        'Debug.WriteLine($"[SUMMONER] Added {def.Name} to {targetArmy.Name}. Units now: {targetArmy.Units.Count}")
        'Debug.WriteLine($"[SUMMONER] {p.Race} bought {fullName} for {targetArmy.Name} at {cost} gold. Remaining: {p.Gold}.")
    End Sub

    ' ------------------------------------------------------------
    ' Read the Buy Summoner UI and apply it to each player

    Private Sub ApplySummonerPurchases()
        If Players Is Nothing Then Exit Sub

        For Each p In Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For

            Dim race As String = p.Race.ToLowerInvariant()
            Dim suffix As String = Char.ToUpper(race(0)) & race.Substring(1).ToLower()

            Dim chk As CheckBox = TryCast(Me.Controls("chkBuySummoner" & suffix), CheckBox)
            Dim cmbSummoner As ComboBox = TryCast(Me.Controls("cmbSummoner" & suffix), ComboBox)
            Dim cmbArmy As ComboBox = TryCast(Me.Controls("cmbArmy" & suffix), ComboBox)

            If chk Is Nothing OrElse cmbSummoner Is Nothing OrElse cmbArmy Is Nothing Then
                'Debug.WriteLine($"[{p.Race}] Missing one or more controls — skipping.")
                Continue For
            End If

            ' Skip if not ticked
            If Not chk.Checked Then Continue For

            ' --- Read inputs ---
            Dim chosenSummoner As String = cmbSummoner.Text?.Trim()
            Dim chosenArmyText As String = cmbArmy.Text?.Trim()

            ' Must have a summoner chosen
            If String.IsNullOrWhiteSpace(chosenSummoner) Then
                'Debug.WriteLine($"[{p.Race}] Summoner blank — skipping.")
                Continue For
            End If

            ' --- Resolve the target army (or default to Army(0)) ---
            Dim targetArmy As Army = Nothing

            ' Try from combo selection
            Dim selectedArmyItem As ArmyListItem = TryCast(cmbArmy.SelectedItem, ArmyListItem)
            If selectedArmyItem IsNot Nothing AndAlso selectedArmyItem.Index >= 0 AndAlso selectedArmyItem.Index < p.Armies.Count Then
                targetArmy = p.Armies(selectedArmyItem.Index)
            ElseIf Not String.IsNullOrWhiteSpace(chosenArmyText) Then
                targetArmy = p.Armies.FirstOrDefault(Function(a) a.Name.Equals(chosenArmyText, StringComparison.OrdinalIgnoreCase))
            End If

            ' Default to first army if none chosen or found
            If targetArmy Is Nothing AndAlso p.Armies IsNot Nothing AndAlso p.Armies.Count > 0 Then
                targetArmy = p.Armies(0)
                'Debug.WriteLine($"[{p.Race}] No army selected — defaulting to Army #1 '{targetArmy.Name}'.")
            End If

            ' Still no army (player has none)
            If targetArmy Is Nothing Then
                'Debug.WriteLine($"[{p.Race}] No available armies to receive summoner — skipping.")
                Continue For
            End If

            ' --- Find definition for the chosen summoner ---
            Dim def As SummonerInfo = SummonerDefinitions.FirstOrDefault(
            Function(s) s.Name.Equals(chosenSummoner, StringComparison.OrdinalIgnoreCase) AndAlso
                        s.AllowedRace.Equals(p.Race, StringComparison.OrdinalIgnoreCase))

            If def Is Nothing Then
                'Debug.WriteLine($"[{p.Race}] Invalid summoner name '{chosenSummoner}' — skipping.")
                Continue For
            End If

            ' --- Apply purchase ---
            AttemptBuySummoner(p, def, targetArmy)

            ' --- Reset UI for next turn ---
            chk.Checked = False
            cmbSummoner.SelectedIndex = -1
            cmbArmy.SelectedIndex = -1
        Next
    End Sub


    Private Sub ProcessSummoners()
        For Each p In Players
            If p.Armies Is Nothing Then Continue For
            For Each a In p.Armies
                Dim unitsSnapshot = a.Units.ToList()
                For Each u In unitsSnapshot
                    If u IsNot Nothing AndAlso
                   u.IsHero AndAlso
                   u.HeroType = "Summoner" AndAlso
                   Not String.IsNullOrWhiteSpace(u.SummonerFaction) Then
                        SummonCreaturesForUnit(u, a)
                        u.Level += 1
                    End If
                Next
            Next
        Next
    End Sub

    Private Sub SummonCreaturesForUnit(summoner As Unit, ownerArmy As Army)
        ' === Validation ===
        If String.IsNullOrEmpty(summoner.SummonerFaction) Then Exit Sub
        If Not SummonerRosters.ContainsKey(summoner.SummonerFaction) Then Exit Sub

        Dim roster = SummonerRosters(summoner.SummonerFaction)
        If roster Is Nothing OrElse roster.Count = 0 Then Exit Sub

        ' === Base budget: 10 points per summoner level ===
        Dim budget As Integer = summoner.Level * 10

        ' --- Deterministic randomiser per turn/summoner ---
        Dim seed As Integer = (Math.Max(1, TurnNumber) * 97) Xor
                          (ownerArmy.X * 73856093) Xor
                          (ownerArmy.Y * 19349663) Xor
                          (If(summoner.Name, "").GetHashCode())
        Dim rnd As New Random(seed)

        ' === If too weak to summon anything, train instead ===
        Dim minPower As Integer = roster.Min(Function(u) u.Power)
        If budget < minPower Then
            summoner.Level += 1
            'Debug.WriteLine($"[SUMMON] {summoner.Name} ({summoner.SummonerFaction}) too weak to summon. Trains to Level {summoner.Level}.")
            Exit Sub
        End If

        ' === Summoning loop ===
        Dim remaining As Integer = budget
        Dim guard As Integer = 10000

        Do While remaining > 0 AndAlso guard > 0
            guard -= 1

            ' Pick a random affordable creature
            Dim affordable = roster.Where(Function(u) u.Power > 0 AndAlso u.Power <= remaining).ToList()
            If affordable.Count = 0 Then Exit Do

            Dim pick As UnitStats = affordable(rnd.Next(affordable.Count))
            If pick Is Nothing Then Exit Do

            ' Group size scaling — weak units appear in small groups
            Dim count As Integer = Math.Max(1, rnd.Next(1, 4))
            If pick.Power >= 8 Then count = 1

            ' Merge into army or add new
            Dim existing = ownerArmy.Units.FirstOrDefault(Function(u) u.Name = pick.Name)
            If existing IsNot Nothing Then
                existing.Size += count
            Else
                ownerArmy.Units.Add(New Unit(pick, ownerArmy.Race, count))
            End If

            remaining -= pick.Power
        Loop

        If guard = 0 Then
            'Debug.WriteLine("Guard triggered in SummonCreaturesForUnit — possible bad roster config.")
        End If
    End Sub


    Public Class RaceUnits
        Public Property RaceName As String
        Public Property Units As List(Of UnitStats)

        Public Function GetUnitByType(uType As UnitType) As UnitStats
            Return Units(CInt(uType))
        End Function

        Public Function GetUnitByIndex(idx As Integer) As UnitStats
            Return Units(idx)
        End Function
    End Class

    Public Class Player
        Public Property Nickname As String
        Public Property PlayerNumber As Integer
        Public Property Race As String ' Could be created on-the-fly whenever by using 'Races(PlayerNumber)' but storing it in advance for simplicity later.
        Public Property Population As Integer
        Public Property Armies As List(Of Army)
        Public Property FoodCollectedThisTurn As Integer
        Public Property Iron As Integer
        Public Property IronCollectedThisTurn As Integer
        Public Property Wood As Integer
        Public Property WoodCollectedThisTurn As Integer
        Public Property Mounts As Integer
        Public Property MountsCollectedThisTurn As Integer
        Public Property Gold As Integer
        Public Property GoldCollectedThisTurn As Integer
        Public Property Mithril As Integer
        Public Property MithrilCollectedThisTurn As Integer
        Public Property AIControlled As Boolean
        Public Property CatchUpCooldown As Integer  ' turns until next catch-up recruit allowed
        Public Property Gems As Integer
        Public Property Amber As Integer
        Public Property Wine As Integer
        Public Property Furs As Integer
        Public Property CurrentBid As Integer ' Mercenary bid for this turn
        Public Property LastMercWages As Integer = 0
        Public Property Xorns As Integer = 0
        ' === Slave tracking (Orcs only, but stored for all players) ===
        Public Property ElfSlaves As Integer = 0
        Public Property DwarfSlaves As Integer = 0
        Public Property HumanSlaves As Integer = 0
        Public Property IsEliminated As Boolean = False
        Public Property TargetArmyForMercs As Army
        Public Property Investments As Integer = 0
        Public Property PendingInvestment As Integer = 0 ' gold being spent this turn on new investments
        Public Sub New()
            ' required for JSON deserialization
        End Sub


    End Class

#End Region

#Region "=== Save / Load ==="


    Public Sub SaveGame(gameName As String)
        Try
            Dim saveDir As String = IO.Path.Combine(Application.StartupPath, "Saves", gameName)
            If Not IO.Directory.Exists(saveDir) Then IO.Directory.CreateDirectory(saveDir)

            Dim saveFile As String = IO.Path.Combine(saveDir, $"Turn{TurnNumber:D3}.json")

            ' === Apply combo assignments before saving ===
            If Players IsNot Nothing Then
                For Each p In Players
                    Select Case p.Race.ToLower()
                        Case "elf" : HandlePlayerAssignment(p, cmbElf)
                        Case "dwarf" : HandlePlayerAssignment(p, cmbDwarf)
                        Case "orc" : HandlePlayerAssignment(p, cmbOrc)
                        Case "human" : HandlePlayerAssignment(p, cmbHuman)
                    End Select
                Next
            End If

            ' === Update Seen Monsters just before saving ===
            UpdateSeenMonstersForAllPlayers(Players)

            ' === Build state ===
            Dim state As New GameState With {
            .TurnNumber = TurnNumber,
            .Players = Players,
            .Map = Map,
            .MercPriceLevel = MercPriceLevel,
            .CurrentMercOffer = CurrentMercOffer
        }

            ' === Save main game state ===
            Dim json As String = Newtonsoft.Json.JsonConvert.SerializeObject(state, Newtonsoft.Json.Formatting.Indented)
            IO.File.WriteAllText(saveFile, json)

            ' === ALSO save turn reports ===
            If CurrentReports IsNot Nothing Then
                Dim reportsFile As String = IO.Path.Combine(saveDir, $"Turn{TurnNumber:D3}_Reports.json")
                Dim reportsJson As String = Newtonsoft.Json.JsonConvert.SerializeObject(CurrentReports, Newtonsoft.Json.Formatting.Indented)
                IO.File.WriteAllText(reportsFile, reportsJson)
            End If

        Catch ex As Exception
            MessageBox.Show($"Error saving game: {ex.Message}")
        End Try
    End Sub



    ' ============================================================
    ' Helper to apply human/AI assignment based on combo selection
    ' ============================================================
    Private Sub HandlePlayerAssignment(p As Player, combo As ComboBox)
        Dim sel As String = If(combo.SelectedItem?.ToString(), "").Trim()

        If String.IsNullOrEmpty(sel) OrElse sel.Equals("AI", StringComparison.OrdinalIgnoreCase) Then
            p.Nickname = ""
            p.AIControlled = True
        Else
            p.Nickname = sel
            p.AIControlled = False
        End If
    End Sub

    ' ============================================================
    '  Load selected game from combo box
    ' ============================================================
    Private Sub PopulateGameList()
        Try
            cmbGameSelect.Items.Clear()
            cmbGameSelect.Items.Add("(Select Game)")

            Dim savesRoot As String = Path.Combine(Application.StartupPath, "Saves")
            If Not Directory.Exists(savesRoot) Then Directory.CreateDirectory(savesRoot)

            For Each folderPath In Directory.GetDirectories(savesRoot)
                Dim gameName As String = Path.GetFileName(folderPath)
                cmbGameSelect.Items.Add(gameName)
            Next

            cmbGameSelect.SelectedIndex = 0
        Catch ex As Exception
            Debug.WriteLine($"[GAMELIST ERROR] {ex.Message}")
        End Try
    End Sub

    Private Sub cmbGameSelect_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbGameSelect.SelectedIndexChanged
        Try
            If cmbGameSelect.SelectedIndex <= 0 Then Exit Sub ' ignore placeholder

            Dim selectedFolder As String = cmbGameSelect.SelectedItem.ToString().Trim()
            If String.IsNullOrWhiteSpace(selectedFolder) Then Exit Sub

            Dim numPart As String = New String(selectedFolder.Where(AddressOf Char.IsDigit).ToArray())
            Dim n As Integer
            If Not Integer.TryParse(numPart, n) Then n = 1

            GameNumber = n
            LoadGame(selectedFolder)      ' <-- load the most recent turn as usual

            ' === Populate the Turn list for this game ===
            PopulateTurnList(selectedFolder)

        Catch ex As Exception
            MessageBox.Show($"Error loading selected game: {ex.Message}")
        End Try
    End Sub


    Private Sub PopulateTurnList(gameName As String)
        IsPopulatingTurns = True
        cmbTurn.Items.Clear()

        Dim saveDir As String = IO.Path.Combine(Application.StartupPath, "Saves", gameName)
        If Not IO.Directory.Exists(saveDir) Then
            IsPopulatingTurns = False
            Exit Sub
        End If

        Dim turnFiles = IO.Directory.GetFiles(saveDir, "Turn*.json")
        Dim turnNumbers = turnFiles.
        Select(Function(f)
                   Dim name = IO.Path.GetFileNameWithoutExtension(f)
                   Dim digits = New String(name.SkipWhile(Function(c) Not Char.IsDigit(c)).ToArray())
                   Dim num As Integer
                   If Integer.TryParse(digits, num) Then Return num Else Return -1
               End Function).
        Where(Function(n) n >= 0).
        Distinct().
        OrderBy(Function(n) n)

        For Each num In turnNumbers
            cmbTurn.Items.Add($"Turn {num:D3}")
        Next

        If cmbTurn.Items.Count > 0 Then
            cmbTurn.SelectedIndex = cmbTurn.Items.Count - 1
        End If

        IsPopulatingTurns = False
    End Sub


    Private Sub cmbTurn_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbTurn.SelectedIndexChanged
        If IsPopulatingTurns Then Exit Sub  ' <--- ignore changes during fill

        Dim gameName As String = cmbGameSelect.SelectedItem?.ToString()
        If String.IsNullOrWhiteSpace(gameName) Then Exit Sub

        Dim turnText As String = cmbTurn.SelectedItem?.ToString()
        If String.IsNullOrWhiteSpace(turnText) OrElse Not turnText.Contains(" ") Then Exit Sub

        Dim turnNum As Integer
        If Not Integer.TryParse(turnText.Split(" "c)(1), turnNum) Then Exit Sub

        LoadGame(gameName, turnNum)

        ' Load matching reports (if any)
        Dim saveDir As String = IO.Path.Combine(Application.StartupPath, "Saves", gameName)
        Dim reportsFile As String = IO.Path.Combine(saveDir, $"Turn{turnNum:D3}_Reports.json")
        If IO.File.Exists(reportsFile) Then
            Dim jsonReports = IO.File.ReadAllText(reportsFile)
            CurrentReports = JsonConvert.DeserializeObject(Of TurnReports)(jsonReports)
        Else
            CurrentReports = New TurnReports With {.TurnNumber = turnNum}
        End If

        'MessageBox.Show($"Loaded Turn {turnNum:D3} for {gameName} in read-only mode. Restart the program to resume play.")
    End Sub


    Public Sub LoadGame(gameName As String, Optional loadTurnNumber As Integer? = Nothing)
        dgvOrders.Rows.Clear()

        Try
            isRestoringCombos = True
            Dim saveDir As String = IO.Path.Combine(Application.StartupPath, "Saves", gameName)
            If Not IO.Directory.Exists(saveDir) Then
                MessageBox.Show($"No saved games found for {gameName}.")
                Exit Sub
            End If

            ' --- Choose file ---
            Dim saveFile As String
            If loadTurnNumber.HasValue Then
                saveFile = IO.Path.Combine(saveDir, $"Turn{loadTurnNumber.Value:D3}.json")
            Else
                saveFile = IO.Directory.GetFiles(saveDir, "Turn*.json").
                        OrderByDescending(Function(f) f).FirstOrDefault()
            End If

            If saveFile Is Nothing OrElse Not IO.File.Exists(saveFile) Then
                MessageBox.Show($"No save file found in {saveDir}.")
                Exit Sub
            End If

            ' --- Deserialize state ---
            Dim json As String = IO.File.ReadAllText(saveFile)
            Dim state As GameState = Newtonsoft.Json.JsonConvert.DeserializeObject(Of GameState)(json)

            ' --- Restore core data ---
            TurnNumber = state.TurnNumber
            Players = state.Players
            Map = state.Map
            MercPriceLevel = state.MercPriceLevel
            CurrentMercOffer = state.CurrentMercOffer

            ' --- Rebuild runtime-only data ---
            InitializeRaceUnits()
            If terrainCache Is Nothing OrElse terrainCache.Count = 0 Then CreateTerrainCache()
            PostLoadFixups()

            ' --- UI refresh ---
            lblHud.Text = $"Game: {gameName}" & vbCrLf &
                      $"Turn: {TurnNumber}" & vbCrLf &
                      $"Next Merc Cost: {MercPriceLevel}"

            pnlMap.Invalidate()
            pnlMap.Update()
            rtbPlayerSummary.Clear()
            rtbPlayerSummary.AppendText(GenerateEmpireSummary())
            UpdateArmiesReport()

            ' --- Orders grid and combos ---
            If Players IsNot Nothing AndAlso Players.Count > 0 Then
                PopulateArmyOrdersGrid()
                InitialiseMoveColumns()
            End If
            RestoreCustomerCombosFromPlayers()
            isRestoringCombos = False

            ' === If this was an explicit old-turn load, lock interface ===
            If loadTurnNumber.HasValue Then
                btnProcessTurn.Enabled = False
                dgvOrders.Enabled = False
                cmbElf.Enabled = False
                cmbDwarf.Enabled = False
                cmbOrc.Enabled = False
                cmbHuman.Enabled = False
                lblHud.Text &= vbCrLf & "(read-only)"
                'ViewingArchivedTurn = True
            Else
                ' Normal load: re-enable everything just to be sure
                btnProcessTurn.Enabled = True
                dgvOrders.Enabled = True
                cmbElf.Enabled = True
                cmbDwarf.Enabled = True
                cmbOrc.Enabled = True
                cmbHuman.Enabled = True
            End If

            ' === Rebuild all player-related UI lists after loading ===
            RefreshArmyOrdersGrid()

        Catch ex As Exception
            MessageBox.Show($"Error loading game: {ex.Message}")
        End Try
    End Sub



    ' Select an item in a ComboBox by the text it displays (works with strings or bound objects).
    Private Sub SelectComboByDisplayText(combo As ComboBox, displayText As String)
        If combo Is Nothing Then Exit Sub
        Dim target As String = (If(displayText, "")).Trim()

        ' Try exact match first.
        Dim idx As Integer = combo.FindStringExact(target)

        ' If not found, try case-insensitive/trimmed compare using the display text of each item.
        If idx < 0 Then
            For i As Integer = 0 To combo.Items.Count - 1
                Dim shown As String = combo.GetItemText(combo.Items(i))
                If String.Equals(shown?.Trim(), target, StringComparison.OrdinalIgnoreCase) Then
                    idx = i : Exit For
                End If
            Next
        End If

        ' Apply selection (will leave blank if still not found).
        combo.SelectedIndex = idx

        'Debug.WriteLine($"[LOAD] {combo.Name} -> '{target}', index={idx}, items={combo.Items.Count}")
    End Sub

    ' Restore the four race/customer combos from Players without adding anything.
    Private Sub RestoreCustomerCombosFromPlayers()
        isRestoringCombos = True
        Try
            If Players Is Nothing Then Exit Sub

            Dim elfName = Players.FirstOrDefault(Function(p) p IsNot Nothing AndAlso p.Race.Equals("Elf", StringComparison.OrdinalIgnoreCase))?.Nickname
            Dim dwarfName = Players.FirstOrDefault(Function(p) p IsNot Nothing AndAlso p.Race.Equals("Dwarf", StringComparison.OrdinalIgnoreCase))?.Nickname
            Dim orcName = Players.FirstOrDefault(Function(p) p IsNot Nothing AndAlso p.Race.Equals("Orc", StringComparison.OrdinalIgnoreCase))?.Nickname
            Dim humanName = Players.FirstOrDefault(Function(p) p IsNot Nothing AndAlso p.Race.Equals("Human", StringComparison.OrdinalIgnoreCase))?.Nickname

            ' Only select if we actually have a non-blank nickname.
            If Not String.IsNullOrWhiteSpace(elfName) Then SelectComboByDisplayText(cmbElf, elfName)
            If Not String.IsNullOrWhiteSpace(dwarfName) Then SelectComboByDisplayText(cmbDwarf, dwarfName)
            If Not String.IsNullOrWhiteSpace(orcName) Then SelectComboByDisplayText(cmbOrc, orcName)
            If Not String.IsNullOrWhiteSpace(humanName) Then SelectComboByDisplayText(cmbHuman, humanName)
        Finally
            isRestoringCombos = False
        End Try
    End Sub

    Private Sub PostLoadFixups()
        If Players Is Nothing Then Exit Sub

        For Each p In Players
            If p.Armies Is Nothing Then p.Armies = New List(Of Army)
            For Each a In p.Armies
                If a.Units Is Nothing Then a.Units = New List(Of Unit)
                If a.MoveQueue Is Nothing Then a.MoveQueue = New List(Of ArmyCommand)
                a.HasUsedSpecial = False
                a.RetreatedThisTurn = False

                ' Defensive fixups
                If String.IsNullOrWhiteSpace(a.Race) Then a.Race = p.Race

                For Each u In a.Units
                    If u Is Nothing Then Continue For
                    If u.FoodCost <= 0 AndAlso Not u.IsHero Then u.FoodCost = 1
                    If u.IsHero AndAlso String.Equals(u.HeroType, "Summoner", StringComparison.OrdinalIgnoreCase) _
                   AndAlso u.Level <= 0 Then u.Level = 1
                Next
            Next
        Next
    End Sub


#End Region






    ' Helper to store info about each army involved
    Public Class CombatArmyInfo
        Public Property PlayerNumber As Integer
        Public Property PlayerRace As String
        Public Property InitialSoldiers As Integer
        Public Property RemainingSoldiers As Integer
        Public Property ArmyReference As Army               ' Optional reference back to the army
    End Class


    Public Class BattleLog
        ''' <summary>
        ''' Stores the results of a battle for reporting and printing.
        ''' Tracks per-phase damage, casualties, and army retreats.
        ''' </summary>

        ' Phase-by-phase log: dictionary of phase name -> list of entries
        Public PhaseEntries As New Dictionary(Of String, List(Of BattleEntry))

        ' Optional: track armies sent back to spawn
        Public Retreats As New List(Of Army)

        ' Constructor
        Public Sub New(armies As List(Of Army))
            ' Initialize phase entries
            PhaseEntries("Ranged") = New List(Of BattleEntry)
            PhaseEntries("Charge") = New List(Of BattleEntry)
            PhaseEntries("Melee") = New List(Of BattleEntry)
            PhaseEntries("Chase") = New List(Of BattleEntry)
        End Sub

        ''' <summary>
        ''' Record a single battle entry.
        ''' Mitigation explanation is passed in from ApplyProportionalDamage.
        ''' </summary>
        Public Sub Record(attacker As Unit,
                      defender As Unit,
                      rawDamage As Double,
                      finalDamage As Double,
                      mitigation As Double,
                      mitigationExplanation As String,
                      phase As String,
                      sizeBefore As Integer,
                      sizeAfter As Integer,
                      casualties As Integer)

            ' Ensure the phase exists
            If Not PhaseEntries.ContainsKey(phase) Then
                PhaseEntries(phase) = New List(Of BattleEntry)
            End If

            ' Add the entry
            PhaseEntries(phase).Add(New BattleEntry With {
            .Attacker = attacker,
            .Defender = defender,
            .Phase = phase,
            .SizeBefore = sizeBefore,
            .SizeAfter = sizeAfter,
            .RawDamage = rawDamage,
            .Mitigation = mitigation,
            .MitigationExplanation = mitigationExplanation,
            .FinalDamage = finalDamage,
            .Casualties = casualties
        })
        End Sub

        ' Record army retreat
        Public Sub RecordRetreat(army As Army)
            Retreats.Add(army)
        End Sub
    End Class


    Public Class BattleEntry
        Public Property Attacker As Unit
        Public Property Defender As Unit
        Public Property Phase As String

        ' Snapshot of defender size
        Public Property SizeBefore As Integer
        Public Property SizeAfter As Integer

        ' --- NEW fields for detailed reporting ---
        Public Property RawDamage As Double
        Public Property Mitigation As Double
        Public Property MitigationExplanation As String
        Public Property FinalDamage As Double
        Public Property Casualties As Integer
        Public Property RawDamageExplanation As String

        ' --- NEW: Attacker size at start of phase ---
        Public Property AttackerSizeAtPhaseStart As Integer
    End Class




#Region "=== Fields ==="

    Public Players As List(Of Player)

    Public playerColors As Color() = {
        Color.LightGreen,   ' Elf
        Color.LightSkyBlue, ' Dwarf
        Color.LightCoral,   ' Orc
        Color.Yellow        ' Human
    }

    Private WithEvents printDoc As New PrintDocument

    Public TurnNumber As Integer = 0
    Public GameNumber As Integer = 0

    Public AllRaces As New List(Of RaceUnits)

    Private Shared rnd As New Random()

    Private capitals As New Dictionary(Of Integer, Point) From {
        {0, New Point(2, 2)},     ' Elf
        {1, New Point(22, 2)},    ' Dwarf
        {2, New Point(2, 22)},    ' Orc
        {3, New Point(22, 22)}    ' Human
    }


#End Region



#Region "=== New / Load Game ==="

    Private Sub btnNewGame_Click(sender As Object, e As EventArgs) Handles btnNewGame.Click
        ' --- Reset all player assignments to AI before creating a new game ---
        isRestoringCombos = True
        Try
            cmbElf.SelectedIndex = 0
            cmbDwarf.SelectedIndex = 0
            cmbOrc.SelectedIndex = 0
            cmbHuman.SelectedIndex = 0
        Finally
            isRestoringCombos = False
        End Try

        ' 1) Pick next number
        Dim nextNum As Integer = GetNextAvailableGameNumber()
        GameNumber = nextNum

        ' 2) Create the folder name "Game###" and start the game
        Dim folderName As String = FormatGameFolder(nextNum)
        StartNewGame(folderName)   ' this will SaveGame(Turn000) immediately

        ' 3) Refresh the combobox and select the newly created game
        PopulateGameList()
        SelectGameInCombo(folderName)   ' will trigger load via SelectedIndexChanged
        RefreshArmyOrdersGrid()
    End Sub



    ' Returns the next available integer (1-based) for a folder named "Game###"
    Private Function GetNextAvailableGameNumber() As Integer
        Dim savesRoot As String = Path.Combine(Application.StartupPath, "Saves")
        If Not Directory.Exists(savesRoot) Then Directory.CreateDirectory(savesRoot)

        Dim maxNum As Integer = 0
        For Each folderPath In Directory.GetDirectories(savesRoot)
            Dim name As String = Path.GetFileName(folderPath) ' e.g., "Game001"
            If name.StartsWith("Game", StringComparison.OrdinalIgnoreCase) Then
                Dim digits As String = New String(name.Where(AddressOf Char.IsDigit).ToArray())
                Dim n As Integer
                If Integer.TryParse(digits, n) AndAlso n > maxNum Then maxNum = n
            End If
        Next
        Return maxNum + 1
    End Function


    Private Function FormatGameFolder(n As Integer) As String
        Return $"Game{n:D3}"
    End Function

    Private Sub SelectGameInCombo(folderName As String)
        Dim idx As Integer = cmbGameSelect.FindStringExact(folderName)
        If idx >= 0 Then cmbGameSelect.SelectedIndex = idx
    End Sub


    Private Sub StartNewGame(gameName As String)
        ' --- Clear existing data ---
        Players = Nothing
        rtbInfo.Clear()
        TurnNumber = 0
        MercPriceLevel = 50
        CurrentMercOffer = Nothing

        ' --- Initialize everything ---
        CreateTerrainCache()
        InitializeRaceUnits()
        InitializePlayers()
        GenerateMap()

        ' --- Generate the first merc offer for turn 0 ---
        CurrentMercOffer = GenerateMercenaryOffer(TurnNumber)
        'Debug.WriteLine("[MERC] Initial offer generated at game start.")

        ' --- Mark all currently visible unit types as "seen" for all human players ---
        UpdateSeenMonstersForAllPlayers(Players)

        ' --- Update HUD ---
        lblHud.Text = $"Game: {gameName}" & vbCrLf &
                  $"Turn: {TurnNumber}" & vbCrLf &
                  $"Next Merc Cost: {MercPriceLevel}"

        ' --- Draw map ---
        pnlMap.Invalidate()

        ' --- Immediately save as Turn000 so it’s ready for printing / orders ---
        SaveGame(gameName)

        'Debug.WriteLine($"[NEW GAME] {gameName} created successfully (Turn {currentTurnNumber}).")
    End Sub

    Public Sub InitializeRaceUnits()
        AllRaces = New List(Of RaceUnits)

        ' ----------------- Elves -----------------
        Dim elfUnits As New RaceUnits With {
    .RaceName = "Elf",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Elven Archers", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 2, .DefencePoints = 0, .Cost = "W:1", .ShortName = "a"},
        New UnitStats With {.Name = "Light Rangers", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .Cost = "I:1, W:1", .ShortName = "li"},
        New UnitStats With {.Name = "Heavy Warblades", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 3, .Ranged = 0, .DefencePoints = 2, .Cost = "I:2, W:2", .ShortName = "hi", .CanCharge = True},
        New UnitStats With {.Name = "Light Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 2, .Ranged = 2, .DefencePoints = 1, .Cost = "I:1, W:1, M:1", .ShortName = "lc", .CanChase = True},
        New UnitStats With {.Name = "Heavy Lancers", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 3, .Ranged = 0, .DefencePoints = 2, .Cost = "I:2, W:2, M:1", .ShortName = "hc", .CanCharge = True}
        }
}
        AllRaces.Add(elfUnits)

        ' ----------------- Dwarves -----------------
        Dim dwarfUnits As New RaceUnits With {
    .RaceName = "Dwarf",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Dwarven Crossbows", .Type = UnitType.Archer, .HP = 6, .Melee = 1, .Ranged = 1, .DefencePoints = 1, .Cost = "I:2, W:1", .ShortName = "a"},
        New UnitStats With {.Name = "Light Ironfoot", .Type = UnitType.LightInfantry, .HP = 7, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .Cost = "I:2", .ShortName = "li"},
        New UnitStats With {.Name = "Heavy Ironfoot", .Type = UnitType.HeavyInfantry, .HP = 15, .Melee = 3, .Ranged = 0, .DefencePoints = 3, .Cost = "I:3", .ShortName = "hi", .CanCharge = True},
        New UnitStats With {.Name = "Light Ramriders", .Type = UnitType.LightCavalry, .HP = 7, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .Cost = "I:3, M:1", .ShortName = "lc", .CanChase = True},
        New UnitStats With {.Name = "War Chariots", .Type = UnitType.HeavyCavalry, .HP = 15, .Melee = 3, .Ranged = 0, .DefencePoints = 3, .Cost = "I:3, M:2", .ShortName = "hc", .CanCharge = True},
        New UnitStats With {.Name = "Mithril Guards", .Type = UnitType.HeavyInfantry, .HP = 25, .Melee = 3, .Ranged = 0, .DefencePoints = 10, .Cost = "I:1, X:1", .ShortName = "mg", .CanCharge = True}
    }
}
        AllRaces.Add(dwarfUnits)

        ' ----------------- Orcs -----------------
        Dim orcUnits As New RaceUnits With {
    .RaceName = "Orc",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Orc Warbows", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .Cost = "I:1, W:1", .ShortName = "a"},
        New UnitStats With {.Name = "Light Grunts", .Type = UnitType.LightInfantry, .HP = 6, .Melee = 2, .Ranged = 0, .DefencePoints = 3, .Cost = "I:1", .ShortName = "li"},
        New UnitStats With {.Name = "Heavy Grunts", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 3, .Ranged = 0, .DefencePoints = 2, .Cost = "I:2", .ShortName = "hi", .CanCharge = True},
        New UnitStats With {.Name = "Light Warg Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .Cost = "I:1, M:1", .ShortName = "lc", .CanChase = True},
        New UnitStats With {.Name = "Heavy Warg Riders", .Type = UnitType.HeavyCavalry, .HP = 15, .Melee = 3, .Ranged = 0, .DefencePoints = 3, .Cost = "I:2, M:1", .ShortName = "hc", .CanCharge = True}
    }
}
        AllRaces.Add(orcUnits)

        ' ----------------- Humans -----------------
        Dim humanUnits As New RaceUnits With {
    .RaceName = "Human",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Longbowmen", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .Cost = "I:1, W:1", .ShortName = "a"},
        New UnitStats With {.Name = "Light Spearmen", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .Cost = "I:2, W:1", .ShortName = "li"},
        New UnitStats With {.Name = "Heavy Footmen", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 3, .Ranged = 0, .DefencePoints = 3, .Cost = "I:3, W:1", .ShortName = "hi", .CanCharge = True},
        New UnitStats With {.Name = "Light Horsemen", .Type = UnitType.LightCavalry, .HP = 8, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .Cost = "I:2, W:1, M:1", .ShortName = "lc", .CanChase = True},
        New UnitStats With {.Name = "Knights", .Type = UnitType.HeavyCavalry, .HP = 20, .Melee = 4, .Ranged = 0, .DefencePoints = 5, .Cost = "I:5, M:1", .ShortName = "hc", .CanCharge = True}
    }
}
        AllRaces.Add(humanUnits)


        Dim druidSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Faun Infantry", .Power = 3, .HP = 8, .Melee = 2, .DefencePoints = 2, .FoodCost = 1},
            New UnitStats With {.Name = "Satyr Warriors", .Power = 4, .HP = 12, .Melee = 3, .DefencePoints = 2, .FoodCost = 1},
            New UnitStats With {.Name = "Forest Wolves", .Power = 4, .HP = 8, .Melee = 2, .DefencePoints = 4, .FoodCost = 1, .CanChase = True},
            New UnitStats With {.Name = "Giant Eagles", .Power = 15, .HP = 30, .Melee = 4, .Ranged = 2, .DefencePoints = 7, .CanChase = True, .Flying = True}
        }
        Dim druidRoster As New RaceUnits With {
            .RaceName = "Druid",   ' << this string is the key your SummonerFaction must use
            .Units = druidSummonerUnits
        }
        AllRaces.Add(druidRoster)

        ' === Centaur Warchief Summoner Roster ===
        Dim centaurSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Centaurs", .Power = 4, .HP = 8, .Melee = 1, .Ranged = 2, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer}
        }
        Dim centaurRoster As New RaceUnits With {
            .RaceName = "Centaur Warchief",   ' key used by SummonerFaction
            .Units = centaurSummonerUnits
        }
        AllRaces.Add(centaurRoster)

        ' === Archdruid Summoner Roster ===
        Dim archdruidSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Fey Knights", .Power = 30, .HP = 40, .Melee = 6, .DefencePoints = 8, .FoodCost = 1, .CanCharge = True, .CanChase = True},
            New UnitStats With {.Name = "Unicorns", .Power = 100, .HP = 60, .Melee = 7, .DefencePoints = 10, .CanCharge = True, .CanChase = True},
            New UnitStats With {.Name = "Ents", .Power = 200, .HP = 90, .Melee = 11, .DefencePoints = 14}
        }

        Dim archdruidRoster As New RaceUnits With {
            .RaceName = "Archdruid",   ' key used by SummonerFaction
            .Units = archdruidSummonerUnits
        }

        AllRaces.Add(archdruidRoster)

        ' === Gnome Commander Summoner Roster ===
        Dim gnomeCommanderUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Gnome Infantry", .Power = 1, .HP = 4, .Melee = 1, .DefencePoints = 1, .FoodCost = 0.8},
            New UnitStats With {.Name = "Gnome Crossbowmen", .Power = 2, .HP = 5, .Melee = 1, .Ranged = 1, .DefencePoints = 1, .FoodCost = 0.8},
            New UnitStats With {.Name = "Gnome Gyrocopters", .Power = 10, .HP = 20, .Melee = 3, .Ranged = 2, .DefencePoints = 5, .CanChase = True, .Flying = True, .FoodCost = 0.8}
        }

        Dim gnomeRoster As New RaceUnits With {
            .RaceName = "Gnome Commander",   ' key used by SummonerFaction
            .Units = gnomeCommanderUnits
        }

        AllRaces.Add(gnomeRoster)

        ' === Giant Overlord Summoner Roster ===
        Dim giantOverlordUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Hill Giants", .Power = 100, .HP = 250, .Melee = 20, .Ranged = 0, .DefencePoints = 10, .FoodCost = 3, .Type = UnitType.HeavyInfantry, .CanCharge = True},
            New UnitStats With {.Name = "Stone Giants", .Power = 200, .HP = 300, .Melee = 25, .Ranged = 0, .DefencePoints = 12, .FoodCost = 3, .Type = UnitType.HeavyInfantry, .CanCharge = True},
            New UnitStats With {.Name = "Frost Giants", .Power = 350, .HP = 350, .Melee = 30, .Ranged = 8, .DefencePoints = 14, .FoodCost = 4, .Type = UnitType.HeavyInfantry, .CanCharge = True, .CanChase = True},
            New UnitStats With {.Name = "Fire Giants", .Power = 500, .HP = 400, .Melee = 40, .Ranged = 10, .DefencePoints = 16, .FoodCost = 5, .Type = UnitType.HeavyInfantry, .CanCharge = True},
            New UnitStats With {.Name = "Cloud Giants", .Power = 650, .HP = 450, .Melee = 45, .Ranged = 15, .DefencePoints = 18, .FoodCost = 6, .Type = UnitType.HeavyCavalry, .Flying = True, .CanCharge = True},
            New UnitStats With {.Name = "Storm Giants", .Power = 800, .HP = 500, .Melee = 50, .Ranged = 20, .DefencePoints = 20, .FoodCost = 7, .Type = UnitType.HeavyCavalry, .Flying = True, .CanCharge = True, .CanChase = True}
        }

        Dim giantOverlordRoster As New RaceUnits With {
            .RaceName = "Giant Overlord",
            .Units = giantOverlordUnits
        }
        AllRaces.Add(giantOverlordRoster)


        ' === Runesmith Summoner Roster ===
        Dim runesmithSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Wood Construct", .Power = 5, .HP = 20, .Melee = 1, .DefencePoints = 5},
            New UnitStats With {.Name = "Stone Construct", .Power = 10, .HP = 60, .Melee = 1, .DefencePoints = 8},
            New UnitStats With {.Name = "Iron Construct", .Power = 20, .HP = 120, .Melee = 1, .DefencePoints = 12},
            New UnitStats With {.Name = "Mithril Construct", .Power = 500, .HP = 100, .Melee = 1, .Ranged = 0, .DefencePoints = 20, .FoodCost = 0, .Type = UnitType.HeavyInfantry}
        }

        Dim runesmithRoster As New RaceUnits With {
            .RaceName = "Runesmith",   ' key used by SummonerFaction
            .Units = runesmithSummonerUnits
        }

        AllRaces.Add(runesmithRoster)

        ' === Kobold War Chief Summoner Roster ===
        Dim koboldSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Kobold Rabble", .Power = 1, .HP = 2, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 0.1, .Type = UnitType.LightInfantry},
            New UnitStats With {.Name = "Kobold Slinger", .Power = 1, .HP = 2, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 0.1, .Type = UnitType.Archer}
        }
        Dim koboldRoster As New RaceUnits With {
            .RaceName = "Kobold War Chief",   ' key used by SummonerFaction
            .Units = koboldSummonerUnits
        }
        AllRaces.Add(koboldRoster)

        ' === Goblin Chieftain Summoner Roster ===
        Dim goblinSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Goblin Grunts", .Power = 2, .HP = 3, .Melee = 1, .Ranged = 0, .DefencePoints = 1, .FoodCost = 0.2, .Type = UnitType.LightInfantry},
            New UnitStats With {.Name = "Goblin Skirmishers", .Power = 2, .HP = 3, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 0.2, .Type = UnitType.Archer},
            New UnitStats With {.Name = "Hobgoblins", .Power = 5, .HP = 6, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .FoodCost = 0.4, .CanCharge = True, .Type = UnitType.HeavyInfantry},
            New UnitStats With {.Name = "Goblin Spider Riders", .Power = 5, .HP = 6, .Melee = 1, .Ranged = 0, .DefencePoints = 2, .FoodCost = 0.4, .CanChase = True, .Type = UnitType.LightCavalry}
        }
        Dim goblinRoster As New RaceUnits With {
            .RaceName = "Goblin Chieftain",   ' key used by SummonerFaction
            .Units = goblinSummonerUnits
        }
        AllRaces.Add(goblinRoster)

        ' === Gnoll Packlord Summoner Roster ===
        Dim gnollSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Gnoll Warriors", .Power = 5, .HP = 6, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1.0, .Type = UnitType.LightInfantry}
        }
        Dim gnollRoster As New RaceUnits With {
            .RaceName = "Gnoll Packlord",   ' key used by SummonerFaction
            .Units = gnollSummonerUnits
        }
        AllRaces.Add(gnollRoster)

        ' === Lizardman High Priest Summoner Roster ===
        Dim lizardmanSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Lizardman Warrior", .Power = 6, .HP = 7, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .Type = UnitType.HeavyInfantry},
            New UnitStats With {.Name = "Lizardman Hunter", .Power = 7, .HP = 5, .Melee = 1, .Ranged = 2, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer},
            New UnitStats With {.Name = "Raptor Rider", .Power = 20, .HP = 10, .Melee = 3, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .CanChase = True, .Type = UnitType.LightCavalry},
            New UnitStats With {.Name = "Lizardman Priest", .Power = 50, .HP = 10, .Melee = 1, .Ranged = 10, .DefencePoints = 5, .FoodCost = 1, .Type = UnitType.Archer}
        }
        Dim lizardmanRoster As New RaceUnits With {
            .RaceName = "Lizardman High Priest",   ' key used by SummonerFaction
            .Units = lizardmanSummonerUnits
        }
        AllRaces.Add(lizardmanRoster)

        ' === Ogre Warlord Summoner Roster ===
        Dim ogreSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Ogre Brutes", .Power = 50, .HP = 25, .Melee = 7, .Ranged = 0, .DefencePoints = 5, .FoodCost = 1.5, .CanCharge = True, .Type = UnitType.HeavyInfantry},
            New UnitStats With {.Name = "Ogre Warhulks", .Power = 100, .HP = 50, .Melee = 15, .Ranged = 0, .DefencePoints = 10, .FoodCost = 3.0, .CanCharge = True, .Type = UnitType.HeavyInfantry}
        }
        Dim ogreRoster As New RaceUnits With {
            .RaceName = "Ogre Warlord",   ' key used by SummonerFaction
            .Units = ogreSummonerUnits
        }
        AllRaces.Add(ogreRoster)

        ' === Troll Caller Summoner Roster ===
        Dim trollSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Hill Troll", .Power = 50, .HP = 30, .Melee = 8, .Ranged = 0, .DefencePoints = 5, .FoodCost = 1.5, .CanCharge = True, .Type = UnitType.HeavyInfantry},
            New UnitStats With {.Name = "Cave Troll", .Power = 100, .HP = 60, .Melee = 12, .Ranged = 0, .DefencePoints = 8, .FoodCost = 2.5, .CanCharge = True, .Type = UnitType.HeavyInfantry},
            New UnitStats With {.Name = "War Troll", .Power = 150, .HP = 100, .Melee = 18, .Ranged = 0, .DefencePoints = 12, .FoodCost = 4.0, .CanCharge = True, .Type = UnitType.HeavyInfantry}
        }
        Dim trollRoster As New RaceUnits With {
            .RaceName = "Troll Tamer",   ' key used by SummonerFaction
            .Units = trollSummonerUnits
        }
        AllRaces.Add(trollRoster)

        ' === Orc Dragonbinder Summoner Roster (retuned with battle-accurate metric) ===
        Dim dragonbinderSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "White Dragon", .Power = 200, .HP = 180, .Melee = 35, .Ranged = 35, .DefencePoints = 10, .FoodCost = 5.0, .CanCharge = True, .CanChase = True, .Flying = True},
            New UnitStats With {.Name = "Black Dragon", .Power = 350, .HP = 260, .Melee = 55, .Ranged = 55, .DefencePoints = 12, .FoodCost = 6.0, .CanCharge = True, .CanChase = True, .Flying = True},
            New UnitStats With {.Name = "Green Dragon", .Power = 500, .HP = 360, .Melee = 75, .Ranged = 75, .DefencePoints = 14, .FoodCost = 7.0, .CanCharge = True, .CanChase = True, .Flying = True},
            New UnitStats With {.Name = "Blue Dragon", .Power = 650, .HP = 460, .Melee = 95, .Ranged = 95, .DefencePoints = 16, .FoodCost = 8.0, .CanCharge = True, .CanChase = True, .Flying = True},
            New UnitStats With {.Name = "Red Dragon", .Power = 800, .HP = 560, .Melee = 115, .Ranged = 115, .DefencePoints = 18, .FoodCost = 10.0, .CanCharge = True, .CanChase = True, .Flying = True}
        }
        Dim dragonbinderRoster As New RaceUnits With {
            .RaceName = "Orc Dragonbinder",   ' key used by SummonerFaction
            .Units = dragonbinderSummonerUnits
        }
        AllRaces.Add(dragonbinderRoster)


        ' === Human Summoners ===
        ' === Cleric Summoner Roster ===

        Dim clericSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Levy", .Power = 2, .HP = 5, .Melee = 1, .DefencePoints = 1, .FoodCost = 1},
            New UnitStats With {.Name = "Militia Spearmen", .Power = 4, .HP = 5, .Melee = 2, .DefencePoints = 4, .FoodCost = 1},
            New UnitStats With {.Name = "Crossbowmen", .Power = 5, .HP = 5, .Melee = 1, .Ranged = 1, .DefencePoints = 1, .FoodCost = 1}
        }
        Dim clericRoster As New RaceUnits With {
            .RaceName = "Cleric",   ' << this string is the key your SummonerFaction must use
            .Units = clericSummonerUnits
        }
        AllRaces.Add(clericRoster)

        ' === Beastmaster Summoner Roster ===
        Dim beastmasterSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "War Dog", .Power = 3, .HP = 4, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 0.2, .CanChase = True, .Type = UnitType.LightCavalry},
            New UnitStats With {.Name = "War Bear", .Power = 10, .HP = 12, .Melee = 4, .Ranged = 0, .DefencePoints = 3, .FoodCost = 0.5, .CanCharge = True, .Type = UnitType.HeavyInfantry},
            New UnitStats With {.Name = "War Rhino", .Power = 30, .HP = 25, .Melee = 6, .Ranged = 0, .DefencePoints = 6, .FoodCost = 1.0, .CanCharge = True, .Type = UnitType.HeavyCavalry},
            New UnitStats With {.Name = "War Elephant", .Power = 50, .HP = 40, .Melee = 8, .Ranged = 0, .DefencePoints = 8, .FoodCost = 2.0, .CanCharge = True, .Type = UnitType.HeavyCavalry}
        }
        Dim beastmasterRoster As New RaceUnits With {
            .RaceName = "Beastmaster",   ' key used by SummonerFaction
            .Units = beastmasterSummonerUnits
        }
        AllRaces.Add(beastmasterRoster)

        ' === Dragonlord Summoner Roster (retuned with battle-accurate metric) ===
        Dim dragonlordSummonerUnits As New List(Of UnitStats) From {
            New UnitStats With {.Name = "Brass Dragon", .Power = 200, .HP = 200, .Melee = 40, .Ranged = 40, .DefencePoints = 12, .FoodCost = 5.5, .CanCharge = True, .CanChase = True, .Flying = True},
            New UnitStats With {.Name = "Copper Dragon", .Power = 450, .HP = 350, .Melee = 70, .Ranged = 70, .DefencePoints = 14, .FoodCost = 6.5, .CanCharge = True, .CanChase = True, .Flying = True},
            New UnitStats With {.Name = "Bronze Dragon", .Power = 700, .HP = 500, .Melee = 100, .Ranged = 100, .DefencePoints = 16, .FoodCost = 7.5, .CanCharge = True, .CanChase = True, .Flying = True},
            New UnitStats With {.Name = "Silver Dragon", .Power = 850, .HP = 650, .Melee = 130, .Ranged = 130, .DefencePoints = 18, .FoodCost = 9.0, .CanCharge = True, .CanChase = True, .Flying = True},
            New UnitStats With {.Name = "Gold Dragon", .Power = 1000, .HP = 800, .Melee = 140, .Ranged = 140, .DefencePoints = 20, .FoodCost = 11.0, .CanCharge = True, .CanChase = True, .Flying = True}
        }
        Dim dragonlordRoster As New RaceUnits With {
            .RaceName = "Dragonlord",   ' key used by SummonerFaction
            .Units = dragonlordSummonerUnits
        }
        AllRaces.Add(dragonlordRoster)


        ' === Mercenary Factions ===

        ' === Warrior Monks ===
        Dim warriorMonks As New RaceUnits With {
    .RaceName = "Warrior Monks",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Warrior Monk", .ShortName = "WarMon", .Power = 4, .HP = 9, .Melee = 3, .Ranged = 0, .DefencePoints = 3, .FoodCost = 1.0, .Type = UnitType.HeavyInfantry, .TrainingBonus = 0.5}
    }
}
        AllRaces.Add(warriorMonks)


        ' === Skulkrin ===
        Dim skulkrin As New RaceUnits With {
    .RaceName = "Skulkrin",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Skulkrin Scrapper", .ShortName = "SkuScr", .Power = 1, .HP = 5, .Melee = 1, .Ranged = 0, .DefencePoints = 1, .FoodCost = 0.1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Skulkrin Slinger", .ShortName = "SkuSli", .Power = 2, .HP = 4, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 0.1, .Type = UnitType.Archer}
    }
}
        AllRaces.Add(skulkrin)

        ' Barbarians
        Dim barbarians As New RaceUnits With {
    .RaceName = "Barbarians",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Barbarian Infantry", .ShortName = "BarInf", .Power = 2, .HP = 10, .Melee = 2, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Barbarian Berserker", .ShortName = "BarBer", .Power = 4, .HP = 15, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Barbarian Raider", .ShortName = "BarRaid", .Power = 5, .HP = 15, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .CanCharge = True, .CanChase = True, .Type = UnitType.LightCavalry}
    }
}
        AllRaces.Add(barbarians)

        ' Freeblades
        Dim freeblades As New RaceUnits With {
    .RaceName = "Freeblades",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Freeblade Skirmisher", .ShortName = "FreSkr", .Power = 2, .HP = 6, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightInfantry, .TrainingBonus = 0.1},
        New UnitStats With {.Name = "Freeblade Archer", .ShortName = "FreArc", .Power = 2, .HP = 5, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer, .TrainingBonus = 0.1},
        New UnitStats With {.Name = "Freeblade Rider", .ShortName = "FreRid", .Power = 3, .HP = 7, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .CanChase = True, .Type = UnitType.LightCavalry, .TrainingBonus = 0.1}
    }
}
        AllRaces.Add(freeblades)

        ' Sellswords
        Dim sellswords As New RaceUnits With {
    .RaceName = "Sellswords",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Sellsword", .ShortName = "SelSwo", .Power = 5, .HP = 8, .Melee = 3, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .TrainingBonus = 0.25}
    }
}
        AllRaces.Add(sellswords)

        ' Nomads
        Dim nomads As New RaceUnits With {
    .RaceName = "Nomads",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Nomad Rider", .ShortName = "NomRid", .Power = 3, .HP = 7, .Melee = 1, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .CanChase = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Nomad Horse Archer", .ShortName = "NomArc", .Power = 3, .HP = 6, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Nomad Heavy Rider", .ShortName = "NomHev", .Power = 10, .HP = 15, .Melee = 3, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .CanCharge = True, .Type = UnitType.HeavyCavalry}
    }
}
        AllRaces.Add(nomads)

        ' Bandits
        Dim bandits As New RaceUnits With {
    .RaceName = "Bandits",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Bandit", .ShortName = "Bandit", .Power = 2, .HP = 6, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Bandit Archer", .ShortName = "BanArc", .Power = 2, .HP = 5, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Bandit Rider", .ShortName = "BanRai", .Power = 3, .HP = 7, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .CanChase = True, .Type = UnitType.LightCavalry}
    }
}
        AllRaces.Add(bandits)


        Dim werecreatures As New RaceUnits With {
    .RaceName = "Werecreatures",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Wererat", .ShortName = "WerRat", .Power = 50, .HP = 12, .Melee = 4, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Werewolf", .ShortName = "WerWlf", .Power = 100, .HP = 25, .Melee = 6, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Weretiger", .ShortName = "WerTig", .Power = 150, .HP = 30, .Melee = 13, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .CanCharge = True, .CanChase = True},
        New UnitStats With {.Name = "Werebear", .ShortName = "WerBer", .Power = 200, .HP = 80, .Melee = 8, .Ranged = 0, .DefencePoints = 4, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .CanCharge = True}
    }
}
        AllRaces.Add(werecreatures)


        ' === Harpies & Hydra ===
        Dim harpies As New RaceUnits With {
    .RaceName = "Harpies",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Harpy", .ShortName = "Har", .Power = 2, .HP = 5, .Melee = 2, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry, .Flying = True, .CanChase = True},
        New UnitStats With {.Name = "Hydra", .ShortName = "Hyd", .Power = 700, .HP = 500, .Melee = 90, .Ranged = 0, .DefencePoints = 9, .FoodCost = 8, .Type = UnitType.HeavyInfantry, .CanCharge = True}
    }
}
        AllRaces.Add(harpies)

        ' === Cult ===
        Dim cult As New RaceUnits With {
    .RaceName = "Cultists",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Cultist", .ShortName = "Cul", .Power = 1, .HP = 3, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Grimlock", .ShortName = "Gri", .Power = 10, .HP = 12, .Melee = 3, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Umber Hulk", .ShortName = "Umb", .Power = 250, .HP = 180, .Melee = 25, .Ranged = 0, .DefencePoints = 8, .FoodCost = 4, .Type = UnitType.HeavyInfantry, .CanCharge = True, .CanChase = True},
        New UnitStats With {.Name = "Beholder", .ShortName = "Behold", .Power = 800, .HP = 550, .Melee = 70, .Ranged = 140, .DefencePoints = 11, .FoodCost = 6, .Type = UnitType.HeavyCavalry, .Flying = True, .CanChase = True}
    }
}
        AllRaces.Add(cult)

        ' === Demons ===
        Dim demons As New RaceUnits With {
    .RaceName = "Demons",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Dretch", .ShortName = "Dre", .Power = 1, .HP = 4, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Quasit", .ShortName = "Qua", .Power = 2, .HP = 5, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer, .Flying = True},
        New UnitStats With {.Name = "Vrock", .ShortName = "Vro", .Power = 50, .HP = 45, .Melee = 10, .Ranged = 0, .DefencePoints = 3, .FoodCost = 1, .Type = UnitType.LightCavalry, .Flying = True, .CanChase = True},
        New UnitStats With {.Name = "Hezrou", .ShortName = "Hez", .Power = 75, .HP = 80, .Melee = 15, .Ranged = 0, .DefencePoints = 5, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Glabrezu", .ShortName = "Gla", .Power = 200, .HP = 160, .Melee = 25, .Ranged = 0, .DefencePoints = 8, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .CanCharge = True, .CanChase = True},
        New UnitStats With {.Name = "Marilith", .ShortName = "Mar", .Power = 400, .HP = 220, .Melee = 40, .Ranged = 0, .DefencePoints = 10, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Balor", .ShortName = "Bal", .Power = 900, .HP = 600, .Melee = 120, .Ranged = 100, .DefencePoints = 12, .FoodCost = 1, .Type = UnitType.HeavyCavalry, .Flying = True, .CanChase = True, .CanCharge = True}
    }
}
        AllRaces.Add(demons)


        Dim Undead As New RaceUnits With {
    .RaceName = "Undead",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Zombie", .ShortName = "Zombie", .Power = 1, .HP = 5, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 0.1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Skeleton", .ShortName = "Skeleton", .Power = 4, .HP = 20, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .FoodCost = 0.1, .CanChase = True, .CanCharge = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Shadow", .ShortName = "Shadow", .Power = 6, .HP = 30, .Melee = 2, .Ranged = 0, .DefencePoints = 3, .FoodCost = 0.1, .Flying = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Ghost", .ShortName = "Ghost", .Power = 8, .HP = 40, .Melee = 3, .Ranged = 0, .DefencePoints = 4, .FoodCost = 0.1, .Flying = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Wight", .ShortName = "Wight", .Power = 9, .HP = 45, .Melee = 4, .Ranged = 0, .DefencePoints = 5, .FoodCost = 0.1, .CanChase = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Spectre", .ShortName = "Spectre", .Power = 12, .HP = 60, .Melee = 5, .Ranged = 0, .DefencePoints = 7, .FoodCost = 0.1, .Flying = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Wraith", .ShortName = "Wraith", .Power = 16, .HP = 80, .Melee = 7, .Ranged = 0, .DefencePoints = 10, .FoodCost = 0.1, .Flying = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Banshee", .ShortName = "Banshee", .Power = 20, .HP = 100, .Melee = 8, .Ranged = 2, .DefencePoints = 10, .FoodCost = 0.1, .Flying = True, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Ghoul", .ShortName = "Ghoul", .Power = 25, .HP = 90, .Melee = 8, .Ranged = 0, .DefencePoints = 10, .FoodCost = 0.1, .CanChase = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Bone Golem", .ShortName = "BnGolm", .Power = 36, .HP = 140, .Melee = 9, .Ranged = 0, .DefencePoints = 12, .FoodCost = 0.1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Vampire", .ShortName = "Vampire", .Power = 49, .HP = 180, .Melee = 15, .Ranged = 0, .DefencePoints = 13, .FoodCost = 0.1, .Flying = True, .Type = UnitType.HeavyCavalry},
        New UnitStats With {.Name = "Lich", .ShortName = "Lich", .Power = 64, .HP = 220, .Melee = 15, .Ranged = 8, .DefencePoints = 14, .FoodCost = 0.1, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Death Knight", .ShortName = "DthKnt", .Power = 81, .HP = 260, .Melee = 40, .Ranged = 0, .DefencePoints = 14, .FoodCost = 0.1, .CanCharge = True, .Type = UnitType.HeavyCavalry},
        New UnitStats With {.Name = "Bone Dragon", .ShortName = "BnDrgn", .Power = 100, .HP = 500, .Melee = 100, .Ranged = 100, .DefencePoints = 10, .FoodCost = 0.1, .CanChase = True, .CanCharge = True, .Flying = True, .Type = UnitType.HeavyCavalry}
    }
}
        AllRaces.Add(Undead)


        Dim Golems As New RaceUnits With {
    .RaceName = "Golems",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Mud Golem", .ShortName = "MudGol", .Power = 1, .HP = 10, .Melee = 1, .Ranged = 0, .DefencePoints = 1, .FoodCost = 0.1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Stone Golem", .ShortName = "StnGol", .Power = 5, .HP = 30, .Melee = 2, .Ranged = 0, .DefencePoints = 4, .FoodCost = 0.1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Iron Golem", .ShortName = "IrnGol", .Power = 10, .HP = 60, .Melee = 3, .Ranged = 0, .DefencePoints = 8, .FoodCost = 0.1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Flesh Golem", .ShortName = "FlhGol", .Power = 20, .HP = 70, .Melee = 4, .Ranged = 0, .DefencePoints = 5, .FoodCost = 0.1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Adamant Golem", .ShortName = "AdmGol", .Power = 25, .HP = 100, .Melee = 4, .Ranged = 0, .DefencePoints = 12, .FoodCost = 0.1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Crystal Golem", .ShortName = "CryGol", .Power = 30, .HP = 140, .Melee = 4, .Ranged = 2, .DefencePoints = 14, .FoodCost = 0.1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Obsidian Golem", .ShortName = "ObsGol", .Power = 40, .HP = 180, .Melee = 5, .Ranged = 0, .DefencePoints = 15, .FoodCost = 0.1, .Type = UnitType.HeavyInfantry}
    }
}
        AllRaces.Add(Golems)

        Dim Elementals As New RaceUnits With {
            .RaceName = "Elementals",
            .Units = New List(Of UnitStats) From {
                New UnitStats With {.Name = "Earth Elemental", .ShortName = "ErtEle", .Power = 300, .HP = 120, .Melee = 25, .Ranged = 0, .DefencePoints = 8, .FoodCost = 0.1, .CanCharge = True, .CanChase = True},
                New UnitStats With {.Name = "Air Elemental", .ShortName = "AirEle", .Power = 400, .HP = 140, .Melee = 30, .Ranged = 4, .DefencePoints = 5, .FoodCost = 0.1, .Flying = True, .CanCharge = True, .CanChase = True},
                New UnitStats With {.Name = "Fire Elemental", .ShortName = "FirEle", .Power = 500, .HP = 160, .Melee = 40, .Ranged = 4, .DefencePoints = 4, .FoodCost = 0.1, .CanCharge = True, .CanChase = True},
                New UnitStats With {.Name = "Water Elemental", .ShortName = "WatEle", .Power = 600, .HP = 180, .Melee = 45, .Ranged = 0, .DefencePoints = 6, .FoodCost = 0.1, .CanCharge = True, .CanChase = True}
            }
        }
        AllRaces.Add(Elementals)


    End Sub

    Public Sub InitializePlayers()
        ' Reset player list and used name tracker
        Players = New List(Of Player)()
        UsedArmyNames.Clear()

        ' Loop over 4 players
        For i As Integer = 0 To 3
            Dim p As New Player With {
            .PlayerNumber = i,
            .Race = Races(i),
            .Population = 5000,
            .Armies = New List(Of Army)(),
            .AIControlled = True,
            .CatchUpCooldown = 0
        }

            ' Determine starting corner center for this player
            Dim startPos As Point = GetStartingCornerCenter(i)

            ' Find the race units in consolidated AllRaces
            Dim raceUnits As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName = p.Race)
            If raceUnits Is Nothing Then
                'System.Diagnostics.Debug.WriteLine($"Warning: Race '{p.Race}' not found in AllRaces. Armies will start empty.")
                Players.Add(p)
                Continue For
            End If

            ' === Create 3 armies at starting corner ===
            For a As Integer = 1 To 3
                Dim army As New Army With {
                .X = startPos.X,
                .Y = startPos.Y,
                .Race = p.Race,
                .Name = GenerateArmyName(p.Race, a) ' <<< assign unique random name
            }

                ' Templates we’ll need
                Dim liTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.LightInfantry)
                Dim archerTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.Archer)
                Dim lcTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.LightCavalry)

                army.Units.Add(New Unit(liTemplate, army.Race, 200))
                army.Units.Add(New Unit(archerTemplate, army.Race, 200))
                army.Units.Add(New Unit(lcTemplate, army.Race, 100))

                ' Add army to player
                p.Armies.Add(army)

                ' Optional debug info
                'System.Diagnostics.Debug.WriteLine($"{p.Race}: Created {army.Name} at ({army.X},{army.Y}) with {army.TotalSoldiers} men.")
            Next

            Players.Add(p)
        Next
    End Sub


    Private Function GenerateArmyName(race As String, index As Integer) As String
        Dim rnd As New Random()
        Dim prefixes As List(Of String)
        Dim suffixes As List(Of String)

        Select Case race.ToLower()
            Case "elf"
                prefixes = New List(Of String) From {"Sylvan", "Emerald", "Silverwood", "Moonleaf", "Starbloom", "Whisperwind", "Verdant", "Dawn", "Twilight", "Glade"}
                suffixes = New List(Of String) From {"Legion", "Guard", "Host", "Sentinels", "Archers", "Riders", "Kin", "Blades", "Watch", "Company"}

            Case "dwarf"
                prefixes = New List(Of String) From {"Ironshield", "Stoneheart", "Deepforge", "Hammerfall", "Granite", "Mithril", "Oathbound", "Anvil", "Hearth", "Forgeguard"}
                suffixes = New List(Of String) From {"Cohort", "Phalanx", "Guard", "Legion", "Keepers", "Vanguard", "Company", "March", "Brigade", "Brotherhood"}

            Case "orc"
                prefixes = New List(Of String) From {"Bloodwolf", "Bonebreaker", "Fangrider", "Redmaw", "Ironjaw", "Skullcrush", "Gorefang", "Ashfang", "Wargborn", "Blacktooth"}
                suffixes = New List(Of String) From {"Pack", "Horde", "Clan", "Warband", "Marauders", "Tribe", "Raiders", "Warhost", "Mob", "Crushers"}

            Case "human"
                prefixes = New List(Of String) From {"Lionheart", "Silver", "Crimson", "Golden", "Stormborn", "Sunshield", "King’s", "Dawnward", "White Hart", "Freeblade"}
                suffixes = New List(Of String) From {"Battalion", "Banner", "Lancers", "Company", "Brigade", "Legion", "Spear", "Host", "Regiment", "Vanguard"}

            Case Else
                Return $"{race} Army {index}"
        End Select

        Dim finalName As String = Nothing
        Dim tries As Integer = 0

        Do
            Dim prefix = prefixes(rnd.Next(prefixes.Count))
            Dim suffix = suffixes(rnd.Next(suffixes.Count))
            finalName = $"{prefix} {suffix}"
            tries += 1
        Loop While UsedArmyNames.Contains(finalName) AndAlso tries < 100

        UsedArmyNames.Add(finalName)
        Return finalName
    End Function


#End Region

#Region "=== Process Turn ==="


    Public Sub ProcessTurn()
        Dim maxSteps As Integer = 5
        Dim mapSize As Integer = Map.GetLength(0)

        ' --- Cooldown tick (for catch-up recruiting) ---
        For Each p In Players
            If p.CatchUpCooldown > 0 Then p.CatchUpCooldown -= 1
        Next

        ' --- Movement + Special steps ---
        For stepIndex As Integer = 0 To maxSteps - 1
            For Each p In Players
                If p.Armies Is Nothing Then Continue For

                For Each a In p.Armies
                    ' Skip armies that retreated this turn
                    If a.RetreatedThisTurn Then Continue For

                    ' Fill AI move queue if empty
                    If p.AIControlled AndAlso a.MoveQueue.Count = 0 Then
                        Dim plannedMoves As List(Of ArmyCommand) = GenerateAIMoves(a, p, maxSteps)
                        a.MoveQueue.AddRange(plannedMoves)
                    End If

                    Dim isSpecial As Boolean = (stepIndex = maxSteps - 1)

                    ' ======================================================
                    ' === SPECIAL STEP (5th command: move or recruit) ======
                    ' ======================================================
                    If isSpecial Then
                        a.HasUsedSpecial = True

                        ' If there's no command at this special index, let AI add one now (includes baseline revive)
                        If p.AIControlled AndAlso stepIndex >= a.MoveQueue.Count Then
                            Dim aiArmyIndex As Integer = p.Armies.IndexOf(a)
                            If aiArmyIndex < 0 Then aiArmyIndex = 0
                            AIRecruitArmy(a, p, aiArmyIndex)
                        End If

                        ' Prefer command at this index; otherwise use the last one
                        Dim cmdToRun As ArmyCommand = Nothing
                        If stepIndex < a.MoveQueue.Count Then
                            cmdToRun = a.MoveQueue(stepIndex)
                        ElseIf a.MoveQueue.Count > 0 Then
                            cmdToRun = a.MoveQueue(a.MoveQueue.Count - 1)
                        End If

                        If cmdToRun IsNot Nothing Then
                            Dim moveCommand As String = If(cmdToRun.Command, "").ToUpper().Trim()

                            Select Case moveCommand
                            ' === Recruit command ===
                                Case "RECRUIT"
                                    'Debug.WriteLine($"[RECRUIT DEBUG] Player={p.Race}, Army={a.Name}, Param='{cmdToRun.Parameter}', Amt='{cmdToRun.Amount}'")

                                    Dim unitShort As String = If(cmdToRun.Parameter, "").Trim().ToLowerInvariant()
                                    Dim amtRaw As String = If(cmdToRun.Amount, "").Trim().ToUpperInvariant()

                                    ' Find unit template for THIS player's race by ShortName
                                    Dim ru As RaceUnits = Nothing
                                    For Each r In AllRaces
                                        If r IsNot Nothing AndAlso r.RaceName.Equals(p.Race, StringComparison.OrdinalIgnoreCase) Then
                                            ru = r : Exit For
                                        End If
                                    Next
                                    If ru Is Nothing Then Exit Select

                                    'Debug.WriteLine($"[RECRUIT DEBUG] Searching {ru.Units.Count} units for race {ru.RaceName}")
                                    For Each u In ru.Units
                                        'Debug.WriteLine($"    Checking sn='{u.ShortName}', name='{u.Name}'")
                                    Next
                                    'Debug.WriteLine($"[RECRUIT DEBUG] Done searching.")


                                    Dim tmpl As UnitStats = Nothing
                                    For Each u In ru.Units
                                        If u Is Nothing Then Continue For
                                        Dim sn As String = If(String.IsNullOrWhiteSpace(u.ShortName), u.Name.ToLowerInvariant(), u.ShortName.ToLowerInvariant())
                                        If sn = unitShort Then tmpl = u : Exit For
                                    Next
                                    If tmpl Is Nothing Then
                                        Console.WriteLine($"Recruit failed: unknown unit shortname '{unitShort}' for {p.Race}.")
                                        Exit Select
                                    End If

                                    If amtRaw = "" OrElse amtRaw = "MAX" Then
                                        RecruitArmyUnits(a, p, tmpl)
                                    Else
                                        Dim n As Integer
                                        If Integer.TryParse(amtRaw, n) AndAlso n > 0 Then
                                            RecruitArmyUnits(a, p, tmpl, n)
                                        Else
                                            Console.WriteLine($"Recruit failed: bad amount '{amtRaw}'.")
                                        End If
                                    End If

                            ' === Train command ===
                                Case "TRAIN"
                                    ' Army trains: gain +10% TrainingLevel
                                    a.TrainingLevel *= 1.1

                            ' === Stay still ===
                                Case "STAY"
                                ' No action

                            ' === Movement on 5th step ===
                                Case "N", "NE", "E", "SE", "S", "SW", "W", "NW"
                                    If a.TotalSoldiers < 500 Then Exit Select

                                    Dim offset As Point = DirectionToOffset(moveCommand)
                                    Dim newX As Integer = Math.Max(0, Math.Min(mapSize - 1, a.X + offset.X))
                                    Dim newY As Integer = Math.Max(0, Math.Min(mapSize - 1, a.Y + offset.Y))

                                    ' Identify owner of target tile before capture
                                    Dim targetOwner As Player = Nothing
                                    Dim targetOwnerId As Integer = Map(newX, newY, 1)
                                    If targetOwnerId >= 0 AndAlso targetOwnerId < Players.Count Then
                                        targetOwner = Players(targetOwnerId)
                                    End If

                                    ' Capture ownership if valid (orthogonal only)
                                    If Map(newX, newY, 1) <> p.PlayerNumber AndAlso IsCaptureValidOrthogonal(newX, newY, p.PlayerNumber) Then
                                        Map(newX, newY, 1) = p.PlayerNumber
                                        ' Orc slave capture trigger
                                        If p.Race.Equals("Orc", StringComparison.OrdinalIgnoreCase) Then
                                            TryCaptureSlaves(p, targetOwner)
                                        End If
                                    End If

                                    ' Move army
                                    a.X = newX
                                    a.Y = newY
                            End Select
                        End If

                        ' ======================================================
                        ' === NORMAL MOVEMENT STEPS (1–4) =====================
                        ' ======================================================
                    Else
                        If a.TotalSoldiers < 500 Then Continue For

                        If stepIndex < a.MoveQueue.Count Then
                            Dim cmd As ArmyCommand = a.MoveQueue(stepIndex)
                            Dim offset As Point = DirectionToOffset(If(cmd.Command, ""))
                            Dim newX As Integer = Math.Max(0, Math.Min(mapSize - 1, a.X + offset.X))
                            Dim newY As Integer = Math.Max(0, Math.Min(mapSize - 1, a.Y + offset.Y))

                            ' Identify owner of target tile before capture
                            Dim targetOwner As Player = Nothing
                            Dim targetOwnerId As Integer = Map(newX, newY, 1)
                            If targetOwnerId >= 0 AndAlso targetOwnerId < Players.Count Then
                                targetOwner = Players(targetOwnerId)
                            End If

                            ' Capture ownership if valid (orthogonal only)
                            If Map(newX, newY, 1) <> p.PlayerNumber AndAlso IsCaptureValidOrthogonal(newX, newY, p.PlayerNumber) Then
                                Map(newX, newY, 1) = p.PlayerNumber
                                ' Orc slave capture trigger
                                If p.Race.Equals("Orc", StringComparison.OrdinalIgnoreCase) Then
                                    TryCaptureSlaves(p, targetOwner)
                                End If
                            End If

                            ' Move army
                            a.X = newX
                            a.Y = newY
                        End If
                    End If
                Next
            Next

            ' Resolve combat after each move step
            ResolveCombat()
        Next

        ' --- Clean up ---
        For Each p In Players
            If p.Armies Is Nothing Then Continue For
            For Each a In p.Armies
                a.MoveQueue.Clear()
                a.HasUsedSpecial = False
                a.RetreatedThisTurn = False
            Next
        Next
    End Sub

    Private Sub ParseUnitCosts(t As UnitStats, ByRef iron As Integer, ByRef wood As Integer, ByRef mounts As Integer, ByRef mithril As Integer)
        iron = 0 : wood = 0 : mounts = 0 : mithril = 0
        If t Is Nothing OrElse String.IsNullOrWhiteSpace(t.Cost) Then Exit Sub

        For Each rawPart In t.Cost.Split(","c)
            Dim part As String = rawPart.Trim()
            If part = "" Then Continue For

            Dim lower As String = part.ToLowerInvariant()
            Dim n As Integer = 1
            Dim idx As Integer = part.IndexOf(":"c)
            If idx >= 0 Then Integer.TryParse(part.Substring(idx + 1).Trim(), n)

            If lower.StartsWith("i") OrElse lower.Contains("iron") Then
                iron += Math.Max(0, n)
            ElseIf lower.StartsWith("w") OrElse lower.Contains("wood") Then
                wood += Math.Max(0, n)
            ElseIf lower.StartsWith("m") OrElse lower.Contains("mount") OrElse lower.Contains("horse") OrElse lower.Contains("wolf") OrElse lower.Contains("elk") OrElse lower.Contains("ram") Then
                mounts += Math.Max(0, n)
            ElseIf lower.StartsWith("x") OrElse lower.Contains("mith") Then
                mithril += Math.Max(0, n)
            End If
        Next
    End Sub



    Private Sub TryCaptureSlaves(orcPlayer As Player, targetTileOwner As Player)
        ' Skip if not orc
        If Not orcPlayer.Race.Equals("Orc", StringComparison.OrdinalIgnoreCase) Then Exit Sub
        ' Skip if empty tile or already orc-owned
        If targetTileOwner Is Nothing OrElse targetTileOwner Is orcPlayer Then Exit Sub

        ' Slaves captured = 1 per 1000 population (rounded down)
        Dim slavesToCapture As Integer = CInt(Math.Floor(targetTileOwner.Population / 1000.0R))
        If slavesToCapture <= 0 Then Exit Sub

        ' Ensure target keeps at least 1 population
        If targetTileOwner.Population - slavesToCapture < 1 Then
            slavesToCapture = targetTileOwner.Population - 1
        End If
        If slavesToCapture <= 0 Then Exit Sub

        ' Apply population loss
        targetTileOwner.Population -= slavesToCapture

        ' Add to correct slave pool
        Select Case targetTileOwner.Race
            Case "Elf"
                orcPlayer.ElfSlaves += slavesToCapture
            Case "Dwarf"
                orcPlayer.DwarfSlaves += slavesToCapture
            Case "Human"
                orcPlayer.HumanSlaves += slavesToCapture
        End Select

        'Debug.WriteLine($"[SLAVES] Orcs captured {slavesToCapture} {targetTileOwner.Race.ToLower()} slaves. Remaining {targetTileOwner.Race} pop: {targetTileOwner.Population}")
    End Sub

    ' === Unified RecruitArmyUnits: supports both AI/MAX and player-entered numbers ===
    Public Sub RecruitArmyUnits(army As Army, player As Player, unitTemplate As UnitStats, Optional requestedAmount As Integer = -1)
        Console.WriteLine($"DEBUG RecruitArmyUnits called with requestedAmount={requestedAmount}")

        If army Is Nothing OrElse player Is Nothing OrElse unitTemplate Is Nothing Then Exit Sub

        Dim recruitAmount As Integer

        ' Always calculate the 5% population cap first
        Dim maxPerTurn As Integer = Math.Max(1, CInt(Math.Floor(player.Population * 0.05)))

        If requestedAmount > 0 Then
            ' Player manually entered a number — must respect 5% population cap
            recruitAmount = Math.Min(requestedAmount, maxPerTurn)
        Else
            ' AI or "MAX" order
            recruitAmount = maxPerTurn
        End If

        ' --- 2. Parse cost string robustly: supports "I:2, W:1, M:1, X:1" and textual fallbacks ---
        Dim ironRequired As Integer = 0
        Dim woodRequired As Integer = 0
        Dim mountsRequired As Integer = 0
        Dim mithrilRequired As Integer = 0

        If Not String.IsNullOrWhiteSpace(unitTemplate.Cost) Then
            For Each rawPart In unitTemplate.Cost.Split(","c)
                Dim part As String = rawPart.Trim()
                If part.Length = 0 Then Continue For

                Dim lower As String = part.ToLowerInvariant()
                Dim n As Integer
                Dim idx = part.IndexOf(":"c)
                Dim qty As Integer = 1
                If idx >= 0 AndAlso Integer.TryParse(part.Substring(idx + 1).Trim(), n) Then qty = Math.Max(1, n)

                ' --- Match by first letter or word fragment ---
                If lower.StartsWith("i") OrElse lower.Contains("iron") Then
                    ironRequired += qty

                ElseIf lower.StartsWith("w") OrElse lower.Contains("wood") Then
                    woodRequired += qty

                ElseIf lower.StartsWith("m") OrElse lower.Contains("mount") OrElse lower.Contains("horse") OrElse
                 lower.Contains("wolf") OrElse lower.Contains("elk") OrElse lower.Contains("ram") Then
                    mountsRequired += qty

                ElseIf lower.StartsWith("x") OrElse lower.Contains("mith") Then
                    mithrilRequired += qty
                End If
            Next
        End If

        ' === Catch shorthand like "M:1" ===
        If mountsRequired = 0 AndAlso unitTemplate.Cost.ToUpperInvariant().Contains("M:") Then mountsRequired = 1

        ' --- 3. Limit by available resources (hard caps) ---
        If ironRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Iron \ ironRequired)
        If woodRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Wood \ woodRequired)
        If mountsRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Mounts \ mountsRequired)
        If mithrilRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Mithril \ mithrilRequired)

        ' --- 4. Check viability ---
        If recruitAmount <= 0 Then
            Console.WriteLine($"{player.Race} wanted to recruit {unitTemplate.Name} but lacked resources/pop.")
            If player.AIControlled Then TryRecruitMilitia(player, army)
            Exit Sub
        End If

        ' --- 5. Tiny batch rule (AI only) ---
        If recruitAmount < 50 Then
            If player.AIControlled Then
                Console.WriteLine($"{player.Race} skipped tiny batch ({recruitAmount} {unitTemplate.Name}) and raised militia instead.")
                TryRecruitMilitia(player, army)
                Exit Sub
            Else
                Console.WriteLine($"{player.Race} recruits small batch ({recruitAmount} {unitTemplate.Name}) due to limited resources.")
            End If
        End If

        ' --- 6. Check if unit already exists in army ---
        Dim existingUnit As Unit = army.Units.FirstOrDefault(Function(u) u.Name = unitTemplate.Name AndAlso u.Type = unitTemplate.Type)

        If existingUnit IsNot Nothing Then
            existingUnit.Size += recruitAmount
            If existingUnit.FoodCost < 0 Then existingUnit.FoodCost = 1.0
        Else
            Dim newUnit As New Unit(unitTemplate, army.Race, recruitAmount)
            If newUnit.FoodCost < 0 Then newUnit.FoodCost = 1.0
            army.Units.Add(newUnit)
        End If

        ' --- 7. Deduct population and resources ---
        player.Population -= recruitAmount
        If ironRequired > 0 Then player.Iron -= ironRequired * recruitAmount
        If woodRequired > 0 Then player.Wood -= woodRequired * recruitAmount
        If mountsRequired > 0 Then player.Mounts -= mountsRequired * recruitAmount
        If mithrilRequired > 0 Then player.Mithril -= mithrilRequired * recruitAmount

        ' --- 8. Safety clamps (no negatives) ---
        player.Population = Math.Max(player.Population, 0)
        player.Iron = Math.Max(player.Iron, 0)
        player.Wood = Math.Max(player.Wood, 0)
        player.Mounts = Math.Max(player.Mounts, 0)
        player.Mithril = Math.Max(player.Mithril, 0)

        ' --- 8.5 Training dilution ---
        Dim oldMen As Integer = army.TotalSoldiers - recruitAmount
        Dim newMen As Integer = recruitAmount

        If oldMen > 0 AndAlso newMen > 0 Then
            Dim totalMen As Integer = oldMen + newMen
            ' Weighted average: new recruits start at 0% training
            army.TrainingLevel = (army.TrainingLevel * oldMen) / totalMen
        ElseIf oldMen = 0 AndAlso newMen > 0 Then
            ' Newly formed army starts untrained
            army.TrainingLevel = 0
        End If


        ' --- 9. Log outcome ---
        Dim mode As String = If(requestedAmount > 0, $"requested {requestedAmount}", "auto")
        Console.WriteLine($"{player.Race} recruits {recruitAmount} {unitTemplate.Name} in army at ({army.X},{army.Y}) [{mode}]  " &
                      $"[I:{ironRequired}, W:{woodRequired}, M:{mountsRequired}, X:{mithrilRequired}]  Mithril left={player.Mithril}")
    End Sub



    Private Sub TryRecruitMilitia(player As Player, army As Army)
        If player Is Nothing OrElse player.IsEliminated OrElse army Is Nothing Then Exit Sub
        If player Is Nothing OrElse army Is Nothing Then Exit Sub
        If player.Population < 10 Then Exit Sub

        ' --- Same 5% rule as normal recruitment ---
        Dim maxPerTurn As Integer = Math.Max(1, CInt(Math.Floor(player.Population * 0.05)))
        Dim recruitAmount As Integer = Math.Min(maxPerTurn, player.Population)

        ' --- Race-flavoured naming ---
        Dim unitName As String = ""
        Select Case player.Race.ToLower()
            Case "elf" : unitName = "Elf Recruits"
            Case "dwarf" : unitName = "Dwarf Militia"
            Case "orc" : unitName = "Orc Rabble"
            Case "human" : unitName = "Peasants"
            Case Else : unitName = "Militia"
        End Select

        ' --- Basic, terrible stats ---
        Dim militiaStats As New UnitStats With {
        .Name = unitName,
        .ShortName = "Militia",
        .HP = 2,
        .Melee = 1,
        .Ranged = 0,
        .Power = 0,                 ' irrelevant for army units
        .DefencePoints = 0,
        .Type = UnitType.LightInfantry,
        .FoodCost = 1.0,
        .CanCharge = False,
        .CanChase = False,
        .Flying = False
    }

        ' --- Deduct population ---
        player.Population -= recruitAmount
        If player.Population < 0 Then player.Population = 0

        ' --- Merge if existing militia already in army ---
        Dim existingMilitia = army.Units.FirstOrDefault(Function(u) u.Name = unitName)
        If existingMilitia IsNot Nothing Then
            existingMilitia.Size += recruitAmount
        Else
            Dim militiaUnit As New Unit(militiaStats, player.Race, recruitAmount)
            army.Units.Add(militiaUnit)
        End If

        Console.WriteLine($"[AI-FALLBACK] {player.Race} raises {recruitAmount} {unitName} (no resources). Pop now {player.Population}.")
    End Sub

    ' === NEW: AI movement with defend/assault intent (AI-only) ===========================
    Private Function GenerateAIMoves(army As Army, player As Player, maxSteps As Integer) As List(Of ArmyCommand)
        Dim moves As New List(Of ArmyCommand)()
        If player Is Nothing OrElse army Is Nothing Then Return moves
        If player.IsEliminated Then Return moves

        ' === ABSOLUTE SAFETY GUARD ===
        ' Never run this logic for human-controlled players
        If Not player.AIControlled Then Return moves

        ' === SPECIAL DEFENSIVE BEHAVIOUR: Third army defends the citadel ===
        Dim myIndex As Integer = player.Armies.IndexOf(army)
        If myIndex = 2 Then  ' third army in the list
            Dim citadelPos As Point = GetStartingCornerCenter(player.PlayerNumber)
            Dim enemyCloser As Boolean = False
            Dim nearestEnemy As Army = Nothing
            Dim nearestDist As Integer = Integer.MaxValue
            Dim localForcedTarget As Nullable(Of Point) = Nothing

            ' Check if any enemy army is closer to our capital than we are
            For Each opp In Players
                If opp Is Nothing OrElse opp Is player OrElse opp.IsEliminated Then Continue For
                If opp.Armies Is Nothing Then Continue For

                For Each ea In opp.Armies
                    If ea Is Nothing OrElse ea.TotalSoldiers < 500 Then Continue For
                    Dim dEnemyToHome As Integer = Manhattan(ea.X, ea.Y, citadelPos.X, citadelPos.Y)
                    Dim dUsToHome As Integer = Manhattan(army.X, army.Y, citadelPos.X, citadelPos.Y)
                    If dEnemyToHome < dUsToHome Then
                        enemyCloser = True
                        nearestEnemy = ea
                        nearestDist = dEnemyToHome
                    End If
                Next
            Next

            ' Defensive movement logic
            If enemyCloser AndAlso nearestEnemy IsNot Nothing Then
                localForcedTarget = citadelPos
            Else
                Dim distHome As Integer = Manhattan(army.X, army.Y, citadelPos.X, citadelPos.Y)
                If distHome > 2 Then localForcedTarget = citadelPos
            End If

            ' Recruitment check: if weaker than nearby enemies, recruit
            Dim strongestEnemyNearby As Integer = 0
            For Each opp In Players
                If opp Is Nothing OrElse opp Is player OrElse opp.IsEliminated Then Continue For
                If opp.Armies Is Nothing Then Continue For

                For Each ea In opp.Armies
                    If ea Is Nothing Then Continue For
                    Dim d As Integer = Manhattan(ea.X, ea.Y, citadelPos.X, citadelPos.Y)
                    If d < 12 Then
                        strongestEnemyNearby = Math.Max(strongestEnemyNearby, ea.TotalSoldiers)
                    End If
                Next
            Next

            If army.TotalSoldiers < strongestEnemyNearby Then
                ' Recruit only for AI players
                AIRecruitArmy(army, player, myIndex)
            End If

            ' If we’ve decided to move home, stay there
            If localForcedTarget.HasValue Then
                moves.Add(New ArmyCommand With {.Command = "Stay", .Parameter = ""})
            End If

            ' Return early — this army does not perform exploration or assault logic
            Return moves
        End If


        ' === GENERAL AI MOVEMENT ===
        If army.TotalSoldiers < 500 Then Return moves

        Dim mapSize As Integer = Map.GetLength(0)
        Dim homePos As Point = GetStartingCornerCenter(player.PlayerNumber)
        Dim currentPos As New Point(army.X, army.Y)

        ' --- Tactical intents ---
        Const DEFENSE_RADIUS As Integer = 8
        Const ASSAULT_RADIUS As Integer = 6

        Dim forcedTarget As Nullable(Of Point) = Nothing

        ' 1) DEFEND (highest priority)
        If IsEnemyThreateningHome(player, DEFENSE_RADIUS) Then
            forcedTarget = homePos
        Else
            ' 2) GLOBAL ASSAULT — if we’re much stronger, march on enemy capital
            Dim strongTarget As Nullable(Of Point) = GetGlobalAssaultTarget(player, army, 2.0)
            If strongTarget.HasValue Then
                forcedTarget = strongTarget
            Else
                ' 3) LOCAL ASSAULT — if near an enemy capital
                Dim assault As Nullable(Of Point) = NearestEnemyCapitalWithinRadius(player, army, ASSAULT_RADIUS)
                If assault.HasValue Then forcedTarget = assault
            End If
        End If

        ' === Pathfinding & tactical step generation ===
        For stepIndex As Integer = 0 To Math.Max(0, maxSteps - 2)

            ' --- Intent-guided move ---
            Dim pickedFromIntent As Boolean = False
            If forcedTarget.HasValue Then
                Dim bestIntent As Nullable(Of Point) = Nothing
                Dim bestDist As Integer = Integer.MaxValue
                Dim adjOffsets As Point() = {New Point(0, -1), New Point(1, 0), New Point(0, 1), New Point(-1, 0)}

                For Each off In adjOffsets
                    Dim nx = currentPos.X + off.X
                    Dim ny = currentPos.Y + off.Y
                    If nx < 0 OrElse nx >= mapSize OrElse ny < 0 OrElse ny >= mapSize Then Continue For

                    ' Skip occupied active tiles
                    Dim blockingArmy As Army = Players.SelectMany(Function(pp) If(pp?.Armies, Enumerable.Empty(Of Army)())) _
                    .FirstOrDefault(Function(a) a IsNot Nothing AndAlso a.X = nx AndAlso a.Y = ny AndAlso a.TotalSoldiers >= 500)
                    If blockingArmy IsNot Nothing Then Continue For

                    Dim dAfter As Integer = Manhattan(nx, ny, forcedTarget.Value.X, forcedTarget.Value.Y)
                    Dim dNow As Integer = Manhattan(currentPos.X, currentPos.Y, forcedTarget.Value.X, forcedTarget.Value.Y)
                    If dAfter < dNow AndAlso dAfter < bestDist Then
                        bestDist = dAfter
                        bestIntent = New Point(nx, ny)
                    End If
                Next

                If bestIntent.HasValue Then
                    Dim dx As Integer = Math.Sign(bestIntent.Value.X - currentPos.X)
                    Dim dy As Integer = Math.Sign(bestIntent.Value.Y - currentPos.Y)
                    Dim direction As String = ""

                    If Math.Abs(dx) >= Math.Abs(dy) AndAlso dx <> 0 Then
                        direction = If(dx > 0, "E", "W")
                    ElseIf dy <> 0 Then
                        direction = If(dy > 0, "S", "N")
                    End If

                    If direction <> "" Then
                        moves.Add(New ArmyCommand With {.Command = direction, .Parameter = ""})
                        currentPos.X += dx
                        currentPos.Y += dy
                        pickedFromIntent = True
                    End If
                End If
            End If

            If pickedFromIntent Then Continue For

            ' --- Exploration / expansion heuristic ---
            Dim bestTile As Nullable(Of Point) = Nothing
            Dim bestScore As Double = Double.MinValue
            Dim adjOffsets2 As Point() = {New Point(0, -1), New Point(1, 0), New Point(0, 1), New Point(-1, 0)}

            For Each dirOffset In adjOffsets2
                Dim nx As Integer = currentPos.X + dirOffset.X
                Dim ny As Integer = currentPos.Y + dirOffset.Y
                If nx < 0 OrElse nx >= mapSize OrElse ny < 0 OrElse ny >= mapSize Then Continue For

                ' Skip blocked by active army
                Dim blockingArmy As Army = Players.SelectMany(Function(pp) If(pp?.Armies, Enumerable.Empty(Of Army)())) _
                .FirstOrDefault(Function(a) a IsNot Nothing AndAlso a.X = nx AndAlso a.Y = ny AndAlso a.TotalSoldiers >= 500)
                If blockingArmy IsNot Nothing Then Continue For

                ' Skip already-owned tiles
                If Map(nx, ny, 1) = player.PlayerNumber Then Continue For

                Dim score As Double = 100

                ' Terrain bias
                Select Case player.Race.ToLower()
                    Case "elf" : If Map(nx, ny, 0) = 1 Then score += 10
                    Case "dwarf" : If Map(nx, ny, 0) = 3 Then score += 10
                    Case "orc" : If Map(nx, ny, 0) = 2 Then score += 10
                    Case "human" : If Map(nx, ny, 0) = 0 Then score += 10
                End Select

                ' Home-bias
                Dim distHome As Integer = Math.Abs(nx - homePos.X) + Math.Abs(ny - homePos.Y)
                score += Math.Max(0, 20 - distHome)

                ' Nudge toward intent
                If forcedTarget.HasValue Then
                    Dim dNow = Manhattan(currentPos.X, currentPos.Y, forcedTarget.Value.X, forcedTarget.Value.Y)
                    Dim dAfter = Manhattan(nx, ny, forcedTarget.Value.X, forcedTarget.Value.Y)
                    score += Math.Max(0, dNow - dAfter) * 2
                End If

                If score > bestScore Then
                    bestScore = score
                    bestTile = New Point(nx, ny)
                End If
            Next

            ' --- BFS fallback ---
            If Not bestTile.HasValue Then
                Dim bfsQueue As New Queue(Of Point)
                Dim visited(mapSize - 1, mapSize - 1) As Boolean
                bfsQueue.Enqueue(currentPos)
                visited(currentPos.X, currentPos.Y) = True

                While bfsQueue.Count > 0
                    Dim pt As Point = bfsQueue.Dequeue()
                    If Math.Abs(pt.X - currentPos.X) > 5 OrElse Math.Abs(pt.Y - currentPos.Y) > 5 Then Continue While

                    For Each dirOffset In adjOffsets2
                        Dim nx As Integer = pt.X + dirOffset.X
                        Dim ny As Integer = pt.Y + dirOffset.Y
                        If nx < 0 OrElse nx >= mapSize OrElse ny < 0 OrElse ny >= mapSize Then Continue For
                        If visited(nx, ny) Then Continue For
                        visited(nx, ny) = True

                        Dim blockingArmy As Army = Players.SelectMany(Function(pp) If(pp?.Armies, Enumerable.Empty(Of Army)())) _
                        .FirstOrDefault(Function(a) a IsNot Nothing AndAlso a.X = nx AndAlso a.Y = ny AndAlso a.TotalSoldiers >= 500)
                        If blockingArmy IsNot Nothing Then Continue For
                        If Map(nx, ny, 1) = player.PlayerNumber Then Continue For

                        Dim score As Double = 0
                        Dim distHome As Integer = Math.Abs(nx - homePos.X) + Math.Abs(ny - homePos.Y)
                        score += Math.Max(0, 50 - distHome)

                        Select Case player.Race.ToLower()
                            Case "elf" : If Map(nx, ny, 0) = 1 Then score += 5
                            Case "dwarf" : If Map(nx, ny, 0) = 3 Then score += 5
                            Case "orc" : If Map(nx, ny, 0) = 2 Then score += 5
                            Case "human" : If Map(nx, ny, 0) = 0 Then score += 5
                        End Select

                        If forcedTarget.HasValue Then
                            Dim dNow = Manhattan(currentPos.X, currentPos.Y, forcedTarget.Value.X, forcedTarget.Value.Y)
                            Dim dAfter = Manhattan(nx, ny, forcedTarget.Value.X, forcedTarget.Value.Y)
                            score += Math.Max(0, dNow - dAfter)
                        End If

                        If score > bestScore Then
                            bestScore = score
                            bestTile = New Point(nx, ny)
                        End If

                        bfsQueue.Enqueue(New Point(nx, ny))
                    Next
                End While
            End If

            ' --- Fallback to nearest living enemy capital ---
            If Not bestTile.HasValue Then
                Dim livingEnemies As New List(Of Player)
                For Each e As Player In Players
                    If e Is Nothing Then Continue For
                    If e.PlayerNumber = player.PlayerNumber Then Continue For
                    If e.IsEliminated Then Continue For
                    If capitals.ContainsKey(e.PlayerNumber) Then livingEnemies.Add(e)
                Next

                If livingEnemies.Count > 0 Then
                    Dim targetEnemy As Player = Nothing
                    Dim minDist As Integer = Integer.MaxValue
                    For Each e As Player In livingEnemies
                        Dim capPt As Point = capitals(e.PlayerNumber)
                        Dim d As Integer = Manhattan(currentPos.X, currentPos.Y, capPt.X, capPt.Y)
                        If d < minDist Then
                            minDist = d
                            targetEnemy = e
                        End If
                    Next

                    If targetEnemy IsNot Nothing Then
                        Dim cap As Point = capitals(targetEnemy.PlayerNumber)
                        bestTile = cap
                    End If
                Else
                    bestTile = New Point(mapSize \ 2, mapSize \ 2)
                End If
            End If

            ' --- Convert bestTile to single orthogonal move ---
            If Not bestTile.HasValue Then Exit For
            Dim dx2 As Integer = bestTile.Value.X - currentPos.X
            Dim dy2 As Integer = bestTile.Value.Y - currentPos.Y
            Dim moveX As Integer = 0, moveY As Integer = 0

            If Math.Abs(dx2) >= Math.Abs(dy2) AndAlso dx2 <> 0 Then
                moveX = Math.Sign(dx2)
            ElseIf dy2 <> 0 Then
                moveY = Math.Sign(dy2)
            End If

            Dim direction2 As String = ""
            If moveX <> 0 Then
                direction2 = If(moveX > 0, "E", "W")
            ElseIf moveY <> 0 Then
                direction2 = If(moveY > 0, "S", "N")
            End If

            If direction2 <> "" Then
                moves.Add(New ArmyCommand With {.Command = direction2, .Parameter = ""})
                currentPos.X += moveX
                currentPos.Y += moveY
            Else
                Exit For
            End If
        Next

        Return moves
    End Function


    ' === Helpers (put near your other helpers) =================================
    Private Function Manhattan(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer) As Integer
        Return Math.Abs(x1 - x2) + Math.Abs(y1 - y2)
    End Function

    Private Function IsEnemyThreateningHome(p As Player, radius As Integer) As Boolean
        If p Is Nothing Then Return False
        Dim home As Point = GetStartingCornerCenter(p.PlayerNumber)

        For Each opp In Players
            If opp Is Nothing OrElse opp Is p OrElse opp.IsEliminated Then Continue For
            If opp.Armies Is Nothing Then Continue For

            For Each a In opp.Armies
                If a Is Nothing OrElse a.TotalSoldiers < 500 Then Continue For
                If Manhattan(a.X, a.Y, home.X, home.Y) <= radius Then
                    Return True
                End If
            Next
        Next
        Return False
    End Function

    Private Function NearestEnemyCapitalWithinRadius(p As Player, a As Army, radius As Integer) As Nullable(Of Point)
        If p Is Nothing OrElse a Is Nothing Then Return Nothing

        Dim best As Nullable(Of Point) = Nothing
        Dim bestDist As Integer = Integer.MaxValue

        For Each opp In Players
            If opp Is Nothing OrElse opp Is p OrElse opp.IsEliminated Then Continue For

            Dim enemyCap As Point = GetStartingCornerCenter(opp.PlayerNumber)
            Dim d As Integer = Manhattan(a.X, a.Y, enemyCap.X, enemyCap.Y)
            If d <= radius AndAlso d < bestDist Then
                bestDist = d
                best = enemyCap
            End If
        Next

        Return best
    End Function


    Private Function GetGlobalAssaultTarget(p As Player, a As Army, strengthRatio As Double) As Nullable(Of Point)
        ' Looks across the entire map for a weaker enemy, ignoring distance
        If p Is Nothing OrElse a Is Nothing Then Return Nothing

        Dim myPower As Double = a.TotalSoldiers
        Dim bestTarget As Nullable(Of Point) = Nothing
        Dim weakestEnemyPower As Double = Double.MaxValue

        For Each opp In Players
            If opp Is Nothing OrElse opp Is p OrElse opp.IsEliminated Then Continue For

            ' Estimate total enemy power (sum of all armies)
            Dim totalEnemyPower As Double = 0
            If opp.Armies IsNot Nothing Then
                For Each ea In opp.Armies
                    If ea Is Nothing Then Continue For
                    totalEnemyPower += ea.TotalSoldiers
                Next
            End If

            ' If we significantly overpower this enemy, target their capital
            If totalEnemyPower > 0 AndAlso myPower >= totalEnemyPower * strengthRatio Then
                If totalEnemyPower < weakestEnemyPower Then
                    weakestEnemyPower = totalEnemyPower
                    bestTarget = GetStartingCornerCenter(opp.PlayerNumber)
                End If
            End If
        Next

        Return bestTarget
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


    Private Function GetStartingCornerCenter(playerNumber As Integer) As Point
        Select Case playerNumber
            Case 0 : Return New Point(2, 2)          ' Elf, top-left 5x5 block (1..5)
            Case 1 : Return New Point(22, 2)         ' Dwarf, top-right 5x5 block (21..25)
            Case 2 : Return New Point(2, 22)         ' Orc, bottom-left 5x5 block (1..5)
            Case 3 : Return New Point(22, 22)        ' Human, bottom-right 5x5 block (21..25)
            Case Else : Return New Point(0, 0)
        End Select
    End Function

    Private Function GetCombatGrid(a As Army) As List(Of Point)
        Dim points As New List(Of Point)
        Dim maxX As Integer = Map.GetLength(0) - 1
        Dim maxY As Integer = Map.GetLength(1) - 1
        For dx As Integer = -2 To 2
            For dy As Integer = -2 To 2
                Dim x = a.X + dx
                Dim y = a.Y + dy
                If x >= 0 AndAlso x <= maxX AndAlso y >= 0 AndAlso y <= maxY Then
                    points.Add(New Point(x, y))
                End If
            Next
        Next
        Return points
    End Function



#End Region

    ' ===========================================================
    ' === SAFE VERSION (for combat phase) =======================
    ' ===========================================================
    Private Sub QueueEliminationChecksSafe()
        If Players Is Nothing Then Exit Sub

        ' Collect defeated players first (don’t modify inside this loop)
        Dim defeated As New List(Of Player)

        For Each p In Players.ToList()   ' snapshot of current players
            If p Is Nothing Then Continue For

            ' Reuse existing logic but suppress direct cleanup here
            If ShouldBeEliminated(p) Then
                defeated.Add(p)
            End If
        Next

        ' Now safely apply defeat to those queued
        For Each p In defeated
            ApplyElimination(p)
        Next
    End Sub


    ' ===========================================================
    ' === ORIGINAL VERSION (for start of turn) ==================
    ' ===========================================================
    Private Sub RecomputeEliminationForAllPlayers()
        If Players Is Nothing Then Exit Sub
        For Each p In Players
            CheckAndApplyDefeat(p)
        Next
    End Sub


    ' ===========================================================
    ' === HELPER: check defeat condition only (no mutation) =====
    ' ===========================================================
    Private Function ShouldBeEliminated(p As Player) As Boolean
        If p Is Nothing OrElse p.IsEliminated Then Return False
        If capitals Is Nothing OrElse Not capitals.ContainsKey(p.PlayerNumber) Then Return False

        Dim citadel As Point = capitals(p.PlayerNumber)
        Dim cx As Integer = citadel.X
        Dim cy As Integer = citadel.Y

        ' === Check if enemy sits on citadel ===
        Dim enemyPresent As Boolean = False
        For Each q In Players
            If q Is Nothing OrElse q.PlayerNumber = p.PlayerNumber Then Continue For
            If q.Armies Is Nothing Then Continue For
            For Each a In q.Armies
                If a Is Nothing Then Continue For
                If a.X = cx AndAlso a.Y = cy AndAlso a.TotalSoldiers > 0 Then
                    enemyPresent = True
                    Exit For
                End If
            Next
            If enemyPresent Then Exit For
        Next

        ' === Check garrison (exclude armies that retreated this turn) ===
        Dim garrison As Integer = 0
        If p.Armies IsNot Nothing Then
            For Each a In p.Armies
                If a Is Nothing Then Continue For
                If a.X = cx AndAlso a.Y = cy AndAlso a.TotalSoldiers > 0 AndAlso Not a.RetreatedThisTurn Then
                    garrison += a.TotalSoldiers
                End If
            Next
        End If

        ' === Return only the boolean, no cleanup ===
        Return (enemyPresent AndAlso garrison <= 0)
    End Function

    ' ===========================================================
    ' === HELPER: actually eliminate after combat ===============
    ' ===========================================================
    Private Sub ApplyElimination(p As Player)
        If p Is Nothing OrElse p.IsEliminated Then Exit Sub

        p.IsEliminated = True
        Dim citadel As Point = capitals(p.PlayerNumber)
        Dim cx As Integer = citadel.X
        Dim cy As Integer = citadel.Y

        'rtbGameInfo.AppendText($"[DEFEAT] {p.Race} (Player {p.PlayerNumber + 1}) eliminated. Citadel captured at ({cx},{cy})." & vbCrLf)

        ' Clean up map and armies
        NeutralizePlayerMap(p)
        NeutralizePlayerArmies(p)

        ' Reset stats
        p.Population = 0
        p.FoodCollectedThisTurn = 0
        p.WoodCollectedThisTurn = 0
        p.IronCollectedThisTurn = 0
        p.MountsCollectedThisTurn = 0
        p.MithrilCollectedThisTurn = 0
    End Sub


    Private Sub CheckAndApplyDefeat(p As Player)
        If p Is Nothing Then Exit Sub
        If p.IsEliminated Then
            ' Already eliminated — just ensure map stays clean.
            NeutralizePlayerMap(p)
            Exit Sub
        End If

        If capitals Is Nothing OrElse Not capitals.ContainsKey(p.PlayerNumber) Then Exit Sub

        Dim citadel As Point = capitals(p.PlayerNumber)
        Dim cx As Integer = citadel.X
        Dim cy As Integer = citadel.Y

        ' === Check if enemy sits on citadel ===
        Dim enemyPresent As Boolean = False
        For Each q In Players
            If q Is Nothing OrElse q.PlayerNumber = p.PlayerNumber Then Continue For
            If q.Armies Is Nothing Then Continue For
            For Each a In q.Armies
                If a Is Nothing Then Continue For
                If a.X = cx AndAlso a.Y = cy AndAlso a.TotalSoldiers > 0 Then
                    enemyPresent = True
                    Exit For
                End If
            Next
            If enemyPresent Then Exit For
        Next

        ' === Check garrison ===
        Dim garrison As Integer = 0
        If p.Armies IsNot Nothing Then
            For Each a In p.Armies
                If a Is Nothing Then Continue For
                If a.X = cx AndAlso a.Y = cy Then garrison += a.TotalSoldiers
            Next
        End If

        ' === Defeat condition ===
        If enemyPresent AndAlso garrison <= 0 Then
            p.IsEliminated = True
            'rtbGameInfo.AppendText($"[DEFEAT] {p.Race} (Player {p.PlayerNumber + 1}) eliminated. Citadel captured at ({cx},{cy})." & vbCrLf)

            ' --- Clean up map and armies ---
            NeutralizePlayerMap(p)
            NeutralizePlayerArmies(p)

            ' --- Reset stats ---
            p.Population = 0
            p.FoodCollectedThisTurn = 0
            p.WoodCollectedThisTurn = 0
            p.IronCollectedThisTurn = 0
            p.MountsCollectedThisTurn = 0
            p.MithrilCollectedThisTurn = 0
        End If
    End Sub

    Private Sub NeutralizePlayerMap(p As Player)
        If p Is Nothing Then Exit Sub
        Dim rows As Integer = Map.GetLength(0)
        Dim cols As Integer = Map.GetLength(1)
        For x As Integer = 0 To rows - 1
            For y As Integer = 0 To cols - 1
                If Map(x, y, 1) = p.PlayerNumber Then
                    Map(x, y, 1) = -1
                End If
            Next
        Next
        ' Force a redraw after clearing ownership
        pnlMap.Invalidate()
    End Sub

    Private Sub NeutralizePlayerArmies(p As Player)
        If p Is Nothing OrElse p.Armies Is Nothing Then Exit Sub

        ' --- Safely delete all armies for this player ---
        For Each a In p.Armies.ToList()
            If a Is Nothing Then Continue For
            If a.MoveQueue IsNot Nothing Then a.MoveQueue.Clear()
            a.Units.Clear()
        Next

        p.Armies.Clear()
    End Sub




    ' convenience guard
    Private Function CanAct(p As Player) As Boolean
        Return (p IsNot Nothing AndAlso Not p.IsEliminated)
    End Function



#Region "=== Gameplay Logic ==="


    Public Sub CollectResources()
        Dim rows As Integer = Map.GetLength(0)
        Dim cols As Integer = Map.GetLength(1)

        For Each p In Players
            If Not CanAct(p) Then Continue For
            ' === Reset ===
            p.FoodCollectedThisTurn = 0
            p.WoodCollectedThisTurn = 0
            p.IronCollectedThisTurn = 0
            p.MountsCollectedThisTurn = 0
            p.MithrilCollectedThisTurn = 0

            Dim mountsCollectedRaw As Double = 0.0

            ' === Count owned squares ===
            Dim ownedSquares As Integer = 0
            For x = 0 To rows - 1
                For y = 0 To cols - 1
                    If Map(x, y, 1) = p.PlayerNumber Then ownedSquares += 1
                Next
            Next
            If ownedSquares <= 0 Then ownedSquares = 1

            ' Each square gets an even share of population
            Dim popPerSquare As Integer = Math.Max(1, p.Population \ ownedSquares)

            ' === Define terrain yields per race ===
            ' Arrays ordered as: Plains(0), Forest(1), Hills(2), Mountains(3)
            Dim foodYield(3) As Integer
            Dim woodYield(3) As Integer
            Dim ironYield(3) As Integer

            Select Case p.Race.ToLower()
                Case "elf"
                    ' Forest specialists – high wood, decent food
                    foodYield = {1, 2, 1, 1}   ' Plains, Forest, Hills, Mountains
                    woodYield = {0, 2, 1, 0}
                    ironYield = {0, 0, 1, 1}

                Case "dwarf"
                    ' Mountain specialists – high iron & food on mountains
                    foodYield = {1, 1, 1, 2}   ' Plains, Forest, Hills, Mountains
                    woodYield = {0, 1, 1, 0}
                    ironYield = {0, 0, 1, 2}

                Case "orc"
                    ' Hills specialists – good iron and food
                    foodYield = {1, 1, 2, 1}   ' Plains, Forest, Hills, Mountains
                    woodYield = {0, 1, 1, 0}
                    ironYield = {0, 0, 2, 1}

                Case "human"
                    ' Plains specialists – strong food, balanced otherwise
                    foodYield = {2, 1, 1, 1}   ' Plains, Forest, Hills, Mountains
                    woodYield = {0, 2, 0, 0}
                    ironYield = {0, 0, 1, 2}

                Case Else
                    ' Default fallback
                    foodYield = {1, 1, 1, 1}
                    woodYield = {1, 1, 1, 1}
                    ironYield = {1, 1, 1, 1}
            End Select

            ' === Loop through owned tiles ===
            For x = 0 To rows - 1
                For y = 0 To cols - 1
                    If Map(x, y, 1) <> p.PlayerNumber Then Continue For

                    Dim terrain As Integer = Map(x, y, 0)
                    Dim f As Integer = foodYield(terrain)
                    Dim w As Integer = woodYield(terrain)
                    Dim i As Integer = ironYield(terrain)

                    ' Mounts scale with food yield (favoured terrain advantage)
                    Dim m As Double = f * 0.2

                    ' === Accumulate per square ===
                    p.FoodCollectedThisTurn += f * popPerSquare
                    p.WoodCollectedThisTurn += w * popPerSquare
                    p.IronCollectedThisTurn += i * popPerSquare
                    mountsCollectedRaw += m * popPerSquare

                    ' Dwarves: trace Mithril from mountains
                    If p.Race.Equals("dwarf", StringComparison.OrdinalIgnoreCase) AndAlso terrain = 3 Then
                        p.MithrilCollectedThisTurn += 1
                    End If
                Next
            Next

            ' === Apply small territory bonus (0.5% per tile) ===
            Dim territoryBonus As Double = 1 + ownedSquares * 0.005

            ' === Apply resource scaling ===
            ' Food stays full (used for population & upkeep)
            ' Non-food resources are scaled by NONFOOD_MULTIPLIER
            ' ---------------------------------------------------------
            Const NONFOOD_MULTIPLIER As Double = 0.05  ' <<<<<< adjust this later (0.1 = 10% normal yield)
            ' ---------------------------------------------------------

            p.FoodCollectedThisTurn = CInt(p.FoodCollectedThisTurn * territoryBonus)
            p.WoodCollectedThisTurn = CInt(p.WoodCollectedThisTurn * territoryBonus * NONFOOD_MULTIPLIER)
            p.IronCollectedThisTurn = CInt(p.IronCollectedThisTurn * territoryBonus * NONFOOD_MULTIPLIER)
            p.MithrilCollectedThisTurn = CInt(p.MithrilCollectedThisTurn * territoryBonus * NONFOOD_MULTIPLIER)
            'mountsCollectedRaw *= territoryBonus * NONFOOD_MULTIPLIER

            ' === Convert mounts ===
            Const MOUNTS_DIVISOR As Integer = 60
            p.MountsCollectedThisTurn = CInt(Math.Floor(mountsCollectedRaw / MOUNTS_DIVISOR))

            ' === Add to stockpiles (except food) ===
            p.Wood += p.WoodCollectedThisTurn
            p.Iron += p.IronCollectedThisTurn
            p.Mounts += p.MountsCollectedThisTurn
            p.Mithril += p.MithrilCollectedThisTurn

            ' === Orc slave output ===
            If p.Race.Equals("orc", StringComparison.OrdinalIgnoreCase) Then
                Dim woodFromSlaves As Integer = p.ElfSlaves
                Dim ironFromSlaves As Integer = p.DwarfSlaves
                Dim foodFromSlaves As Integer = p.HumanSlaves

                p.WoodCollectedThisTurn += woodFromSlaves
                p.IronCollectedThisTurn += ironFromSlaves
                p.FoodCollectedThisTurn += foodFromSlaves

                p.Wood += woodFromSlaves
                p.Iron += ironFromSlaves
                'Debug.WriteLine($"[SLAVE OUTPUT] Orc slaves produced +{woodFromSlaves} wood, +{ironFromSlaves} iron, +{foodFromSlaves} food.")
            End If

            ' --- Apply investments ---
            If p.PendingInvestment > 0 AndAlso p.Gold >= p.PendingInvestment Then
                Dim blocks As Integer = p.PendingInvestment \ 1000
                p.Investments += blocks
                p.Gold -= p.PendingInvestment
                p.PendingInvestment = 0
            End If

            ' === 1. Normal income from population ===
            p.GoldCollectedThisTurn = p.Population \ 10

            ' === 2. Add income from previous investments ===
            If p.Investments > 0 Then
                p.GoldCollectedThisTurn += (p.Investments * 100)
            End If

            ' === 3. Apply total to player's gold ===
            p.Gold += p.GoldCollectedThisTurn


            ' === Debug output ===
            'Debug.WriteLine($"[RESOURCE] {p.Race}: +{p.FoodCollectedThisTurn} food, +{p.WoodCollectedThisTurn} wood, +{p.IronCollectedThisTurn} iron, +{p.MountsCollectedThisTurn} mounts, +{p.MithrilCollectedThisTurn} mithril (tiles={ownedSquares})")
        Next
    End Sub


    Public Sub GrowPopulationAndFeedEverybody()
        Const POP_CAP As Double = 50000.0       ' soft ceiling where growth slows heavily
        Const POP_GROWTH_RATE As Double = 0.05  ' base growth multiplier

        For Each p In Players
            If Not CanAct(p) Then Continue For

            ' === 1. Calculate total food required for armies ===
            Dim armyFoodRequirement As Integer = 0
            If p.Armies IsNot Nothing Then
                For Each a In p.Armies
                    armyFoodRequirement += CInt(Math.Floor(a.Units.Sum(Function(u) u.Size * u.FoodCost)))
                Next
            End If

            ' === 2. Total food to feed civilians + armies ===
            Dim totalToFeed As Integer = p.Population + armyFoodRequirement

            ' === 3. Remaining food ===
            Dim remainingFood As Integer = p.FoodCollectedThisTurn - totalToFeed

            ' === 4. Count owned squares ===
            Dim ownedSquares As Integer = 0
            For x As Integer = 0 To 24
                For y As Integer = 0 To 24
                    If Map(x, y, 1) = p.PlayerNumber Then
                        ownedSquares += 1
                    End If
                Next
            Next
            If ownedSquares <= 0 Then ownedSquares = 1

            ' === 5. Growth if surplus food ===
            If remainingFood > 0 Then
                Dim foodRatio As Double = remainingFood / Math.Max(1, totalToFeed)

                ' --- Add territory-based growth bonus ---
                Dim tileBoost As Double = 1.0 + (ownedSquares / 100.0)   ' +1% per owned tile

                ' --- Logistic slow-down factor ---
                ' --- Logistic / exponential slow-down factor ---
                Dim capacityFactor As Double = 1.0 / (1.0 + Math.Exp((p.Population - POP_CAP) / (POP_CAP / 4.0)))
                If capacityFactor < 0 Then capacityFactor = 0

                ' --- Final growth calculation ---
                Dim growth As Integer = CInt(p.Population * foodRatio * POP_GROWTH_RATE * tileBoost * capacityFactor)

                If growth > 0 Then
                    p.Population += growth
                    'Debug.WriteLine($"[POP] {p.Race} grew by {growth} (foodRatio={foodRatio:F2}, tiles={ownedSquares}, tileBoost={tileBoost:F2}, capFactor={capacityFactor:F2}, pop={p.Population})")
                End If
            End If
        Next
    End Sub




#End Region

#Region "=== Map Generation ==="

    Public Sub GenerateMap()
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



#End Region


#Region "=== UI Events ==="

    Private Sub pnlMap_Paint(sender As Object, e As PaintEventArgs) Handles pnlMap.Paint
        DrawMap(e.Graphics, pnlMap.ClientSize.Width, pnlMap.ClientSize.Height)
    End Sub

    Private Sub btn_Show_Click(sender As Object, e As EventArgs)
        pnlMap.Invalidate()
    End Sub

    Private Sub btnProcessTurn_Click(sender As Object, e As EventArgs) Handles btnProcessTurn.Click
        rtbInfo.Clear()

        CurrentReports = New TurnReports With {.TurnNumber = TurnNumber}
        CurrentReports.Messages.Add("Elves sighted an orc warband in the north.")
        CurrentReports.BattleReports.Add("Battle at (10,14): Elves defeated the Orcs.")


        ' BEFORE ANYTHING THIS TURN
        RecomputeEliminationForAllPlayers()

        Dim orders As List(Of ArmyOrder) = GetArmyOrders() '  Apply orders from players, from UI
        ApplyOrdersToArmies(orders) ' Apply orders from players, from UI
        ApplyInvestmentPurchases() ' Apply orders from players, from UI
        ApplyMercBids() '  Apply orders from players, from UI
        ApplyMarketTransactions() ' Apply orders from players, from UI

        ' Resolve bidding on mercenaries
        ResolveBiddingPhase()

        ' --- 1. Collect resources for all players ---
        CollectResources()

        ' --- 2. Grow population and feed armies/civilians ---
        GrowPopulationAndFeedEverybody()

        ' --- 3. Produce trade goods (based on updated population) ---
        ProduceTradeGoods()

        ' --- 4. Summoners act ---
        ApplySummonerPurchases()
        AIBuySummoners()
        ProcessSummoners()

        ' --- 4b. AI Recruitment Phase ---
        For Each p In Players
            If p.AIControlled Then
                'AI_RunRecruitmentPhase_ForPlayer(p)
            End If
        Next

        ' --- 5. Execute army movements step by step ---
        ProcessTurn()

        ' --- 5b. Deduct mercenary wages ---
        PayMercenaryWages()

        ' --- 6. Refresh map and summaries ---
        pnlMap.Invalidate()
        rtbPlayerSummary.Clear()
        rtbPlayerSummary.AppendText(GenerateEmpireSummary())

        ' --- 7. Generate new mercenary offer (for next turn) ---
        HandleNewMercenaryOffer(TurnNumber)

        ' --- 8. Process market for this turn ---
        ProcessMarketTurn(TurnNumber)

        ' --- 9. Update army reports ---
        UpdateArmiesReport()

        ' --- 10. Increment for the next turn ---
        TurnNumber += 1
        ' --- 11. Save the game as this completed turn ---
        SaveGame($"Game{GameNumber:D3}")

        ' --- 12. Update HUD text ---
        lblHud.Text =
        $"Turn: {TurnNumber}" & vbCrLf &
        $"Next Merc Cost: {MercPriceLevel}"

        ' === FINAL sanity sweep ===
        For Each p In Players.ToList()
            If ShouldBeEliminated(p) Then
                ApplyElimination(p)
            End If
        Next

        SafetyCaptureReconciliation()

        RefreshArmyOrdersGrid()

        UpdateSeenMonstersForAllPlayers(Players)

        Printouts.GenerateAllPlayerReports()

    End Sub

    Private Sub HandleNewMercenaryOffer(turnNumber As Integer)
        ''' <summary>
        ''' Handles generation and logging of a new mercenary offer for the current turn.
        ''' </summary>
        CurrentMercOffer = GenerateMercenaryOffer(turnNumber)

        If CurrentMercOffer Is Nothing Then
            'Debug.WriteLine($"Turn {turnNumber}: No mercenary offer generated this turn.")
            Exit Sub
        End If

        Dim offerWages As Integer = CalculateMercenaryOfferWages(CurrentMercOffer)
        Debug.WriteLine($"Turn {turnNumber}: New Mercenary Offer = {CurrentMercOffer}, Wages = {offerWages} gold/turn")
    End Sub


    Private Sub ProcessMarketTurn(currentTurnNumber As Integer)
        ' Clear RTB at start of turn
        'rtbGameInfo.Clear()

        ' Show AI activity
        'rtbGameInfo.AppendText($"=== AI Market Actions (Turn {currentTurnNumber}) ===" & vbCrLf)
        AIHandleMarketTurn(Players)
        'rtbGameInfo.AppendText(vbCrLf)

        ' Update prices + show report
        TheMarket.UpdatePrices()
        'UpdateMarketReport(currentTurnNumber)

        ' Reset demand/supply logs ready for next turn
        TheMarket.ResetTradeLogs()
    End Sub




#End Region


#Region "=== Printing: Front Page ==="

    Private Sub btnPrint_Click(sender As Object, e As EventArgs) Handles btnPrint.Click
        ' Make sure the printing system is ready
        Printouts.CreateTerrainCache()

        ' Use the PrintDocument that lives in the Printouts module
        If Printouts.printDoc IsNot Nothing Then
            Printouts.printDoc.Print()
        Else
            MessageBox.Show("Print document not initialised. (SetupPrinting must run first.)")
        End If
    End Sub



#End Region

    Public Function IsCapital(x As Integer, y As Integer) As Boolean
        For Each kvp In capitals
            Dim playerIndex As Integer = kvp.Key
            Dim pt As Point = kvp.Value
            If pt.X = x AndAlso pt.Y = y Then
                ' Skip eliminated or invalid players
                If playerIndex >= 0 AndAlso playerIndex < Players.Count Then
                    Dim p As Player = Players(playerIndex)
                    If p IsNot Nothing AndAlso Not p.IsEliminated Then
                        Return True
                    End If
                End If
            End If
        Next
        Return False
    End Function



    Private Function GenerateTileName(terrainType As Integer, x As Integer, y As Integer) As String
        ' --- Unique pools for each terrain, no overlaps ---
        Dim plainsPrefixes As String() = {"Ash", "Craven", "Wither", "Fallow", "Rot", "Blight", "Wraith", "Fright", "Bane", "Drift",
                                     "Shorn", "Pale", "Mire", "Doom", "Sallow", "Bleak", "Fester", "Hollow", "Shade", "Grave"}
        Dim plainsSuffixes As String() = {"field", "plain", "reach", "vale", "steppe", "heath", "hollow", "waste", "pasture", "glen",
                                     "moors", "acre", "fen", "march", "expanse", "fold", "drift", "moorland", "bleak", "rift"}

        Dim forestPrefixes As String() = {"Thal", "Eryn", "Fay", "Mor", "Sil", "Loth", "Bryn", "El", "Cinder", "Ashwood",
                                     "Raven", "Umber", "Brack", "Sorrow", "Night", "Dusk", "Fang", "Hollow", "Shade", "Wyrm"}
        Dim forestSuffixes As String() = {"wood", "grove", "mere", "leaf", "copse", "thicket", "briar", "marsh", "loom", "tangle",
                                     "bog", "darkling", "shroud", "hollow", "glade", "shade", "mire", "fen", "bramble", "thorn"}

        Dim hillPrefixes As String() = {"Rock", "High", "Grim", "Fal", "Stone", "Bran", "Cold", "Iron", "Tor", "Crag",
                                   "Obsidian", "Skull", "Blood", "Fell", "Ravenhill", "Brim", "Gorge", "Thorn", "Hollowcrest", "Storm"}
        Dim hillSuffixes As String() = {"peak", "crest", "ridge", "fell", "knoll", "rise", "slope", "spur", "heath", "bluff",
                                   "summit", "fang", "scar", "spire", "marrow", "bleak", "tor", "crag", "knoll", "brim"}

        Dim mountainPrefixes As String() = {"Black", "Storm", "Snow", "Grim", "Frost", "Skull", "Blood", "Thunder", "Obsidian", "Dread",
                                       "Bone", "Night", "Brimstone", "Ironhold", "Cragstone", "Shadowpeak", "Ravenspire", "Blight", "Stonefang", "Bleak"}
        Dim mountainSuffixes As String() = {"spire", "crag", "hold", "mount", "cliff", "rock", "summit", "fang", "pinnacle", "keep",
                                       "hollow", "bleak", "blight", "tor", "doom", "throne", "gloom", "fang", "spire", "crag"}

        ' Select pools
        Dim prefixes() As String
        Dim suffixes() As String
        Select Case terrainType
            Case 0 : prefixes = plainsPrefixes : suffixes = plainsSuffixes
            Case 1 : prefixes = forestPrefixes : suffixes = forestSuffixes
            Case 2 : prefixes = hillPrefixes : suffixes = hillSuffixes
            Case 3 : prefixes = mountainPrefixes : suffixes = mountainSuffixes
            Case Else : prefixes = plainsPrefixes : suffixes = plainsSuffixes
        End Select

        ' Deterministic random using x, y, terrain
        Dim seed As Integer = terrainType * 10000 + x * 25 + y
        Dim rand As New Random(seed)

        Dim prefix As String = prefixes(rand.Next(prefixes.Length))
        Dim suffix As String = suffixes(rand.Next(suffixes.Length))

        Return prefix & suffix
    End Function

    Public Sub ResolveCombat()
        Dim armiesAlreadyInBattle As New HashSet(Of Army)()
        Dim processedLocations As New HashSet(Of Point)()

        ' === Always enumerate snapshots to avoid "collection modified" ===
        Dim playersSnapshot As List(Of Player) =
        If(Players, New List(Of Player)()) _
        .Where(Function(pp) pp IsNot Nothing AndAlso Not pp.IsEliminated AndAlso pp.Armies IsNot Nothing) _
        .ToList()

        ' Loop through each player (snapshot)
        For Each p As Player In playersSnapshot
            ' Snapshot this player's armies (live list may change due to retreats/elims)
            Dim armiesOfP As List(Of Army) = If(p.Armies, New List(Of Army)()).ToList()

            For Each a As Army In armiesOfP
                If a Is Nothing Then Continue For
                ' Skip armies that cannot fight or already battled
                If a.TotalSoldiers <= 0 OrElse armiesAlreadyInBattle.Contains(a) Then Continue For

                Dim grid As List(Of Point) = GetCombatGrid(a)
                If grid Is Nothing OrElse grid.Count = 0 Then Continue For

                ' Snapshot grid too (GetCombatGrid could be dynamic)
                For Each pt As Point In grid.ToList()
                    If processedLocations.Contains(pt) Then Continue For

                    ' --- Gather all armies on this tile that can fight (via snapshots) ---
                    Dim allArmiesHere As New List(Of Army)

                    For Each pl As Player In playersSnapshot
                        Dim plArmies As List(Of Army) = If(pl.Armies, New List(Of Army)()).ToList()
                        For Each ar As Army In plArmies
                            If ar Is Nothing Then Continue For
                            If ar.TotalSoldiers <= 0 Then Continue For
                            If armiesAlreadyInBattle.Contains(ar) Then Continue For

                            Dim arGrid As List(Of Point) = GetCombatGrid(ar)
                            If arGrid IsNot Nothing AndAlso arGrid.Contains(pt) Then
                                allArmiesHere.Add(ar)
                            End If
                        Next
                    Next

                    ' Debug line to confirm how many armies are found at this tile
                    Console.WriteLine($"[COMBAT] Tile {pt.X},{pt.Y} has {allArmiesHere.Count} armies engaged.")

                    ' Need at least 2 distinct sides
                    Dim distinctPlayers As New HashSet(Of String)(
                    allArmiesHere.Where(Function(ar) ar IsNot Nothing).Select(Function(ar) ar.Race),
                    StringComparer.OrdinalIgnoreCase
                )
                    If distinctPlayers.Count < 2 Then Continue For

                    processedLocations.Add(pt)

                    ' --- Merge armies per player temporarily for battle ---
                    Dim mergedArmies As New List(Of Army)
                    Dim mergedToOriginal As New Dictionary(Of Army, List(Of Army))()

                    For Each race As String In distinctPlayers.ToList()
                        Dim playerArmies As List(Of Army) =
                        allArmiesHere.Where(Function(ar) ar.Race.Equals(race, StringComparison.OrdinalIgnoreCase)).ToList()

                        ' Guard: skip if somehow empty
                        If playerArmies.Count = 0 Then Continue For

                        Dim mergedArmy As New Army With {
                        .Race = race,
                        .X = playerArmies(0).X,
                        .Y = playerArmies(0).Y
                    }

                        ' Copy units from each source army into the merged army (clone stacks)
                        For Each ua As Army In playerArmies
                            For Each u As Unit In ua.Units.ToList()
                                mergedArmy.Units.Add(New Unit(u)) ' clone constructor
                            Next
                        Next

                        mergedArmies.Add(mergedArmy)
                        mergedToOriginal(mergedArmy) = playerArmies
                    Next

                    ' --- Immutable pre-battle snapshot BEFORE Battle() mutates anything ---
                    Dim startSnapshot As List(Of Army) = CloneArmiesForSnapshot(mergedArmies)

                    ' --- Conduct battle ---
                    Dim battleLog As BattleLog = Battle(mergedArmies)
                    If battleLog Is Nothing Then Continue For

                    ' --- Distribute casualties back proportionally to original armies ---
                    For Each mergedArmy As Army In mergedArmies.ToList()
                        Dim originalArmies As List(Of Army) = mergedToOriginal(mergedArmy)

                        For Each mergedUnit As Unit In mergedArmy.Units.ToList()
                            ' Total size of this unit across all original armies (pre-battle)
                            Dim totalOriginalSize As Integer =
                            originalArmies.Sum(Function(origArmy) origArmy.Units.
                                Where(Function(u) u.Name = mergedUnit.Name AndAlso u.Type = mergedUnit.Type).
                                Sum(Function(u) u.Size))

                            If totalOriginalSize = 0 Then Continue For

                            ' Sum casualties this specific merged unit took across all phases (by reference)
                            Dim mergedCasualties As Integer = 0
                            For Each phaseList In battleLog.PhaseEntries.Values.ToList()
                                For Each e In phaseList.ToList()
                                    If Object.ReferenceEquals(e.Defender, mergedUnit) Then
                                        mergedCasualties += e.Casualties
                                    End If
                                Next
                            Next
                            If mergedCasualties <= 0 Then Continue For

                            ' === EXACT ALLOCATION (floor + largest remainder) ===
                            Dim targets As New List(Of Tuple(Of Unit, Integer))() ' (unitRef, preSize)
                            For Each origArmy In originalArmies
                                For Each u In origArmy.Units.ToList()
                                    If u.Name = mergedUnit.Name AndAlso u.Type = mergedUnit.Type Then
                                        targets.Add(Tuple.Create(u, u.Size))
                                    End If
                                Next
                            Next
                            If targets.Count = 0 Then Continue For

                            Dim shares(targets.Count - 1) As Integer
                            Dim remainders As New List(Of Tuple(Of Integer, Double))()
                            Dim assigned As Integer = 0

                            For i As Integer = 0 To targets.Count - 1
                                Dim pre As Integer = targets(i).Item2
                                Dim exact As Double = mergedCasualties * (pre / CDbl(totalOriginalSize))
                                Dim flo As Integer = CInt(Math.Floor(exact))
                                flo = Math.Min(flo, pre)
                                shares(i) = flo
                                assigned += flo
                                remainders.Add(Tuple.Create(i, exact - flo))
                            Next

                            Dim remainder As Integer = mergedCasualties - assigned
                            If remainder > 0 Then
                                remainders = remainders.OrderByDescending(Function(t) t.Item2).ThenBy(Function(t) t.Item1).ToList()
                                Dim k As Integer = 0
                                While remainder > 0 AndAlso remainders.Count > 0
                                    Dim idx As Integer = remainders(k Mod remainders.Count).Item1
                                    Dim pre As Integer = targets(idx).Item2
                                    If shares(idx) < pre Then
                                        shares(idx) += 1
                                        remainder -= 1
                                        k += 1
                                    Else
                                        ' drop capped index from rotation
                                        remainders.RemoveAt(k Mod remainders.Count)
                                    End If
                                End While
                            End If

                            ' Apply shares to live unit refs
                            For i As Integer = 0 To targets.Count - 1
                                Dim uRef As Unit = targets(i).Item1
                                Dim take As Integer = shares(i)
                                If take > uRef.Size Then take = uRef.Size
                                uRef.Size -= take
                            Next
                            ' === END EXACT ALLOCATION ===
                        Next
                    Next

                    ' --- Determine winning race from post-battle merged totals ---
                    Dim winningRace As String = Nothing
                    Dim raceTotals = mergedArmies _
                    .GroupBy(Function(ma) ma.Race, StringComparer.OrdinalIgnoreCase) _
                    .Select(Function(g) New With {
                        .Race = g.Key,
                        .Total = g.Sum(Function(ma) ma.TotalSoldiers)
                    }) _
                    .OrderByDescending(Function(x) x.Total) _
                    .ToList()

                    If raceTotals.Count > 0 Then
                        Dim top = raceTotals(0)
                        If raceTotals.Count = 1 OrElse top.Total > raceTotals(1).Total Then
                            winningRace = top.Race
                        End If
                    End If

                    ' --- Retreat losers on the live armies (does not remove lists) ---
                    If Not String.IsNullOrEmpty(winningRace) Then
                        For Each army As Army In allArmiesHere.ToList()
                            If Not army.Race.Equals(winningRace, StringComparison.OrdinalIgnoreCase) Then
                                SendArmyBackToSpawn(army)      ' must NOT remove army from its owner's list
                                battleLog.RecordRetreat(army)
                            End If
                        Next
                    End If

                    ' --- Mark these live armies as having battled ---
                    For Each army As Army In allArmiesHere.ToList()
                        armiesAlreadyInBattle.Add(army)
                    Next

                    ' --- Report ---
                    Dim compactReport As String = GenerateCompactPhaseReport(battleLog, mergedArmies, startSnapshot)
                    rtbInfo.AppendText(compactReport)

                    ' --- Cleanup dead stacks (safe, only mutates Units of each army) ---
                    For Each army As Army In allArmiesHere.ToList()
                        army.Units.RemoveAll(Function(u) u.Size <= 0)
                    Next

                    ' Do NOT recompute eliminations here; queue them and apply after loops
                    ' (prevents collection mutation mid-enumeration)

                    ' *** Removed "Exit For" to allow all armies on same tile to be processed ***
                Next
            Next
        Next

        ' After all enumerations are complete, now it’s safe to eliminate players
        QueueEliminationChecksSafe()
    End Sub



    Public Function Battle(battleArmies As List(Of Army)) As BattleLog
        ' --- Filter armies with soldiers remaining ---
        Dim activeArmies As List(Of Army) = battleArmies.Where(Function(a) a.TotalSoldiers > 0).ToList()
        If activeArmies.Count = 0 Then Return Nothing

        Dim battleLog As New BattleLog(activeArmies)

        ' --- Run the first three phases exactly as before ---
        For Each phaseName In New String() {"Ranged", "Charge", "Melee"}
            ' Snapshot of units for proportional damage (phase-start sizes)
            Dim unitSnapshot As New Dictionary(Of Unit, Integer)
            For Each army In activeArmies
                For Each u In army.Units
                    unitSnapshot(u) = u.Size
                Next
            Next

            ' Apply proportional damage for each defending army from all attackers
            For Each defArmy In activeArmies
                Dim attackers As List(Of Unit) = activeArmies.
                                             Where(Function(a) a IsNot defArmy).
                                             SelectMany(Function(a) a.Units).ToList()
                Dim attackerArmies As List(Of Army) = activeArmies.Where(Function(a) a IsNot defArmy).ToList()
                ApplyProportionalDamage(defArmy, attackerArmies, phaseName, unitSnapshot, battleLog)
            Next
        Next

        ' === Decide winner/losers after Melee (phase 3) ===
        ' Compute current strengths post-casualties
        If activeArmies.Count < 2 Then
            ' Nothing to chase if only one side remains
            Return battleLog
        End If

        Dim maxStrength As Integer = activeArmies.Max(Function(a) a.TotalSoldiers)
        Dim topArmies = activeArmies.Where(Function(a) a.TotalSoldiers = maxStrength).ToList()

        ' If tie for strongest -> no chase
        If topArmies.Count <> 1 Then
            Return battleLog
        End If

        Dim winner As Army = topArmies(0)
        Dim losers As List(Of Army) = activeArmies.Where(Function(a) a IsNot winner).ToList()

        ' Winner's LC only
        Dim winnerLC As List(Of Unit) = winner.Units.
            Where(Function(u) u.CanChase AndAlso u.Size > 0).ToList()

        ' If no LC or no losers -> no chase
        If winnerLC.Count = 0 OrElse losers.Count = 0 Then
            Return battleLog
        End If

        ' --- CHASE PHASE (only winner LC attacks, only losers defend) ---
        ' Build a fresh snapshot at start of Chase (sizes after Melee)
        Dim chaseSnapshot As New Dictionary(Of Unit, Integer)
        For Each army In activeArmies
            For Each u In army.Units
                chaseSnapshot(u) = u.Size
            Next
        Next

        ' Each losing army takes proportional damage from winner LC
        For Each defArmy In losers
            ApplyProportionalDamage(defArmy, {winner}.ToList(), "Chase", chaseSnapshot, battleLog)
        Next

        Return battleLog
    End Function


    Public Sub ClearArmyMoveQueue(army As Army)
        ''' <summary>
        ''' Clears all remaining moves in the army's MoveQueue.
        ''' This is used after an army is sent back to spawn,
        ''' since any queued moves are no longer relevant.
        ''' </summary>

        If army.MoveQueue IsNot Nothing Then
            army.MoveQueue.Clear()
        End If
    End Sub


    Public Sub ApplyProportionalDamage(defArmy As Army,
                                   attackerArmies As List(Of Army),
                                   phase As String,
                                   unitSnapshot As Dictionary(Of Unit, Integer),
                                   battleLog As BattleLog)

        ' --- Initialize dictionary to accumulate casualties per defender unit ---
        Dim calculatedCasualties As New Dictionary(Of Unit, Long)
        For Each defUnit In defArmy.Units
            calculatedCasualties(defUnit) = 0
        Next

        ' --- Flatten attackers for iteration ---
        Dim attackingUnits As List(Of Unit) = attackerArmies.SelectMany(Function(a) a.Units).ToList()

        ' --- Loop over each attacker ---
        For Each atkUnit In attackingUnits
            Dim atkSize As Integer = unitSnapshot(atkUnit) ' snapshot for attacker's size
            Dim atkValue As Integer = 0
            Dim atkExplanation As String = ""

            ' Determine attack value based on phase
            Select Case phase.ToLower()
                Case "ranged"
                    If atkUnit.Ranged > 0 Then
                        atkValue = atkUnit.Ranged
                        atkExplanation = $"{atkUnit.Ranged} ranged"
                    Else
                        Continue For
                    End If

                Case "charge"
                    If atkUnit.CanCharge Then
                        atkValue = atkUnit.Melee
                        atkExplanation = $"{atkUnit.Melee} melee (charge phase)"
                    Else
                        Continue For
                    End If

                Case "melee"
                    atkValue = atkUnit.Melee
                    atkExplanation = $"{atkUnit.Melee} melee"

                Case "chase"
                    If atkUnit.CanChase Then
                        atkValue = atkUnit.Melee
                        If atkUnit.Flying Then
                            atkValue *= 2
                            atkExplanation = $"{atkUnit.Melee} melee (flying, double in chase)"
                        Else
                            atkExplanation = $"{atkUnit.Melee} melee for chase"
                        End If
                    Else
                        Continue For
                    End If
            End Select

            ' === Find this unit's army (for training level lookup) ===
            Dim atkArmy As Army = attackerArmies.FirstOrDefault(Function(a) a.Units.Contains(atkUnit))

            ' === Apply attacker training bonus ===
            If atkArmy IsNot Nothing AndAlso atkArmy.TrainingLevel > 0 Then
                Dim atkBonus As Double = 1.0 + atkArmy.TrainingLevel
                atkValue = CInt(Math.Round(atkValue * atkBonus))
                atkExplanation &= $" +{atkArmy.TrainingLevel:P0} training"
            End If

            ' --- Total HP-weighted size of defenders ---
            Dim totalDefWeight As Double = defArmy.Units.Sum(Function(u) unitSnapshot(u) * u.GetEffectiveHP())
            If totalDefWeight = 0 Then Continue For

            ' --- Apply proportional damage to each defender unit ---
            For Each defUnit In defArmy.Units
                Dim sizeBefore As Integer = unitSnapshot(defUnit)
                Dim unitWeight As Double = sizeBefore * defUnit.GetEffectiveHP()

                ' Raw damage proportional to weight
                Dim rawDamage As Double = atkValue * atkSize * (unitWeight / totalDefWeight)

                ' Mitigation fraction + explanation
                Dim mitResult = GetUnitMitigation(defUnit)
                Dim mitigationValue As Double = mitResult.Mitigation
                Dim mitigationExplanation As String = mitResult.Explanation

                ' === Apply defender training bonus (reduces damage slightly) ===
                If defArmy.TrainingLevel > 0 Then
                    ' Defensive training bonus is half as strong as attack training
                    Dim defBonus As Double = Math.Min(defArmy.TrainingLevel * 0.5, 0.9)
                    mitigationValue = 1 - ((1 - mitigationValue) * (1 - defBonus))
                    mitigationExplanation &= $" +{defArmy.TrainingLevel:P0} training"
                End If

                ' Final damage after mitigation, with global casualty multiplier
                Const CASUALTY_MULTIPLIER As Double = 3.0
                Dim finalDamage As Double = rawDamage * (1 - mitigationValue) * CASUALTY_MULTIPLIER

                ' --- Calculate casualties (clamped to remaining men this phase) ---
                Dim casualtiesRaw As Integer = CInt(Math.Floor(finalDamage / defUnit.GetEffectiveHP()))
                Dim alreadyAllocated As Integer = CInt(calculatedCasualties(defUnit))
                Dim remainingThisPhase As Integer = Math.Max(0, sizeBefore - alreadyAllocated)
                Dim casualties As Integer = Math.Min(remainingThisPhase, casualtiesRaw)

                ' --- Record entry in BattleLog (using clamped casualties) ---
                If battleLog IsNot Nothing AndAlso casualties > 0 Then
                    Dim entry As New BattleEntry With {
                    .Attacker = atkUnit,
                    .Defender = defUnit,
                    .Phase = phase,
                    .SizeBefore = sizeBefore,
                    .SizeAfter = Math.Max(0, sizeBefore - (alreadyAllocated + casualties)),
                    .RawDamage = rawDamage,
                    .Mitigation = mitigationValue,
                    .MitigationExplanation = mitigationExplanation,
                    .FinalDamage = finalDamage,
                    .Casualties = casualties,
                    .RawDamageExplanation = $"Damage from {atkUnit.Name} ({atkExplanation})",
                    .AttackerSizeAtPhaseStart = atkSize
                }

                    If Not battleLog.PhaseEntries.ContainsKey(phase) Then
                        battleLog.PhaseEntries(phase) = New List(Of BattleEntry)
                    End If
                    battleLog.PhaseEntries(phase).Add(entry)
                End If

                ' Accumulate casualties for simultaneous application
                calculatedCasualties(defUnit) += casualties
            Next
        Next

        ' --- Apply all accumulated casualties to real unit sizes ---
        For Each kvp In calculatedCasualties
            kvp.Key.Size -= CInt(kvp.Value)
            If kvp.Key.Size < 0 Then kvp.Key.Size = 0

            ' Optional: mark/report dead now
            If kvp.Key.Size = 0 Then
                'Debug.WriteLine($"[BATTLE] {kvp.Key.Name} wiped out in {phase} phase")
            End If
        Next

        ' ✅ Note: Do not remove units here. Cleanup will happen at the end of ResolveBattle.
    End Sub



    Public Function GetUnitMitigation(unit As Unit) As (Mitigation As Double, Explanation As String)
        Dim mitigation As Double = 0.0
        Dim parts As New List(Of String)

        ' DefencePoints mitigation — use effective value for heroes
        Dim effectiveDef As Integer = unit.GetEffectiveDefencePoints()
        If effectiveDef > 0 Then
            Dim defMit As Double = effectiveDef * 0.05
            mitigation += defMit
            parts.Add($"{effectiveDef * 5}% defence points")
        End If

        ' Cap total mitigation at 70%
        If mitigation > 0.7 Then
            mitigation = 0.7
            parts.Add("(capped at 70%)")
        End If

        Dim explanation As String = If(parts.Count > 0, String.Join(" + ", parts), "No mitigation")
        Return (mitigation, explanation)
    End Function


#Region "=== AI Recruitment v2 ==="

    ' Predictive power buffer per player (PlayerNumber -> projected gain this tick)
    Private ReadOnly AIPowerQueuedGain As New Dictionary(Of Integer, Integer)()

    ' Reset the projection just for this player (call once at start of their pass)
    Private Sub AIRecruit_ResetQueuedGain(forPlayerNumber As Integer)
        If AIPowerQueuedGain.ContainsKey(forPlayerNumber) Then
            AIPowerQueuedGain.Remove(forPlayerNumber)
        End If
    End Sub

    ' Add predicted gain so later armies see the stronger projection
    Private Sub AddPredictedGain(p As Player, gain As Integer)
        Dim curr As Integer
        If AIPowerQueuedGain.TryGetValue(p.PlayerNumber, curr) Then
            AIPowerQueuedGain(p.PlayerNumber) = curr + gain
        Else
            AIPowerQueuedGain(p.PlayerNumber) = gain
        End If
    End Sub

    ' Actual + queued (projected) combat power for this player
    Private Function GetPredictedTotalPower(p As Player) As Integer
        Dim actual As Integer = 0
        If p IsNot Nothing AndAlso p.Armies IsNot Nothing Then
            For Each a In p.Armies
                If a IsNot Nothing Then actual += a.TotalSoldiers
            Next
        End If
        Dim queued As Integer = 0
        If AIPowerQueuedGain.ContainsKey(p.PlayerNumber) Then queued = AIPowerQueuedGain(p.PlayerNumber)
        Return actual + queued
    End Function

    ' Strongest enemy total soldiers (empire-wide)
    Private Function GetStrongestEnemyPower(p As Player) As Integer
        Dim strongest As Integer = 0
        For Each opp In Players
            If opp Is Nothing OrElse opp Is p Then Continue For
            Dim s As Integer = 0
            If opp.Armies IsNot Nothing Then
                For Each a In opp.Armies
                    If a IsNot Nothing Then s += a.TotalSoldiers
                Next
            End If
            If s > strongest Then strongest = s
        Next
        Return strongest
    End Function

    ' --- Utility: simple "cost weight" to decide the cheapest template ---
    Private Function CostWeight(u As UnitStats) As Integer
        If u Is Nothing Then Return Integer.MaxValue
        If String.IsNullOrWhiteSpace(u.Cost) Then Return 0
        Dim total As Integer = 0
        For Each part As String In u.Cost.Split(","c)
            Dim t As String = part.Trim()
            If t.Length = 0 Then Continue For
            Dim n As Integer = 1
            Dim idx As Integer = t.IndexOf(":"c)
            If idx >= 0 Then
                Dim raw As String = t.Substring(idx + 1).Trim()
                Dim parsed As Integer
                If Integer.TryParse(raw, parsed) Then n = Math.Max(0, parsed)
            End If
            total += n
        Next
        Return total
    End Function

    Private Function FirstRecruitableOfType(ru As RaceUnits, t As UnitType) As UnitStats
        If ru Is Nothing OrElse ru.Units Is Nothing Then Return Nothing
        For Each u In ru.Units
            If u IsNot Nothing AndAlso u.Type = t AndAlso Not String.IsNullOrWhiteSpace(u.ShortName) Then
                Return u
            End If
        Next
        Return Nothing
    End Function

    Private Function CheapestRecruitable(ru As RaceUnits) As UnitStats
        If ru Is Nothing OrElse ru.Units Is Nothing OrElse ru.Units.Count = 0 Then Return Nothing
        Dim best As UnitStats = Nothing
        Dim bestW As Integer = Integer.MaxValue
        For Each u In ru.Units
            If u Is Nothing OrElse String.IsNullOrWhiteSpace(u.ShortName) Then Continue For
            Dim w As Integer = CostWeight(u)
            If w < bestW Then
                bestW = w
                best = u
            End If
        Next
        Return best
    End Function

    ' ==============================
    ' PART 1: Should we recruit now?
    ' ==============================
    Private Function ShouldRecruitArmy(player As Player) As Boolean
        If player Is Nothing Then Return False

        ' Population gate (protects growth)
        If player.Population < 1000 Then Return False

        Dim myPredicted As Integer = GetPredictedTotalPower(player)
        Dim enemyStrongest As Integer = GetStrongestEnemyPower(player)
        If enemyStrongest <= 0 Then Return True ' no opposition yet

        Dim ratio As Double = CDbl(myPredicted) / CDbl(enemyStrongest)

        ' Recruit if we are below 80% of strongest enemy
        Return ratio < 0.8R
    End Function

    ' ============================================
    ' PART 2: What unit should this army recruit?
    ' ============================================
    Private Function DecideRecruitmentUnit(player As Player, army As Army) As UnitStats
        If player Is Nothing OrElse army Is Nothing Then Return Nothing

        Dim ru As RaceUnits = AllRaces.FirstOrDefault(Function(r) String.Equals(r.RaceName, player.Race, StringComparison.OrdinalIgnoreCase))
        If ru Is Nothing Then Return Nothing

        ' If crippled, pick cheapest recruitable body to top up quickly
        If army.TotalSoldiers < 500 Then
            Dim cheap As UnitStats = CheapestRecruitable(ru)
            If cheap IsNot Nothing Then Return cheap
        End If

        ' Race preferences
        Dim preferred As UnitType = UnitType.LightInfantry
        Select Case LCase(Trim(player.Race))
            Case "elf" : preferred = UnitType.Archer
            Case "orc" : preferred = UnitType.LightInfantry
            Case "dwarf" : preferred = UnitType.HeavyInfantry
            Case "human" : preferred = UnitType.HeavyCavalry
        End Select

        ' Elven archer cap: if >60% archers, prefer LI as a shield
        If String.Equals(player.Race, "Elf", StringComparison.OrdinalIgnoreCase) AndAlso army.Units IsNot Nothing Then
            Dim total As Integer = 0
            Dim arch As Integer = 0
            For Each u In army.Units
                If u Is Nothing Then Continue For
                total += u.Size
                If u.Type = UnitType.Archer Then arch += u.Size
            Next
            If total > 0 Then
                Dim ar As Double = CDbl(arch) / CDbl(total)
                If ar > 0.6R Then preferred = UnitType.LightInfantry
            End If
        End If

        ' Try preferred type with a valid ShortName (required by your RECRUIT executor)
        Dim templ As UnitStats = FirstRecruitableOfType(ru, preferred)
        If templ IsNot Nothing Then Return templ

        ' Fallback: cheapest recruitable in roster
        Return CheapestRecruitable(ru)
    End Function

    ' ======================================================
    ' MAIN ENTRY: Enqueue one RECRUIT for this army if needed
    ' (called from your ProcessTurn() special step)
    ' ======================================================
    Public Sub AIRecruitArmy(army As Army, player As Player, armyIndex As Integer)
        If player Is Nothing OrElse player.IsEliminated Then Exit Sub
        If army Is Nothing OrElse player Is Nothing Then Exit Sub
        If Not player.AIControlled Then Exit Sub

        ' One-time reset for this player (first army we touch this tick)
        If armyIndex = 0 Then
            AIRecruit_ResetQueuedGain(player.PlayerNumber)
        End If

        ' Empire-level decision
        If Not ShouldRecruitArmy(player) Then
            'Debug.WriteLine($"[AI] {player.Race}: no recruitment (pred={GetPredictedTotalPower(player)}, enemyMax={GetStrongestEnemyPower(player)}).")
            Exit Sub
        End If

        ' Choose a recruitable template
        Dim pick As UnitStats = DecideRecruitmentUnit(player, army)
        If pick Is Nothing Then
            'Debug.WriteLine($"[AI] {player.Race}: no valid unit template to recruit.")
            Exit Sub
        End If
        If String.IsNullOrWhiteSpace(pick.ShortName) Then
            'Debug.WriteLine($"[AI] {player.Race}: template '{pick.Name}' lacks ShortName; cannot enqueue RECRUIT.")
            Exit Sub
        End If

        ' Ensure MoveQueue exists
        If army.MoveQueue Is Nothing Then army.MoveQueue = New List(Of ArmyCommand)()

        ' Enqueue command; your ProcessTurn special step will perform RecruitArmyUnits
        army.MoveQueue.Add(New ArmyCommand With {.Command = "RECRUIT", .Parameter = pick.ShortName})

        ' Predict empire gain so later armies see a stronger projection (5% of current pop)
        Dim maxPerTurn As Integer = Math.Max(1, CInt(Math.Floor(player.Population * 0.05R)))
        AddPredictedGain(player, maxPerTurn)

        'Debug.WriteLine($"[AI] {player.Race}: queued RECRUIT {pick.Name} (short '{pick.ShortName}') for Army {armyIndex + 1}. PredPower now {GetPredictedTotalPower(player)}.")
    End Sub

#End Region





    Private Function IsCaptureValidOrthogonal(x As Integer, y As Integer, playerNumber As Integer) As Boolean
        Dim mapSizeX As Integer = Map.GetLength(0)
        Dim mapSizeY As Integer = Map.GetLength(1)

        ' Only N/E/S/W neighbors
        Dim directions As Point() = {New Point(0, -1), New Point(1, 0), New Point(0, 1), New Point(-1, 0)}
        For Each d In directions
            Dim nx As Integer = x + d.X
            Dim ny As Integer = y + d.Y
            If nx >= 0 AndAlso nx < mapSizeX AndAlso ny >= 0 AndAlso ny < mapSizeY Then
                If Map(nx, ny, 1) = playerNumber Then
                    Return True
                End If
            End If
        Next
        Return False
    End Function


    Public Function GenerateCompactPhaseReport(battleLog As BattleLog,
                                           mergedArmies As List(Of Army),
                                           startSnapshot As List(Of Army)) As String
        Dim sb As New Text.StringBuilder()

        ' ---------- helpers ----------
        Dim KeyOf As Func(Of Unit, String) =
        Function(u As Unit) u.Name & "|" & CInt(u.Type).ToString()

        ' Map each merged-army Unit reference -> its owning merged Army
        Dim ownerByUnit As New Dictionary(Of Unit, Army)
        For Each ma In mergedArmies
            For Each u In ma.Units
                ownerByUnit(u) = ma
            Next
        Next

        ' Build stable unit order + starting sizes from the immutable snapshot.
        ' Also build a reference-index map per army so we can update sizes by *unit instance*.
        Dim unitOrderByArmy As New Dictionary(Of Army, List(Of Unit))()
        Dim startSizesByArmy As New Dictionary(Of Army, List(Of Integer))()
        Dim indexByUnitRef As New Dictionary(Of Army, Dictionary(Of Unit, Integer))()

        For i As Integer = 0 To startSnapshot.Count - 1
            Dim snapA = startSnapshot(i)
            Dim liveA = mergedArmies(i)

            ' Names/order for reporting come from the snapshot (pre-battle)
            unitOrderByArmy(liveA) = New List(Of Unit)(
            snapA.Units.Select(Function(u) New Unit(u) With {.Size = u.Size})
        )
            startSizesByArmy(liveA) = New List(Of Integer)(snapA.Units.Select(Function(u) u.Size))

            ' Map *live* unit object references -> their index (aligned with snapshot order)
            Dim idxMapRef As New Dictionary(Of Unit, Integer)()
            For j As Integer = 0 To liveA.Units.Count - 1
                idxMapRef(liveA.Units(j)) = j
            Next
            indexByUnitRef(liveA) = idxMapRef
        Next

        ' ---------- Start of Battle ----------
        sb.AppendLine("Start of Battle:")
        For i As Integer = 0 To startSnapshot.Count - 1
            Dim snapA = startSnapshot(i)
            Dim parts = New List(Of String)

            ' Sort units by descending size for initial report
            For Each u In snapA.Units.OrderByDescending(Function(x) x.Size)
                parts.Add($"{u.Name} ({u.Size})")
            Next

            Dim total0 As Integer = snapA.Units.Sum(Function(u) u.Size)
            sb.AppendLine($"{snapA.Race} Army: {String.Join(", ", parts)} | Total: {total0}")

            If i < startSnapshot.Count - 1 Then sb.AppendLine()
        Next
        sb.AppendLine(New String("-"c, 60))

        ' ---------- Phase-by-phase ----------
        Dim currentSizesByArmy As New Dictionary(Of Army, Integer())()
        For Each liveA In mergedArmies
            currentSizesByArmy(liveA) = startSizesByArmy(liveA).ToArray()
        Next

        Dim phases As String() = {"Ranged", "Charge", "Melee", "Chase"}

        For Each phase In phases
            If Not PhaseHadCasualties(battleLog, phase) Then Continue For

            sb.AppendLine($"{phase} Phase")

            ' === Simplified losses summary by race ===
            If battleLog IsNot Nothing AndAlso battleLog.PhaseEntries.ContainsKey(phase) Then
                Dim raceLosses = battleLog.PhaseEntries(phase).
                GroupBy(Function(e) ownerByUnit(e.Defender).Race).
                Select(Function(g) $"{g.Key} ({g.Sum(Function(e) e.Casualties)})").
                ToList()

                If raceLosses.Any() Then
                    sb.AppendLine("Losses: " & String.Join(", ", raceLosses))
                    sb.AppendLine()
                End If
            End If

            ' === Main Casualties summary (top 5, filter out 0-loss, always include heroes) ===
            If battleLog IsNot Nothing AndAlso battleLog.PhaseEntries.ContainsKey(phase) Then
                Dim grouped = battleLog.PhaseEntries(phase).
                GroupBy(Function(e)
                            Dim owningRace As String = ownerByUnit(e.Defender).Race
                            Return $"{e.Defender.Name}|{CInt(e.Defender.Type)}|{owningRace}"
                        End Function).
                Select(Function(g) New With {
                    .Name = g.First().Defender.Name,
                    .Race = ownerByUnit(g.First().Defender).Race,
                    .Casualties = g.Sum(Function(e) e.Casualties),
                    .IsHero = g.First().Defender.IsHero,
                    .Level = g.First().Defender.Level,
                    .Killed = False
                }).
                Where(Function(x) x.Casualties > 0).
                OrderByDescending(Function(x) x.Casualties).
                Take(5).
                ToList()

                ' --- Always include hero deaths/losses ---
                Dim heroLosses = battleLog.PhaseEntries(phase).
                Where(Function(e) e.Defender.IsHero AndAlso e.Casualties > 0).
                Select(Function(e)
                           Dim race = ownerByUnit(e.Defender).Race
                           Dim owningArmy As Army = ownerByUnit(e.Defender)
                           Dim idx As Integer = -1
                           Dim killed As Boolean = False

                           ' Resolve by Unit reference -> index
                           If indexByUnitRef.ContainsKey(owningArmy) AndAlso
                              indexByUnitRef(owningArmy).TryGetValue(e.Defender, idx) Then
                               Dim arr = currentSizesByArmy(owningArmy)
                               If idx >= 0 AndAlso idx < arr.Length AndAlso arr(idx) = 0 Then
                                   killed = True
                               End If
                           End If

                           Return New With {
                               .Name = e.Defender.Name,
                               .Race = race,
                               .Casualties = e.Casualties,
                               .IsHero = True,
                               .Level = e.Defender.Level,
                               .Killed = killed
                           }
                       End Function).ToList()

                For Each h In heroLosses
                    If Not grouped.Any(Function(x) x.Name = h.Name AndAlso x.Race = h.Race) Then
                        grouped.Add(h)
                    End If
                Next

                If grouped.Any() Then
                    sb.AppendLine("Main Casualties:")
                    For Each g In grouped
                        If g.IsHero Then
                            If g.Killed Then
                                sb.AppendLine($" *** HERO KILLED - {g.Name.ToUpper()} ({g.Race.ToUpper()}) - LEVEL {g.Level} ***")
                            Else
                                sb.AppendLine($" - {g.Name} ({g.Race}) lost {g.Casualties} [HERO Lvl {g.Level}]")
                            End If
                        Else
                            sb.AppendLine($" - {g.Name} ({g.Race}) lost {g.Casualties}")
                        End If
                    Next
                    sb.AppendLine("------------")
                    sb.AppendLine()
                End If
            End If

            ' === Apply casualties to working copy (by Unit reference) ===
            If battleLog IsNot Nothing AndAlso battleLog.PhaseEntries.ContainsKey(phase) Then
                For Each entry In battleLog.PhaseEntries(phase)
                    Dim owningArmy As Army = Nothing
                    If ownerByUnit.TryGetValue(entry.Defender, owningArmy) Then
                        Dim arr = currentSizesByArmy(owningArmy)

                        ' Prefer fast dictionary lookup; fall back to a reference scan if needed
                        Dim idx As Integer = -1
                        If indexByUnitRef.ContainsKey(owningArmy) AndAlso
                       indexByUnitRef(owningArmy).TryGetValue(entry.Defender, idx) Then
                            ' found
                        Else
                            ' Fallback: reference-based scan
                            For j As Integer = 0 To owningArmy.Units.Count - 1
                                If Object.ReferenceEquals(owningArmy.Units(j), entry.Defender) Then
                                    idx = j
                                    Exit For
                                End If
                            Next
                        End If

                        If idx >= 0 AndAlso idx < arr.Length Then
                            Dim before As Integer = arr(idx)
                            Dim after As Integer = Math.Max(0, before - entry.Casualties)
                            arr(idx) = after
                        End If
                    End If
                Next
            End If
        Next

        ' ---------- Final Army Status ----------
        sb.AppendLine("=== Final Army Status ===")
        For i As Integer = 0 To startSnapshot.Count - 1
            Dim snapA = startSnapshot(i)
            Dim liveA = mergedArmies(i)
            Dim names = unitOrderByArmy(liveA)
            Dim sizes = currentSizesByArmy(liveA)

            Dim parts As New List(Of String)

            ' Sort by descending size for final report
            Dim sorted = names.Select(Function(u, idx) New With {.Unit = u, .Size = sizes(idx)}).
                           OrderByDescending(Function(x) x.Size).
                           ToList()

            For Each pair In sorted
                If pair.Size <= 0 Then
                    parts.Add($"{pair.Unit.Name} (0 - DEAD)")
                Else
                    parts.Add($"{pair.Unit.Name} ({pair.Size})")
                End If
            Next

            Dim totalNow As Integer = sizes.Sum()
            sb.AppendLine($"{snapA.Race} Army: {String.Join(", ", parts)} | Total: {totalNow}")

            If i < startSnapshot.Count - 1 Then sb.AppendLine()
        Next
        sb.AppendLine(New String("-"c, 60))

        ' ---------- Retreats ----------
        If battleLog IsNot Nothing AndAlso battleLog.Retreats IsNot Nothing AndAlso battleLog.Retreats.Count > 0 Then
            sb.AppendLine("--- Retreats ---")
            Dim seen As New HashSet(Of Army)
            For Each ra In battleLog.Retreats
                If Not seen.Contains(ra) Then
                    sb.AppendLine($"Army of {ra.Race} sent back to home at ({ra.X},{ra.Y}). Remaining soldiers: {ra.TotalSoldiers}")
                    seen.Add(ra)
                End If
            Next
            sb.AppendLine()
        End If

        ' ---------- Result ----------
        Dim resultText As String = ""
        Dim retreatRaces As New HashSet(Of String)(
        If(battleLog Is Nothing, Enumerable.Empty(Of String)(), battleLog.Retreats.Select(Function(a) a.Race)),
        StringComparer.OrdinalIgnoreCase
    )

        Dim survivors = mergedArmies.Where(Function(ma) Not retreatRaces.Contains(ma.Race)).ToList()

        If survivors.Count = 1 AndAlso retreatRaces.Count >= 1 Then
            resultText = $"{survivors(0).Race} Victory"
        ElseIf survivors.Count = 0 AndAlso retreatRaces.Count >= 1 Then
            resultText = "Draw (All armies forced to retreat)"
        Else
            resultText = "Battle unresolved (multiple survivors)"
        End If

        sb.AppendLine("=== End of Battle Summary ===")
        sb.AppendLine(resultText)

        Return sb.ToString()
    End Function



    Private Function CloneArmiesForSnapshot(src As List(Of Army)) As List(Of Army)
        Dim clone As New List(Of Army)
        For Each a In src
            Dim na As New Army With {.Race = a.Race, .X = a.X, .Y = a.Y}
            For Each u In a.Units
                na.Units.Add(New Unit(u)) ' use the clone constructor
            Next
            clone.Add(na)
        Next
        Return clone
    End Function



    ' =======================
    ' === Phase Summaries ===
    ' =======================

    ' Small container for each race's flavour words
    Private Class RaceVoice
        Public Property Adjectives As String()
        Public Property VerbsRanged As String()
        Public Property VerbsCharge As String()
        Public Property VerbsMelee As String()
        Public Property VerbsChase As String()
    End Class

    ' Voice packs by race (case-insensitive)
    Private ReadOnly voicePacks As New Dictionary(Of String, RaceVoice)(StringComparer.OrdinalIgnoreCase) From {
    {"elf", New RaceVoice With {
        .Adjectives = {"moonlit", "silver", "keen", "leaf-shadowed", "swift"},
        .VerbsRanged = {"loosed", "laced", "sang through", "draped volleys over"},
        .VerbsCharge = {"swept", "glided into", "broke the line of"},
        .VerbsMelee = {"cut", "parted", "slipped past", "unraveled"},
        .VerbsChase = {"harried", "shadowed", "unmade the retreat of"}
    }},
    {"dwarf", New RaceVoice With {
        .Adjectives = {"stonebound", "stormforged", "ironclad", "grim"},
        .VerbsRanged = {"pelted", "hammered", "drummed upon"},
        .VerbsCharge = {"thundered into", "smashed", "drove against"},
        .VerbsMelee = {"hewed", "ground down", "broke"},
        .VerbsChase = {"pressed", "trod down", "cornered"}
    }},
    {"orc", New RaceVoice With {
        .Adjectives = {"blooded", "brutal", "ragged", "fang-marked"},
        .VerbsRanged = {"peppered", "tore into", "hissed across"},
        .VerbsCharge = {"crashed into", "slammed", "overran"},
        .VerbsMelee = {"hacked", "battered", "tore through"},
        .VerbsChase = {"hunted", "ran down", "harried"}
    }},
    {"human", New RaceVoice With {
        .Adjectives = {"steelclad", "drilled", "steadfast", "bright-bannered"},
        .VerbsRanged = {"volleyed at", "kept up steady fire on", "measured fire against"},
        .VerbsCharge = {"wheeled into", "drove at", "struck in order at"},
        .VerbsMelee = {"pressed", "locked shields with", "ground forward against"},
        .VerbsChase = {"pursued", "cut off", "sealed the rout of"}
    }}
}

    ' Deterministic small RNG per (turn, phase)
    Private Function PhaseRng(phaseIndex As Integer) As Random
        ' currentTurnNumber already exists in your Form1
        Dim seed As Integer = (Math.Max(1, TurnNumber) * 97) + (phaseIndex * 17)
        Return New Random(seed)
    End Function

    ' Choose one item deterministically
    Private Function Pick(Of T)(items As IList(Of T), rng As Random) As T
        If items Is Nothing OrElse items.Count = 0 Then Return Nothing
        Return items(rng.Next(items.Count))
    End Function

    ' Number formatting helper
    Private Function Fmt(n As Double) As String
        Return Math.Round(n).ToString("N0")
    End Function

    ' Build a human-readable summary line + a one-line losses footer for the given phase,
    ' using the attacker with the HIGHEST TOTAL CASUALTIES in that phase.
    ' phaseIndex: 0=Ranged, 1=Charge, 2=Melee, 3=Chase (used for deterministic variety)

    Public Function BuildPhaseSummary(battleLog As BattleLog, phase As String, phaseIndex As Integer) As String
        If battleLog Is Nothing _
   OrElse Not battleLog.PhaseEntries.ContainsKey(phase) _
   OrElse battleLog.PhaseEntries(phase).Count = 0 Then
            Return "No significant action in this phase."
        End If

        Dim entries = battleLog.PhaseEntries(phase)
        Dim totalCas = entries.Sum(Function(e) e.Casualties)
        Dim minImpact As Double = totalCas * 0.1 ' must cause at least 10% of casualties

        ' ---------- Special narration for CHASE ----------
        If phase.Equals("Chase", StringComparison.OrdinalIgnoreCase) Then
            Dim lossesByRace As New Dictionary(Of String, Integer)(StringComparer.OrdinalIgnoreCase)
            For Each e In entries
                If e IsNot Nothing AndAlso e.Defender IsNot Nothing AndAlso Not String.IsNullOrEmpty(e.Defender.Race) Then
                    Dim r As String = e.Defender.Race
                    If Not lossesByRace.ContainsKey(r) Then lossesByRace(r) = 0
                    lossesByRace(r) += e.Casualties
                End If
            Next

            ' Find top attacking LC cluster (must meet impact threshold)
            Dim topGroup = entries.
            Where(Function(e) e IsNot Nothing AndAlso e.Attacker IsNot Nothing).
            GroupBy(Function(e) e.Attacker).
            Select(Function(g) New With {
                .Attacker = g.Key,
                .TotalCasualties = g.Sum(Function(e) e.Casualties),
                .Entries = g.ToList()
            }).
            Where(Function(x) x.TotalCasualties >= minImpact).
            OrderByDescending(Function(x) x.TotalCasualties).
            FirstOrDefault()

            If topGroup Is Nothing OrElse topGroup.Attacker Is Nothing Then
                Return "The victors do not press the rout."
            End If

            Dim atkUnit = topGroup.Attacker
            Dim atkRace As String = If(atkUnit.Race, "Unknown")
            Dim defRace As String = "the enemy"
            Dim defRaceGroup = topGroup.Entries.
            Where(Function(e) e.Defender IsNot Nothing AndAlso Not String.IsNullOrEmpty(e.Defender.Race)).
            GroupBy(Function(e) e.Defender.Race, StringComparer.OrdinalIgnoreCase).
            OrderByDescending(Function(gr) gr.Sum(Function(e) e.Casualties)).
            FirstOrDefault()
            If defRaceGroup IsNot Nothing Then defRace = defRaceGroup.Key

            Dim rng = PhaseRng(phaseIndex)
            Dim templates As String() = {
            "The {atk} light cavalry chases the {def} army off the field.",
            "The {atk} light riders run down the {def} host in rout.",
            "The {atk} light riders harry the fleeing {def}, driving them from the field.",
            "The {atk} light cavalry pursues and scatters the {def} line."
        }
            Dim line As String = templates(rng.Next(templates.Length))
            line = line.Replace("{atk}", atkRace.ToLower()).Replace("{def}", defRace.ToLower())

            Dim footer As String = ""
            If lossesByRace.Count > 0 Then
                Dim parts As New List(Of String)
                If lossesByRace.ContainsKey(atkRace) Then parts.Add($"{atkRace}: {Fmt(lossesByRace(atkRace))}")
                If Not atkRace.Equals(defRace, StringComparison.OrdinalIgnoreCase) AndAlso lossesByRace.ContainsKey(defRace) Then
                    parts.Add($"{defRace}: {Fmt(lossesByRace(defRace))}")
                End If
                If parts.Count = 0 Then
                    For Each kvp In lossesByRace.OrderByDescending(Function(p) p.Value).Take(2)
                        parts.Add($"{kvp.Key}: {Fmt(kvp.Value)}")
                    Next
                End If
                footer = $" Losses this phase — {String.Join(", ", parts)}."
            End If

            Return line & footer
        End If
        ' ---------- /Chase special-case ----------

        ' --- Standard phases (Ranged / Charge / Melee) ---
        Dim lossesByRaceStd As New Dictionary(Of String, Integer)(StringComparer.OrdinalIgnoreCase)
        For Each e In entries
            If e IsNot Nothing AndAlso e.Defender IsNot Nothing AndAlso Not String.IsNullOrEmpty(e.Defender.Race) Then
                Dim r As String = e.Defender.Race
                If Not lossesByRaceStd.ContainsKey(r) Then lossesByRaceStd(r) = 0
                lossesByRaceStd(r) += e.Casualties
            End If
        Next

        Dim topStd = entries.
        Where(Function(e) e IsNot Nothing AndAlso e.Attacker IsNot Nothing).
        GroupBy(Function(e) e.Attacker).
        Select(Function(g) New With {
            .Attacker = g.Key,
            .TotalCasualties = g.Sum(Function(e) e.Casualties),
            .TotalFinalDamage = g.Sum(Function(e) e.FinalDamage),
            .Entries = g.ToList()
        }).
        Where(Function(x) x.TotalCasualties >= minImpact).
        OrderByDescending(Function(x) x.TotalCasualties).
        ThenByDescending(Function(x) x.TotalFinalDamage).
        FirstOrDefault()

        If topStd Is Nothing OrElse topStd.Attacker Is Nothing Then
            Return "No significant action in this phase."
        End If

        Dim atkUnitStd = topStd.Attacker
        Dim atkRaceStd As String = If(atkUnitStd.Race, "Unknown")
        Dim defRaceStd As String = "the enemy"
        Dim defRaceGroupStd = topStd.Entries.
        Where(Function(e) e.Defender IsNot Nothing AndAlso Not String.IsNullOrEmpty(e.Defender.Race)).
        GroupBy(Function(e) e.Defender.Race, StringComparer.OrdinalIgnoreCase).
        OrderByDescending(Function(gr) gr.Sum(Function(e) e.Casualties)).
        FirstOrDefault()
        If defRaceGroupStd IsNot Nothing Then defRaceStd = defRaceGroupStd.Key

        Dim voice As RaceVoice = Nothing
        If Not voicePacks.TryGetValue(atkRaceStd, voice) OrElse voice Is Nothing Then
            voice = New RaceVoice With {
            .Adjectives = {"resolute"},
            .VerbsRanged = {"fired upon"},
            .VerbsCharge = {"drove at"},
            .VerbsMelee = {"pressed"},
            .VerbsChase = {"pursued"}
        }
        End If

        Dim rngStd = PhaseRng(phaseIndex)
        Dim adjective As String = Pick(voice.Adjectives, rngStd)

        Dim verb As String = "struck"
        Select Case phase.ToLowerInvariant()
            Case "ranged" : verb = Pick(voice.VerbsRanged, rngStd)
            Case "charge" : verb = Pick(voice.VerbsCharge, rngStd)
            Case "melee" : verb = Pick(voice.VerbsMelee, rngStd)
        End Select

        Dim topEntryByCas = topStd.Entries.
        OrderByDescending(Function(e) e.Casualties).
        ThenByDescending(Function(e) e.FinalDamage).
        First()

        Dim mitPct As Integer = 0
        If topEntryByCas IsNot Nothing AndAlso topEntryByCas.RawDamage > 0 Then
            Dim pct As Double = 1.0 - (topEntryByCas.FinalDamage / topEntryByCas.RawDamage)
            mitPct = CInt(Math.Round(Math.Max(0, Math.Min(1, pct)) * 100.0))
        End If

        Dim casualtiesTxt As String = Fmt(topStd.TotalCasualties)
        Dim headline As String
        If mitPct >= 50 Then
            headline = $"The {adjective} {atkUnitStd.Name} {verb} the {defRaceStd} ranks through heavy resistance ({mitPct}% mitigated), felling {casualtiesTxt}."
        Else
            headline = $"The {adjective} {atkUnitStd.Name} {verb} the {defRaceStd} ranks, felling {casualtiesTxt}."
        End If

        Dim footerStd As String = ""
        If lossesByRaceStd.Count > 0 Then
            Dim parts As New List(Of String)
            If lossesByRaceStd.ContainsKey(atkRaceStd) Then parts.Add($"{atkRaceStd}: {Fmt(lossesByRaceStd(atkRaceStd))}")
            If Not atkRaceStd.Equals(defRaceStd, StringComparison.OrdinalIgnoreCase) AndAlso lossesByRaceStd.ContainsKey(defRaceStd) Then
                parts.Add($"{defRaceStd}: {Fmt(lossesByRaceStd(defRaceStd))}")
            End If
            If parts.Count = 0 Then
                For Each kvp As KeyValuePair(Of String, Integer) In lossesByRaceStd.
                OrderByDescending(Function(p As KeyValuePair(Of String, Integer)) p.Value).
                Take(2)
                    parts.Add($"{kvp.Key}: {Fmt(kvp.Value)}")
                Next
            End If
            footerStd = $" Losses this phase — {String.Join(", ", parts)}."
        End If

        Return headline & footerStd
    End Function


    Private Function PhaseHadCasualties(battleLog As BattleLog, phase As String) As Boolean
        If battleLog Is Nothing Then Return False
        If Not battleLog.PhaseEntries.ContainsKey(phase) Then Return False
        Dim list = battleLog.PhaseEntries(phase)
        If list Is Nothing OrElse list.Count = 0 Then Return False
        Return list.Sum(Function(e) e.Casualties) > 0
    End Function

    ' Return the "home centre" (2 in from the corner) for a race
    Private Function GetHomeForRace(race As String) As Point
        Select Case race.ToLower()
            Case "elf" : Return New Point(2, 2)    ' top-left centre
            Case "dwarf" : Return New Point(22, 2)   ' top-right centre
            Case "orc" : Return New Point(2, 22)   ' bottom-left centre
            Case "human" : Return New Point(22, 22)  ' bottom-right centre
            Case Else : Return New Point(0, 0)
        End Select
    End Function

    Public Sub SendArmyBackToSpawn(army As Army)
        ' Moves the army back to its race's corner-centre (consistent with InitializePlayers)
        Dim home As Point = GetHomeForRace(army.Race)
        army.X = home.X
        army.Y = home.Y

        ' Critical: stop it from moving again this turn
        ClearArmyMoveQueue(army)
        army.RetreatedThisTurn = True
    End Sub

    Public Function GenerateEmpireSummary() As String
        Dim sb As New System.Text.StringBuilder()

        For Each p In Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For
            ' --- Header ---
            sb.AppendLine($"{p.Race} Player (Player {p.PlayerNumber + 1})")
            sb.AppendLine($"Population: {p.Population}")
            sb.AppendLine($"Iron: {p.Iron} (+{p.IronCollectedThisTurn} this turn)")
            sb.AppendLine($"Wood: {p.Wood} (+{p.WoodCollectedThisTurn} this turn)")
            If p.Mithril > 0 Then sb.AppendLine($"Mithril: {p.Mithril}")

            ' Race-specific mount name
            Dim mountName As String
            Select Case p.Race.ToLower()
                Case "elf" : mountName = "Forest Elks"
                Case "dwarf" : mountName = "War Rams"
                Case "orc" : mountName = "Wolves"
                Case "human" : mountName = "Horses"
                Case Else : mountName = "Mounts"
            End Select
            sb.AppendLine($"{mountName}: {p.Mounts} (+{p.MountsCollectedThisTurn} this turn)")

            ' Gold (now using stored properties)
            sb.AppendLine($"Gold: {p.Gold} (+{p.GoldCollectedThisTurn} this turn)")
            sb.AppendLine($"Wages: {p.LastMercWages} this turn")
            sb.AppendLine($"Income: {p.GoldCollectedThisTurn} (Wages: {p.LastMercWages}, Net: {p.GoldCollectedThisTurn - p.LastMercWages})")


            ' --- Trade goods (totals only) ---
            'sb.AppendLine($"Gems: {p.Gems}")
            If p.Xorns > 0 Then sb.AppendLine($"Xorns: {p.Xorns}")
            'sb.AppendLine($"Amber: {p.Amber}")
            'sb.AppendLine($"Wine: {p.Wine}")
            'sb.AppendLine($"Furs: {p.Furs}")
            If p.ElfSlaves > 0 Then sb.AppendLine($"Elf Slaves: {p.ElfSlaves}")
            If p.DwarfSlaves > 0 Then sb.AppendLine($"Dwarf Slaves: {p.DwarfSlaves}")
            If p.HumanSlaves > 0 Then sb.AppendLine($"Human Slaves: {p.HumanSlaves}")

            ' Armies (short summary)
            If p.Armies IsNot Nothing AndAlso p.Armies.Count > 0 Then
                sb.AppendLine("Armies:")
                For i As Integer = 0 To p.Armies.Count - 1
                    Dim a = p.Armies(i)
                    sb.AppendLine($"  Army {i + 1}: {a.TotalSoldiers} soldiers at ({a.X},{a.Y})")
                Next
            Else
                sb.AppendLine("No armies")
            End If

            sb.AppendLine(New String("-"c, 60))
        Next

        Return sb.ToString()
    End Function


    Public Function GetCheapestAvailableUnit(player As Player) As UnitStats
        If player Is Nothing Then Return Nothing

        Dim raceUnits As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName = player.Race)
        If raceUnits Is Nothing OrElse raceUnits.Units Is Nothing OrElse raceUnits.Units.Count = 0 Then
            Return Nothing
        End If

        ' --- Racial preferences ---
        Dim preferredTypes As New List(Of UnitType)
        Select Case player.Race.ToLowerInvariant()
            Case "elf"
                preferredTypes.Add(UnitType.Archer)
            Case "orc"
                preferredTypes.Add(UnitType.LightInfantry)
            Case "dwarf", "human"
                preferredTypes.Add(UnitType.Archer)
                preferredTypes.Add(UnitType.LightInfantry)
            Case Else
                preferredTypes.Add(UnitType.LightInfantry)
        End Select

        ' --- Cost parser now includes Mithril ("X:") ---
        Dim parseCosts =
    Function(costSpec As String) As (i As Integer, w As Integer, m As Integer, x As Integer, total As Integer)
        Dim ri As Integer = 0, rw As Integer = 0, rm As Integer = 0, rx As Integer = 0
        If Not String.IsNullOrWhiteSpace(costSpec) Then
            For Each part In costSpec.Split(","c)
                Dim t As String = part.Trim().ToUpperInvariant()
                If t.StartsWith("I:") Then ri += CInt(Val(t.Substring(2)))
                If t.StartsWith("W:") Then rw += CInt(Val(t.Substring(2)))
                If t.StartsWith("M:") Then rm += CInt(Val(t.Substring(2)))
                If t.StartsWith("X:") Then rx += CInt(Val(t.Substring(2)))  ' Mithril
            Next
        End If
        Return (ri, rw, rm, rx, ri + rw + rm + rx)
    End Function

        ' --- Collect affordable candidates ---
        Dim preferredAffordable As New List(Of (u As UnitStats, tot As Integer, m As Integer, i As Integer, w As Integer, x As Integer))()
        Dim globalAffordable As New List(Of (u As UnitStats, tot As Integer, m As Integer, i As Integer, w As Integer, x As Integer))()

        For Each u In raceUnits.Units
            Dim c = parseCosts(u.Cost)

            ' Mithril gate: only allow recruiting Mithril units if stockpile ≥ 50
            If c.x > 0 AndAlso player.Mithril < 50 Then
                Continue For
            End If

            Dim canAfford As Boolean =
            (c.i = 0 OrElse player.Iron >= c.i) AndAlso
            (c.w = 0 OrElse player.Wood >= c.w) AndAlso
            (c.m = 0 OrElse player.Mounts >= c.m) AndAlso
            (c.x = 0 OrElse player.Mithril >= c.x)

            If Not canAfford Then Continue For

            Dim tup = (u, c.total, c.m, c.i, c.w, c.x)
            If preferredTypes.Contains(u.Type) Then
                preferredAffordable.Add(tup)
            Else
                globalAffordable.Add(tup)
            End If
        Next

        ' --- If Dwarf, strongly prefer Mithril-using units when affordable ---
        If player.Race.Equals("Dwarf", StringComparison.OrdinalIgnoreCase) Then
            Dim mithrilUnits = preferredAffordable.Where(Function(t) t.x > 0).ToList()
            If mithrilUnits.Count > 0 Then
                Dim elite = mithrilUnits.OrderByDescending(Function(t) t.tot).First().u
                Return elite
            End If
        End If

        ' --- Pick best normal candidate ---
        Dim pickBest =
    Function(lst As List(Of (u As UnitStats, tot As Integer, m As Integer, i As Integer, w As Integer, x As Integer))) As UnitStats
        If lst Is Nothing OrElse lst.Count = 0 Then Return Nothing
        Dim best = lst.
            OrderBy(Function(t) t.tot).
            ThenBy(Function(t) t.m).
            ThenBy(Function(t) t.i).
            ThenBy(Function(t) t.w).
            ThenBy(Function(t) t.u.Name, StringComparer.OrdinalIgnoreCase).
            First()
        Return best.u
    End Function

        Dim chosen As UnitStats = pickBest(preferredAffordable)
        If chosen Is Nothing Then chosen = pickBest(globalAffordable)

        ' --- FINAL FALLBACK ---
        If chosen Is Nothing Then
            chosen = raceUnits.Units.
            OrderBy(Function(u) u.Cost.Length).
            ThenBy(Function(u) u.Name, StringComparer.OrdinalIgnoreCase).
            FirstOrDefault()
        End If

        Return chosen
    End Function


    ' === Trade Goods Production ===
    Public Sub ProduceTradeGoods()
        For Each p In Players
            If Not CanAct(p) Then Continue For
            Select Case p.Race.ToLowerInvariant()
                Case "dwarf"
                    Dim produced As Integer = Math.Max(1, p.Population \ 2000)
                    p.Gems += produced

                    ' === Xorn discovery chance (based on gems produced) ===
                    ' Roll % chance equal to gems produced
                    If rnd.Next(100) < produced Then
                        p.Xorns += 1
                        ' Announce discovery (later you can hook this into your report system) - TO DO
                        'Debug.WriteLine($"[XORN] {p.Race} Player discovered a Xorn! Total Xorns: {p.Xorns}")
                    End If

                    ' === Xorn bonus gems each turn ===
                    If p.Xorns > 0 Then
                        Dim bonus As Integer = Enumerable.Range(1, p.Xorns).Sum(Function(i) rnd.Next(1, 11))
                        p.Gems += bonus
                        'Debug.WriteLine($"[XORN] {p.Race} Player gained {bonus} bonus Gems from {p.Xorns} Xorn(s). (Total: {p.Gems})")
                    End If

                Case "elf"
                    Dim produced As Integer = Math.Max(1, p.Population \ 2000)
                    p.Amber += produced

                Case "human"
                    Dim produced As Integer = Math.Max(1, p.Population \ 2000)
                    p.Wine += produced

                Case "orc"
                    Dim produced As Integer = Math.Max(1, p.Population \ 2000)
                    p.Furs += produced
            End Select
        Next
    End Sub


    Private Const FeeRate As Double = 0.1

    Private Function ApplyDiminishingReturns(goldHeld As Integer, profit As Double) As Integer
        Dim efficiency As Double = 1000.0 / (1000.0 + goldHeld)
        Return CInt(profit * efficiency)
    End Function

    Private Function AdjustPrice(oldPrice As Double, demand As Integer, supply As Integer, factor As Integer) As Double
        Dim change As Double = (demand - supply) / factor
        Dim newPrice As Double = oldPrice * (1 + change)
        ' clamp between -25% and +25% per turn
        newPrice = Math.Max(oldPrice * 0.75, Math.Min(oldPrice * 1.25, newPrice))
        ' random drift ±5%
        'Dim rnd As New Random()
        newPrice *= 1 + (rnd.NextDouble() * 0.1 - 0.05)
        Return Math.Max(1, newPrice) ' never drop below 1
    End Function



#Region "Mercenary Stuff"

    Public Shared CurrentMercOffer As MercenaryArmy

    Private MercPriceLevel As Integer = 0

    Public Class MercenaryStack
        ' For normal merc units
        Public Property Template As UnitStats
        Public Property Count As Integer

        ' For hero mercs (summoners, champions, etc.)
        Public Property Hero As Unit

    End Class

    Public Class MercenaryArmy
        Public Property Name As String
        Public Property Faction As String
        Public Property Units As New List(Of MercenaryStack)() ' mix of normal and hero entries
        Public Property MinBid As Integer
        Public Property TrainingBonus As Double = 0.0    ' 0 = none, 0.1 = +10%, etc.


        Public ReadOnly Property TotalSize As Integer
            Get
                ' Heroes count as 1, normal stacks use Count
                Return Units.Sum(Function(s) If(s.Hero IsNot Nothing, 1, s.Count))
            End Get
        End Property

        Public Overrides Function ToString() As String
            Dim parts As New List(Of String)

            For Each s In Units
                If s.Hero IsNot Nothing Then
                    parts.Add($"{s.Hero.Name} (Level {s.Hero.Level})")
                ElseIf s.Template IsNot Nothing Then
                    parts.Add($"{s.Template.Name} ({s.Count})")
                End If
            Next

            Return $"{Faction} Mercenary Band — {String.Join(", ", parts)} | Total: {TotalSize}, Min Bid: {MinBid} gold"
        End Function
    End Class

    Private Sub ApplyMercBids()
        If Players Is Nothing Then Exit Sub

        For Each p In Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For

            Dim race As String = p.Race.ToLowerInvariant()
            Dim suffix As String = Char.ToUpper(race(0)) & race.Substring(1).ToLower()

            ' --- Find controls for this player ---
            Dim numBid As NumericUpDown = TryCast(Me.Controls("numMercBid" & suffix), NumericUpDown)
            Dim cmbArmy As ComboBox = TryCast(Me.Controls("cmbArmy" & suffix & "Merc"), ComboBox)

            If numBid Is Nothing OrElse cmbArmy Is Nothing Then Continue For

            ' --- Read the numeric bid ---
            Dim bidAmount As Integer = CInt(numBid.Value)

            ' --- Clamp to available gold ---
            If bidAmount > p.Gold Then
                bidAmount = p.Gold
                numBid.Value = bidAmount  ' optional: visually update
                'Debug.WriteLine($"[MERC] {p.Race} bid reduced to {bidAmount} (max gold available).")
            End If

            p.CurrentBid = bidAmount

            ' --- Skip if no bid ---
            If bidAmount <= 0 Then
                p.TargetArmyForMercs = Nothing
                Continue For
            End If

            ' --- Resolve the target army ---
            Dim targetArmy As Army = Nothing
            Dim selectedItem As ArmyListItem = TryCast(cmbArmy.SelectedItem, ArmyListItem)

            If selectedItem IsNot Nothing AndAlso selectedItem.Index >= 0 AndAlso selectedItem.Index < p.Armies.Count Then
                targetArmy = p.Armies(selectedItem.Index)
            ElseIf Not String.IsNullOrWhiteSpace(cmbArmy.Text) Then
                targetArmy = p.Armies.FirstOrDefault(Function(a) a.Name.Equals(cmbArmy.Text, StringComparison.OrdinalIgnoreCase))
            End If

            ' --- Store the chosen army ---
            p.TargetArmyForMercs = targetArmy

            ' --- Reset UI for next turn ---
            numBid.Value = 0
            cmbArmy.SelectedIndex = -1
        Next
    End Sub

    Private Sub AwardMercenariesToPlayer(offer As MercenaryArmy, winner As Player)
        If offer Is Nothing OrElse winner Is Nothing Then Exit Sub
        If winner.Armies Is Nothing OrElse winner.Armies.Count = 0 Then Exit Sub

        ' === Determine target army ===
        Dim target As Army = Nothing

        ' Human players: use the army chosen via ApplyMercBids (if still valid)
        If Not winner.AIControlled AndAlso winner.TargetArmyForMercs IsNot Nothing Then
            Dim idx As Integer = winner.Armies.IndexOf(winner.TargetArmyForMercs)
            If idx >= 0 Then target = winner.Armies(idx)
        End If

        ' AI players: random army 0..2 (safe if fewer than 3 armies)
        If target Is Nothing AndAlso winner.AIControlled Then
            Dim rnd As New Random()
            Dim upper As Integer = Math.Min(3, winner.Armies.Count) ' Next upper bound is exclusive
            Dim randIndex As Integer = rnd.Next(0, upper)           ' 0..2 if 3+, otherwise 0..Count-1
            target = winner.Armies(randIndex)
        End If

        ' Fallback
        If target Is Nothing Then target = winner.Armies(0)

        ' Ensure Units list exists
        If target.Units Is Nothing Then target.Units = New List(Of Unit)()

        ' Build a concise composition string as we add/merge
        Dim parts As New List(Of String)

        ' Track total men and total training effect
        Dim totalNewMen As Integer = 0
        Dim weightedTraining As Double = 0.0

        For Each s In offer.Units
            If s Is Nothing Then Continue For

            If s.Hero IsNot Nothing Then
                ' === Hero mercenary ===
                s.Hero.IsMercenary = True
                target.Units.Add(s.Hero)
                parts.Add($"{s.Hero.Name} (Lvl {s.Hero.Level})")

            ElseIf s.Template IsNot Nothing Then
                ' === Normal mercenary ===
                Dim t As UnitStats = s.Template
                Dim count As Integer = s.Count
                totalNewMen += count

                ' Accumulate weighted training bonus if this template has one
                If t.TrainingBonus > 0 Then
                    weightedTraining += t.TrainingBonus * count
                End If

                ' Merge if this type already exists
                Dim existing = target.Units.FirstOrDefault(Function(u) u.Name = t.Name AndAlso u.Type = t.Type)
                If existing IsNot Nothing Then
                    existing.Size += count
                    existing.IsMercenary = True
                Else
                    Dim newU As New Unit(t, winner.Race, count)
                    newU.IsMercenary = True
                    target.Units.Add(newU)
                End If

                parts.Add($"{t.Name} x{count}")
            End If
        Next

        ' === Apply combined training from unit-level bonuses ===
        If totalNewMen > 0 AndAlso weightedTraining > 0 Then
            Dim oldMen As Integer = target.TotalSoldiers
            Dim totalMen As Integer = Math.Max(1, oldMen + totalNewMen)
            Dim averageNewTraining As Double = weightedTraining / totalNewMen
            Dim combined As Double = ((target.TrainingLevel * oldMen) + (averageNewTraining * totalNewMen)) / totalMen
            target.TrainingLevel = combined
            Debug.WriteLine($"[MERC] Combined merc training {averageNewTraining:P0}; new army training level = {target.TrainingLevel:P0}")
        End If

        ' === Debug: where it went and what it was ===
        Dim armyIndex As Integer = winner.Armies.IndexOf(target)
        If armyIndex < 0 Then armyIndex = 0
        Debug.WriteLine($"[MERC] Awarded to {winner.Race} (Player {winner.PlayerNumber + 1}) → Army #{armyIndex + 1} '{target.Name}' at ({target.X},{target.Y}). Faction: {offer.Faction}")
        Debug.WriteLine($"[MERC] Composition: {String.Join(", ", parts)}")

        ' Cleanup selection for next turn
        winner.TargetArmyForMercs = Nothing
    End Sub


    Private Function GenerateMercenaryOffer(turnNumber As Integer) As MercenaryArmy
        Dim rnd As New Random()

        ' === 1. Budget scales directly with current market level ===
        ' MercPriceLevel starts at 50 and increases by 50 each time an army is bought.
        Dim maxBudget As Integer = MercPriceLevel

        Dim mercFactions As String() = {
        "WarriorMonks", "Skulkrin", "Barbarians", "Werecreatures", "Harpies",
        "Cultists", "Demons", "Undead", "Golems", "Elementals",
        "Bandits", "Nomads", "Freeblades", "Sellswords"
    }

        Dim mercArmyNormal As MercenaryArmy = Nothing
        Dim outerGuard As Integer = 1000 ' prevent infinite faction loop

        Do While outerGuard > 0
            outerGuard -= 1

            ' === 2. Pick a random faction ===
            Dim faction As String = mercFactions(rnd.Next(mercFactions.Length))
            Dim roster As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName.Equals(faction, StringComparison.OrdinalIgnoreCase))
            If roster Is Nothing OrElse roster.Units.Count = 0 Then Continue Do

            mercArmyNormal = New MercenaryArmy With {
            .Faction = faction,
            .Units = New List(Of MercenaryStack),
            .MinBid = MercPriceLevel        ' Minimum bid equals the current market level
        }

            Dim remaining As Integer = maxBudget
            Dim cheapestPower As Integer = roster.Units.Min(Function(u) u.Power)
            Dim hasFodder As Boolean = roster.Units.Any(Function(u) u.Power <= 2)

            ' --- Allowed block sizes ---
            Dim blockSizes As Integer()
            If hasFodder Then
                blockSizes = {10} ' Only groups of 10 for fodder-based factions
            Else
                blockSizes = {10, 5, 1} ' fallback allowed for elite units
            End If

            Dim pickedAnything As Boolean = False
            Dim innerGuard As Integer = 10000

            ' === 3. Randomly assemble units up to the budget ===
            Do While remaining >= cheapestPower AndAlso innerGuard > 0
                innerGuard -= 1

                ' Weighted bias toward earlier (usually weaker) units
                Dim weighted As New List(Of UnitStats)
                For i As Integer = 0 To roster.Units.Count - 1
                    Dim u = roster.Units(i)
                    Dim weight As Integer = Math.Max(1, (roster.Units.Count - i) * 5)
                    For n As Integer = 1 To weight
                        weighted.Add(u)
                    Next
                Next

                Dim pick As UnitStats = weighted(rnd.Next(weighted.Count))

                ' Skip units that don't fit budget
                If pick.Power <= 0 OrElse pick.Power > remaining Then Continue Do

                ' Find largest block that fits
                Dim count As Integer = 0
                For Each b In blockSizes
                    If pick.Power * b <= remaining Then
                        count = b
                        Exit For
                    End If
                Next

                If count = 0 Then Continue Do

                ' Add to army
                Dim existing = mercArmyNormal.Units.FirstOrDefault(Function(s) s.Template IsNot Nothing AndAlso s.Template.Name = pick.Name)
                If existing IsNot Nothing Then
                    existing.Count += count
                Else
                    mercArmyNormal.Units.Add(New MercenaryStack With {.Template = pick, .Count = count})
                End If

                pickedAnything = True
                remaining -= pick.Power * count
            Loop

            If innerGuard = 0 Then
                'Debug.WriteLine("Guard triggered in GenerateMercenaryOffer inner loop — possible bad roster config.")
            End If

            ' === 4. Guarantee at least one unit ===
            If Not pickedAnything Then
                If cheapestPower > maxBudget Then
                    mercArmyNormal = Nothing
                    Continue Do
                End If

                Dim cheapest = roster.Units.Where(Function(u) u.Power <= maxBudget).OrderBy(Function(u) u.Power).First()
                mercArmyNormal.Units.Add(New MercenaryStack With {.Template = cheapest, .Count = 1})
            End If

            ' === 5. Return the completed mercenary army ===
            If mercArmyNormal.Units.Count > 0 Then Exit Do
        Loop

        If outerGuard = 0 Then
            'Debug.WriteLine("Guard triggered in GenerateMercenaryOffer outer loop — possible issue with faction selection.")
        End If

        ' === Reveal this new mercenary offer to all human players ===
        RevealMercenaryOffer(mercArmyNormal, Players)

        Return mercArmyNormal
    End Function


    Private Sub ResolveMercenaryAuction(bids As Dictionary(Of Player, Integer))
        ' === 1. Validate offer ===
        If CurrentMercOffer Is Nothing OrElse CurrentMercOffer.Units.Count = 0 Then Exit Sub



        ' === 2. Handle no bids ===
        If bids Is Nothing OrElse bids.Count = 0 Then
            'Debug.WriteLine("No bids received; mercenary offer dismissed.")
            CurrentMercOffer = Nothing
            Exit Sub
        End If

        ' === 3. Filter invalid bids ===
        Dim candidateBids = bids.
        Where(Function(kv) kv.Value >= CurrentMercOffer.MinBid AndAlso
                         kv.Key IsNot Nothing AndAlso
                         kv.Key.Gold >= kv.Value).
        ToList()

        If candidateBids.Count = 0 Then
            'Debug.WriteLine("No valid bids (must meet MinBid and be within available gold); mercenary offer dismissed.")
            CurrentMercOffer = Nothing
            Exit Sub
        End If

        ' === 4. Winner selection ===
        Dim topAmount = candidateBids.Max(Function(kv) kv.Value)
        Dim topBidders = candidateBids.Where(Function(kv) kv.Value = topAmount).ToList()

        Dim winnerKV As KeyValuePair(Of Player, Integer)
        If topBidders.Count = 1 Then
            winnerKV = topBidders(0)
        Else
            ' Tie-breaker: richest bidder, then lowest PlayerNumber
            winnerKV = topBidders.
            OrderByDescending(Function(kv) kv.Key.Gold).
            ThenBy(Function(kv) kv.Key.PlayerNumber).
            First()
        End If

        Dim winner As Player = winnerKV.Key
        Dim amount As Integer = winnerKV.Value

        ' === 5. Deduct gold and award ===
        winner.Gold -= amount
        If winner.Gold < 0 Then winner.Gold = 0

        AwardMercenariesToPlayer(CurrentMercOffer, winner)
        'Debug.WriteLine($"Mercenaries hired by Player {winner.PlayerNumber} ({winner.Race}) for {amount} gold!")

        ' === 6. Market inflation: raise price level by 50 ===
        MercPriceLevel += 50
        'If MercPriceLevel > 2000 Then MercPriceLevel = 2000  ' Don't want a cap.

        ' === 7. Clear current offer ===
        CurrentMercOffer = Nothing
    End Sub

    Private Sub ResolveBiddingPhase()
        ' Resolve last turn's mercenary auction
        If CurrentMercOffer IsNot Nothing Then
            Dim bids As New Dictionary(Of Player, Integer)

            For Each p In Players
                ' AI generates its bid now if none set
                If p.AIControlled AndAlso p.CurrentBid <= 0 Then
                    p.CurrentBid = GenerateAIBid(p, CurrentMercOffer)
                End If

                ' Only accept valid bids
                If p.CurrentBid > 0 AndAlso p.CurrentBid <= p.Gold Then
                    ' --- Wage cap check ---
                    If CanAffordMercenaries(p, CurrentMercOffer) Then
                        bids(p) = p.CurrentBid
                    Else
                        Debug.WriteLine($"[MERC] {p.Race} (Player {p.PlayerNumber + 1}) bid {p.CurrentBid} but cannot afford wages. Bid rejected.")
                    End If
                End If
            Next

            ResolveMercenaryAuction(bids)

            ' Reset bids
            For Each p In Players
                p.CurrentBid = 0
            Next
        End If
    End Sub

    Private Function CanAffordMercenaries(p As Player, offer As MercenaryArmy) As Boolean
        If p Is Nothing OrElse offer Is Nothing Then Return False

        ' Current ongoing wages for this player
        Dim currentWages As Integer = CalculateMercenaryWages(p)

        ' Wages for the offered army (sum of Power × Count)
        Dim newWages As Integer = 0
        For Each s In offer.Units
            If s Is Nothing Then Continue For

            If s.Hero IsNot Nothing Then
                ' Hero wage = their Power value
                ' newWages += s.Hero.Power
            ElseIf s.Template IsNot Nothing Then
                newWages += s.Template.Power * s.Count
            End If
        Next

        ' Projected total wages if this army is added
        Dim projected As Integer = currentWages + newWages

        ' Gold income per turn = population / 10
        Dim income As Integer = p.Population \ 10

        ' Can only afford if projected wages fit into income
        Return projected <= income
    End Function

    Private Function GenerateAIBid(p As Player, offer As MercenaryArmy) As Integer
        If p Is Nothing OrElse offer Is Nothing OrElse offer.Units.Count = 0 Then
            Return 0
        End If

        ' === 1. Skip if broke ===
        If p.Gold <= 0 Then Return 0

        Dim rnd As New Random()

        ' === 2. Enforce minimum bid requirement ===
        Dim minBid As Integer = Math.Max(offer.MinBid, 1)

        ' If AI cannot meet the minimum bid, skip bidding
        If p.Gold < minBid Then
            'Debug.WriteLine($"[MERC] {p.Race} (Player {p.PlayerNumber + 1}) cannot afford MinBid {minBid}.")
            Return 0
        End If

        ' === 3. Add a small random aggression multiplier (10–30%) ===
        Dim aggression As Double = 1.0 + (rnd.NextDouble() * 0.3)  ' between 1.0 and 1.3
        Dim desiredBid As Integer = CInt(minBid * aggression)

        ' === 4. Cap at available gold ===
        Dim finalBid As Integer = Math.Min(desiredBid, p.Gold)

        ' Ensure at least the MinBid
        If finalBid < minBid Then finalBid = minBid

        'Debug.WriteLine($"[MERC] {p.Race} (Player {p.PlayerNumber + 1}) bids {finalBid} gold (MinBid {minBid}, aggression {aggression:F2}).")

        Return finalBid
    End Function


#End Region


    Private Sub UpdateArmiesReport()
        rtbArmies.Clear()

        For Each p As Player In Players
            If Not CanAct(p) Then Continue For
            If p.Armies Is Nothing OrElse p.Armies.Count = 0 Then Continue For

            For Each a As Army In p.Armies
                rtbArmies.AppendText($"{p.Race} Army at ({a.X},{a.Y})" & vbCrLf)

                ' --- Compact unit listing ---
                Dim ordered = a.Units.OrderByDescending(Function(u) u.Size).ToList()

                ' Show up to 5 units line by line
                For i As Integer = 0 To Math.Min(4, ordered.Count - 1)
                    Dim u = ordered(i)
                    rtbArmies.AppendText($"  {u.Name} ({u.Size})" & vbCrLf)
                Next

                ' If more than 5, clump the rest
                If ordered.Count > 5 Then
                    Dim clump = String.Join(", ", ordered.Skip(5).Select(Function(u) $"{u.Name} ({u.Size})"))
                    rtbArmies.AppendText("  " & clump & vbCrLf)
                End If

                ' Total line
                rtbArmies.AppendText($"  Total: {a.TotalSoldiers}" & vbCrLf & vbCrLf)
            Next
        Next
    End Sub
    Private Function CalculateMercenaryWages(p As Player) As Integer
        If p Is Nothing OrElse p.Armies Is Nothing Then Return 0

        Dim wages As Integer = 0

        For Each a In p.Armies
            If a.Units Is Nothing Then Continue For
            For Each u In a.Units
                If u IsNot Nothing AndAlso u.IsMercenary Then
                    wages += u.Power * u.Size
                    'Debug.WriteLine($"[WAGE-CHECK] {p.Race} army at ({a.X},{a.Y}): {u.Name} x{u.Size} costing {u.Power * u.Size}")
                End If
            Next
        Next

        Return wages
    End Function

    Private Sub PayMercenaryWages()
        For Each p In Players
            If Not CanAct(p) Then Continue For
            Dim wages As Integer = CalculateMercenaryWages(p)
            p.LastMercWages = wages

            If wages > 0 Then
                p.Gold -= wages
                If p.Gold < 0 Then p.Gold = 0 ' prevent negative gold

                'Debug.WriteLine($"[WAGES] {p.Race} (Player {p.PlayerNumber + 1}) paid {wages} gold in mercenary wages. Remaining gold: {p.Gold}")
            End If
        Next
    End Sub

    ' === Helper: calculate wages for a mercenary offer before hire ===
    Private Function CalculateMercenaryOfferWages(offer As MercenaryArmy) As Integer
        If offer Is Nothing OrElse offer.Units Is Nothing Then Return 0

        Dim wages As Integer = 0
        For Each s In offer.Units
            If s Is Nothing Then Continue For

            If s.Hero IsNot Nothing Then
                wages += s.Hero.Power
            ElseIf s.Template IsNot Nothing Then
                wages += s.Template.Power * s.Count
            End If
        Next

        Return wages
    End Function


    'Private Sub btnKillPlayer_Click(sender As Object, e As EventArgs) Handles btnKillPlayer.Click
    '    ' === DEBUG ONLY ===
    '    Dim input As String = InputBox("Enter player number to eliminate (1–4):", "Eliminate Player", "1")
    '    Dim idx As Integer
    '    If Not Integer.TryParse(input, idx) Then
    '        MessageBox.Show("Invalid number.")
    '        Exit Sub
    '    End If
    '    idx -= 1 ' convert to zero-based index

    '    If idx < 0 OrElse idx >= Players.Count Then
    '        MessageBox.Show("That player number doesn't exist.")
    '        Exit Sub
    '    End If

    '    Dim p As Player = Players(idx)
    '    If p Is Nothing Then
    '        MessageBox.Show("Player not found.")
    '        Exit Sub
    '    End If

    '    ' === Force elimination ===
    '    p.IsEliminated = True
    '    'rtbGameInfo.AppendText($"[DEBUG] Forcibly eliminating {p.Race} (Player {p.PlayerNumber + 1})..." & vbCrLf)

    '    ' === Full cleanup (neutralize tiles + wipe armies) ===
    '    NeutralizePlayerMap(p)
    '    NeutralizePlayerArmies(p)

    '    ' === Run normal defeat logic just in case ===
    '    CheckAndApplyDefeat(p)

    '    ' === Refresh visuals ===
    '    pnlMap.Invalidate()       ' Redraw map
    '    UpdateArmiesReport()      ' Refresh army list

    '    MessageBox.Show($"{p.Race} eliminated for testing. Check map and panels.")
    'End Sub


    ' ===========================================================
    ' === END-OF-TURN SAFETY CAPTURE =============================
    ' ===========================================================
    Private Sub SafetyCaptureReconciliation()
        If Players Is Nothing OrElse Map Is Nothing Then Exit Sub

        Dim rows As Integer = Map.GetLength(0)
        Dim cols As Integer = Map.GetLength(1)

        Dim claims As New Dictionary(Of String, Integer)

        'Debug.WriteLine("=== SafetyCaptureReconciliation ===")

        ' === Normal capture attempts ===
        For Each p In Players
            If p Is Nothing OrElse p.IsEliminated Then Continue For
            If p.Armies Is Nothing Then Continue For

            For Each a In p.Armies
                If a Is Nothing Then Continue For
                If a.TotalSoldiers < 500 Then Continue For      ' <--- NEW: minimum capture size
                If a.RetreatedThisTurn Then Continue For

                Dim x As Integer = a.X
                Dim y As Integer = a.Y
                If x < 0 OrElse y < 0 OrElse x >= rows OrElse y >= cols Then Continue For

                ' Skip if enemy present (safety)
                If EnemyPresentAt(x, y, p.PlayerNumber) Then Continue For

                ' Capture only if orthogonally adjacent to owned land
                If HasAdjacentOwned4(p.PlayerNumber, x, y) Then
                    Dim key As String = KeyXY(x, y)
                    If Not claims.ContainsKey(key) Then
                        claims.Add(key, p.PlayerNumber)
                    ElseIf claims(key) <> p.PlayerNumber Then
                        claims(key) = -2 ' contested
                    End If
                End If
            Next
        Next

        ' === Apply capture claims ===
        For Each kvp In claims
            Dim x As Integer, y As Integer
            ParseXY(kvp.Key, x, y)
            Dim val As Integer = kvp.Value

            If val >= 0 Then
                Dim prevOwner As Integer = Map(x, y, 1)
                If prevOwner <> val Then
                    Map(x, y, 1) = val
                    Dim raceName As String = SafeRaceName(val)
                    'Debug.WriteLine($"[SAFETY CAPTURE] {raceName} claims ({x},{y}) (prev {prevOwner})")
                    'If rtbGameInfo IsNot Nothing Then
                    'rtbGameInfo.AppendText($"[SAFETY CAPTURE] {raceName} claims ({x},{y})." & vbCrLf)
                    'End If
                End If
            Else
                'Debug.WriteLine($"[SAFETY] Contested tile at ({x},{y}); no change.")
            End If
        Next

        ' === Capital auto-flip rule ===
        If capitals IsNot Nothing Then
            For Each kv In capitals
                Dim ownerId As Integer = kv.Key
                Dim cap As Point = kv.Value
                Dim cx As Integer = cap.X
                Dim cy As Integer = cap.Y

                If ownerId < 0 OrElse ownerId >= Players.Count Then Continue For

                Dim p As Player = Players(ownerId)
                If p Is Nothing OrElse p.IsEliminated Then Continue For   ' <-- NEW: skip eliminated players

                ' Check if any enemy army with ≥500 is still on it
                Dim blocked As Boolean = False
                For Each q In Players
                    If q Is Nothing OrElse q.PlayerNumber = ownerId Then Continue For
                    If q.Armies Is Nothing Then Continue For
                    For Each a In q.Armies
                        If a Is Nothing Then Continue For
                        If a.X = cx AndAlso a.Y = cy AndAlso a.TotalSoldiers >= 500 Then
                            blocked = True
                            Exit For
                        End If
                    Next
                    If blocked Then Exit For
                Next

                ' If not blocked, restore to rightful owner
                If Not blocked AndAlso Map(cx, cy, 1) <> ownerId Then
                    Map(cx, cy, 1) = ownerId
                    Dim raceName As String = SafeRaceName(ownerId)
                    'Debug.WriteLine($"[CAPITAL RESTORE] {raceName} recovers its citadel at ({cx},{cy})")
                    'If rtbGameInfo IsNot Nothing Then
                    'rtbGameInfo.AppendText($"[CAPITAL RESTORE] {raceName} regains citadel control." & vbCrLf)
                    'End If
                End If
            Next
        End If


        If pnlMap IsNot Nothing Then pnlMap.Invalidate()
    End Sub


    ' ===========================================================
    ' === HELPER FUNCTIONS =======================================
    ' ===========================================================
    Private Function HasAdjacentOwned4(playerId As Integer, x As Integer, y As Integer) As Boolean
        Dim rows As Integer = Map.GetLength(0)
        Dim cols As Integer = Map.GetLength(1)
        If x > 0 AndAlso Map(x - 1, y, 1) = playerId Then Return True
        If x < rows - 1 AndAlso Map(x + 1, y, 1) = playerId Then Return True
        If y > 0 AndAlso Map(x, y - 1, 1) = playerId Then Return True
        If y < cols - 1 AndAlso Map(x, y + 1, 1) = playerId Then Return True
        Return False
    End Function

    Private Function EnemyPresentAt(x As Integer, y As Integer, friendlyId As Integer) As Boolean
        If Players Is Nothing Then Return False
        For Each p In Players
            If p Is Nothing OrElse p.PlayerNumber = friendlyId Then Continue For
            If p.Armies Is Nothing Then Continue For
            For Each a In p.Armies
                If a Is Nothing Then Continue For
                If a.X = x AndAlso a.Y = y AndAlso a.TotalSoldiers > 0 Then Return True
            Next
        Next
        Return False
    End Function

    Private Function KeyXY(x As Integer, y As Integer) As String
        Return $"{x},{y}"
    End Function

    Private Sub ParseXY(key As String, ByRef x As Integer, ByRef y As Integer)
        Dim parts() As String = key.Split(","c)
        If parts.Length = 2 Then
            Integer.TryParse(parts(0), x)
            Integer.TryParse(parts(1), y)
        End If
    End Sub

    Private Function SafeRaceName(playerId As Integer) As String
        If playerId < 0 OrElse playerId >= Players.Count Then Return "Neutral"
        Dim p As Player = Players(playerId)
        If p Is Nothing Then Return $"Player{playerId}"
        If String.IsNullOrWhiteSpace(p.Race) Then Return $"Player{playerId}"
        Return p.Race
    End Function


#Region "=== Accounts ==="

    ' === Customer model ===
    Private Class Customer
        Public Property Nickname As String       ' Unique identifier used across games
        Public Property Name As String           ' Full name
        Public Property Email As String
        Public Property Address As String
        Public Property Notes As String
    End Class

    ' === Customer data store (local helpers) ===
    Private Function LoadCustomers() As List(Of Customer)
        Dim folder As String = Path.Combine(Application.StartupPath, "Customers")
        Dim filePath As String = Path.Combine(folder, "customers.json")

        If Not Directory.Exists(folder) Then Directory.CreateDirectory(folder)
        If Not File.Exists(filePath) Then Return New List(Of Customer)

        Dim json As String = File.ReadAllText(filePath)
        If String.IsNullOrWhiteSpace(json) Then Return New List(Of Customer)
        Dim list = JsonConvert.DeserializeObject(Of List(Of Customer))(json)
        If list Is Nothing Then list = New List(Of Customer)
        Return list
    End Function

    Private Sub SaveCustomers(customers As IList(Of Customer))
        Dim folder As String = Path.Combine(Application.StartupPath, "Customers")
        Dim filePath As String = Path.Combine(folder, "customers.json")
        If Not Directory.Exists(folder) Then Directory.CreateDirectory(folder)
        Dim json As String = JsonConvert.SerializeObject(customers, Formatting.Indented)
        File.WriteAllText(filePath, json)
    End Sub

    ' === Customer grid data ===
    Private customers As BindingList(Of Customer)

    Private Sub LoadCustomerGrid()
        customers = New BindingList(Of Customer)(LoadCustomers())

        dgvCustomers.AutoGenerateColumns = False
        dgvCustomers.Columns.Clear()

        dgvCustomers.Columns.Add(MakeTextCol("Nickname", "Nickname", False, 120))
        dgvCustomers.Columns.Add(MakeTextCol("Name", "Full Name", False, 160))
        dgvCustomers.Columns.Add(MakeTextCol("Email", "Email", False, 180))
        dgvCustomers.Columns.Add(MakeTextCol("Address", "Address", False, 220))
        dgvCustomers.Columns.Add(MakeTextCol("Notes", "Notes", False, 220))

        dgvCustomers.DataSource = customers
    End Sub

    Private Function MakeTextCol(prop As String, header As String, [ReadOnly] As Boolean, width As Integer) As DataGridViewTextBoxColumn
        Dim col As New DataGridViewTextBoxColumn()
        col.DataPropertyName = prop
        col.HeaderText = header
        col.ReadOnly = [ReadOnly]
        col.Width = width
        Return col
    End Function

    Private Sub btnSaveCustomers_Click(sender As Object, e As EventArgs) Handles btnSaveCustomers.Click
        dgvCustomers.EndEdit()

        ' Validation: every customer must have a unique nickname
        Dim seen As New HashSet(Of String)(StringComparer.OrdinalIgnoreCase)
        For Each c In customers
            If String.IsNullOrWhiteSpace(c.Nickname) Then
                MessageBox.Show("Every customer must have a nickname.", "Save", MessageBoxButtons.OK, MessageBoxIcon.Warning)
                Return
            End If
            If seen.Contains(c.Nickname) Then
                MessageBox.Show($"Duplicate nickname found: {c.Nickname}", "Save", MessageBoxButtons.OK, MessageBoxIcon.Warning)
                Return
            End If
            seen.Add(c.Nickname)
        Next

        SaveCustomers(customers.ToList())
        PopulateCustomerDropdowns()
        MessageBox.Show("Customers saved successfully!", "Save", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

    Private Sub PopulateCustomerDropdowns()
        Dim list = LoadCustomers()
        Dim nicknames = list.Select(Function(c) c.Nickname).
                          Where(Function(n) Not String.IsNullOrWhiteSpace(n)).
                          OrderBy(Function(n) n).
                          ToList()

        ' Insert AI option at the top
        nicknames.Insert(0, "AI")

        ' Bind to each combo (each gets its own copy)
        cmbElf.DataSource = New List(Of String)(nicknames)
        cmbDwarf.DataSource = New List(Of String)(nicknames)
        cmbOrc.DataSource = New List(Of String)(nicknames)
        cmbHuman.DataSource = New List(Of String)(nicknames)

        ' Default all to AI control
        cmbElf.SelectedIndex = 0
        cmbDwarf.SelectedIndex = 0
        cmbOrc.SelectedIndex = 0
        cmbHuman.SelectedIndex = 0
    End Sub


    Private Sub btnApplyAssignments_Click(sender As Object, e As EventArgs) Handles btnApplyAssignments.Click
        ' Safety: ensure Players() exists
        If Players Is Nothing OrElse Players.Count = 0 Then
            MessageBox.Show("No active game or player list found.", "Assign", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            Exit Sub
        End If

        ' For each race, assign the chosen nickname (if any)
        For Each p In Players
            Select Case p.Race.ToLower()
                Case "elf"
                    p.Nickname = If(cmbElf.SelectedItem?.ToString(), "")
                    p.AIControlled = String.IsNullOrWhiteSpace(p.Nickname)
                Case "dwarf"
                    p.Nickname = If(cmbDwarf.SelectedItem?.ToString(), "")
                    p.AIControlled = String.IsNullOrWhiteSpace(p.Nickname)
                Case "orc"
                    p.Nickname = If(cmbOrc.SelectedItem?.ToString(), "")
                    p.AIControlled = String.IsNullOrWhiteSpace(p.Nickname)
                Case "human"
                    p.Nickname = If(cmbHuman.SelectedItem?.ToString(), "")
                    p.AIControlled = String.IsNullOrWhiteSpace(p.Nickname)
            End Select
        Next

        MessageBox.Show("Assignments applied successfully!", "Assign", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

#End Region

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CurrentForm = Me
        CreateTerrainCache()
        SetupPrinting()
        PopulateGameList()
        InitialiseSummonerNames(SummonerDefinitions)
        InitialiseMarketCombos()
        LoadMonsterDescriptions()
        LoadCustomerGrid()
        PopulateCustomerDropdowns()
        InitialiseMoveColumns()
        dgvOrders.EditMode = DataGridViewEditMode.EditOnEnter
        dgvOrders.AllowUserToAddRows = False
    End Sub

#Region "=== UI Events ===="
    Private Sub cmbElf_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbElf.SelectedIndexChanged
        If isRestoringCombos Then Exit Sub
        UpdatePlayerAssignment("elf", cmbElf)
    End Sub

    Private Sub cmbDwarf_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbDwarf.SelectedIndexChanged
        If isRestoringCombos Then Exit Sub
        UpdatePlayerAssignment("dwarf", cmbDwarf)
    End Sub

    Private Sub cmbOrc_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbOrc.SelectedIndexChanged
        If isRestoringCombos Then Exit Sub
        UpdatePlayerAssignment("orc", cmbOrc)
    End Sub

    Private Sub cmbHuman_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cmbHuman.SelectedIndexChanged
        If isRestoringCombos Then Exit Sub
        UpdatePlayerAssignment("human", cmbHuman)
    End Sub

    ' One handler to do both jobs
    Private Sub dgvOrders_CellEnter(sender As Object, e As DataGridViewCellEventArgs) _
    Handles dgvOrders.CellEnter

        If e.RowIndex < 0 OrElse e.ColumnIndex < 0 Then Exit Sub

        Dim col As DataGridViewColumn = dgvOrders.Columns(e.ColumnIndex)

        ' 1) If we're entering the Recruit combo, (re)bind it for this row's race
        If col.Name = "colRecruitUnit" Then
            Dim row As DataGridViewRow = dgvOrders.Rows(e.RowIndex)
            Dim race As String = If(row.Cells("colPlayer").Value, "").ToString()

            Dim cell As DataGridViewComboBoxCell = TryCast(row.Cells("colRecruitUnit"), DataGridViewComboBoxCell)
            If cell IsNot Nothing Then
                ' preserve current value if possible
                Dim prevValue As Object = cell.Value

                cell.DisplayMember = "Display"
                cell.ValueMember = "ShortName"
                cell.DataSource = BuildUnitOptionsForRace(race)

                ' restore previous value if it still exists in the list
                If prevValue IsNot Nothing Then
                    Dim prev As String = prevValue.ToString()
                    Dim found As Boolean = False
                    For Each opt As UnitOption In CType(cell.DataSource, List(Of UnitOption))
                        If String.Equals(opt.ShortName, prev, StringComparison.OrdinalIgnoreCase) Then
                            found = True : Exit For
                        End If
                    Next
                    If found Then cell.Value = prev
                End If
            End If
        End If

        ' 2) Auto-drop any ComboBox column the user enters
        If TypeOf col Is DataGridViewComboBoxColumn Then
            dgvOrders.BeginEdit(True)
            Dim combo As ComboBox = TryCast(dgvOrders.EditingControl, ComboBox)
            If combo IsNot Nothing Then combo.DroppedDown = True
        End If
    End Sub


    ' Quietly ignore transient binding errors on combo cells
    Private Sub dgvOrders_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles dgvOrders.DataError
        e.ThrowException = False
    End Sub

    ' Attach/detach a KeyPress filter when editing the Amount cell
    Private Sub dgvOrders_EditingControlShowing(sender As Object, e As DataGridViewEditingControlShowingEventArgs) _
    Handles dgvOrders.EditingControlShowing

        If dgvOrders.CurrentCell Is Nothing Then Exit Sub
        If dgvOrders.CurrentCell.OwningColumn.Name <> "colRecruitAmount" Then Exit Sub

        Dim tb = TryCast(e.Control, TextBox)
        If tb Is Nothing Then Exit Sub

        RemoveHandler tb.KeyPress, AddressOf Amount_KeyPress   ' avoid multi-subscribe
        AddHandler tb.KeyPress, AddressOf Amount_KeyPress
    End Sub

    Private Sub Amount_KeyPress(sender As Object, e As KeyPressEventArgs)
        Dim ch As Char = e.KeyChar
        If Char.IsControl(ch) Then Return                  ' backspace, delete, arrows, etc.
        If Char.IsDigit(ch) Then Return                    ' 0-9 allowed
        If "mMaAxX".IndexOf(ch) >= 0 Then Return           ' allow MAX letters
        e.Handled = True                                   ' block everything else
    End Sub

    ' === Single validator for the Amount column ===
    Private Sub dgvOrders_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) _
    Handles dgvOrders.CellValidating

        If e.RowIndex < 0 OrElse e.ColumnIndex < 0 Then Exit Sub
        If dgvOrders.Columns(e.ColumnIndex).Name <> "colRecruitAmount" Then Exit Sub

        Dim txt As String = (If(e.FormattedValue, "")).ToString().Trim()

        ' Allow blank (we'll treat as MAX later if a unit is chosen)
        If txt = "" Then
            dgvOrders.Rows(e.RowIndex).ErrorText = ""
            Exit Sub
        End If

        ' Allow MAX (any case)
        If String.Equals(txt, "MAX", StringComparison.OrdinalIgnoreCase) Then
            dgvOrders.Rows(e.RowIndex).ErrorText = ""
            Exit Sub
        End If

        ' Otherwise must be a positive integer
        Dim n As Integer
        If Not Integer.TryParse(txt, n) OrElse n <= 0 Then
            dgvOrders.Rows(e.RowIndex).ErrorText = "Enter a positive number or 'MAX'."
            e.Cancel = True
        Else
            dgvOrders.Rows(e.RowIndex).ErrorText = ""
        End If
    End Sub

    ' === Single handler for post-edit actions ===
    Private Sub dgvOrders_CellEndEdit(sender As Object, e As DataGridViewCellEventArgs) _
    Handles dgvOrders.CellEndEdit

        If e.RowIndex < 0 OrElse e.ColumnIndex < 0 Then Exit Sub

        Dim grid As DataGridView = DirectCast(sender, DataGridView)
        Dim colName As String = grid.Columns(e.ColumnIndex).Name

        Select Case colName

        ' --- Normalise amount to uppercase MAX ---
            Case "colRecruitAmount"
                Dim cell = grid.Rows(e.RowIndex).Cells(e.ColumnIndex)
                Dim txt As String = If(cell.Value, "").ToString().Trim()
                If String.Equals(txt, "max", StringComparison.OrdinalIgnoreCase) Then
                    cell.Value = "MAX"
                End If

        ' --- Instant rename when Army cell edited ---
            Case "colArmy"
                If Players Is Nothing OrElse Players.Count = 0 Then Exit Sub

                Dim playerName As String = grid.Rows(e.RowIndex).Cells("colPlayer").Value?.ToString()
                Dim newArmyName As String = grid.Rows(e.RowIndex).Cells("colArmy").Value?.ToString()
                If String.IsNullOrWhiteSpace(playerName) Then Exit Sub

                ' Find player
                Dim p As Player = Players.FirstOrDefault(Function(pl) pl.Race.Equals(playerName, StringComparison.OrdinalIgnoreCase))
                If p Is Nothing OrElse p.Armies Is Nothing OrElse p.Armies.Count = 0 Then Exit Sub

                ' Determine which army this row represents (your current approach)
                Dim playerIndex As Integer = Array.IndexOf(Races, p.Race)
                If playerIndex < 0 Then Exit Sub

                Dim armyIndex As Integer = e.RowIndex - (playerIndex * 3)
                If armyIndex < 0 OrElse armyIndex >= p.Armies.Count Then Exit Sub

                Dim a As Army = p.Armies(armyIndex)
                If a Is Nothing Then Exit Sub

                ' Apply rename immediately (safe Proper Case)
                If Not String.IsNullOrWhiteSpace(newArmyName) Then
                    Dim formattedName As String = newArmyName.Trim()
                    If formattedName.Length > 0 Then
                        formattedName = Char.ToUpperInvariant(formattedName(0)) &
                                   If(formattedName.Length > 1, formattedName.Substring(1).ToLowerInvariant(), "")
                    End If

                    a.Name = formattedName
                    'System.Diagnostics.Debug.WriteLine($"[INSTANT RENAME] {p.Race} army renamed to '{a.Name}'")
                    ' grid.Rows(e.RowIndex).Cells("colArmy").Style.BackColor = Color.LightGreen
                End If

            Case Else
                ' no-op for other columns

        End Select
    End Sub

#End Region

End Class