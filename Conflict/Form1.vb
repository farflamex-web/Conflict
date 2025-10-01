Option Strict On
Option Explicit On

' Test commit from Visual Studio, again, and again and again, and again, and again and again.

Imports System.Drawing.Printing
Imports System.IO
Imports System.Linq
Imports System.Reflection
Imports Conflict.Form1

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
        Public Property Armour As String
        Public Property Shield As String
        Public Property Type As UnitType
        Public Property FoodCost As Double
        Public Property CanCharge As Boolean
        Public Property CanChase As Boolean
        Public Property Flying As Boolean

        Public Property Race As String
        Public Property Size As Integer

        ' --- Hero / Summoner flags ---
        Public Property IsHero As Boolean = False
        Public Property IsSummoner As Boolean = False
        Public Property SummonerFaction As String
        Public Property Level As Integer = 0


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
            Me.Armour = stats.Armour
            Me.Shield = stats.Shield
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
            Me.IsSummoner = False
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
            Me.Armour = u.Armour
            Me.Shield = u.Shield
            Me.Type = u.Type
            Me.FoodCost = u.FoodCost
            Me.CanCharge = u.CanCharge
            Me.CanChase = u.CanChase
            Me.Flying = u.Flying

            Me.Race = u.Race
            Me.Size = u.Size
            Me.IsHero = u.IsHero
            Me.IsSummoner = u.IsSummoner
            Me.SummonerFaction = u.SummonerFaction
            Me.Level = u.Level
        End Sub

        ' Summoner hero constructor
        Public Sub New(faction As String, level As Integer, ownerRace As String)
            Me.Name = $"{faction} Summoner"
            Me.HP = 10
            Me.Melee = 0
            Me.Ranged = 0
            Me.Power = 0
            Me.DefencePoints = 0
            Me.Size = 1
            Me.Armour = "None"
            Me.Type = UnitType.LightInfantry
            Me.Race = ownerRace
            Me.FoodCost = 1

            Me.IsHero = True
            Me.IsSummoner = True
            Me.SummonerFaction = faction
            Me.Level = If(level > 0, level, 1)
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
        Public Property Armour As String
        Public Property Shield As String
        Public Property Cost As String
        Public Property ShortName As String       ' <<< THIS IS REQUIRED
        Public Property FoodCost As Double = 1.0   ' default; will override for summons
        Public Property CanCharge As Boolean
        Public Property CanChase As Boolean
        Public Property Flying As Boolean
        Public Property IsSummoner As Boolean
        Public Property IsHero As Boolean
        Public Property SummonerFaction As String
    End Class

    Public Class ArmyCommand
        Public Property Command As String       ' e.g., "MOVE", "RECRUIT", "DISBAND"
        Public Property Parameter As String     ' e.g., "Ironfoot", "li", "HeavyInfantry"
    End Class

    Private ReadOnly Property SummonerRosters As Dictionary(Of String, List(Of UnitStats))
        Get
            Dim dict As New Dictionary(Of String, List(Of UnitStats))(StringComparer.OrdinalIgnoreCase)

            Dim forest = AllRaces.FirstOrDefault(Function(r) r.RaceName = "Druidic Spirits")?.Units
            Dim undead = AllRaces.FirstOrDefault(Function(r) r.RaceName = "Undead")?.Units
            Dim golems = AllRaces.FirstOrDefault(Function(r) r.RaceName = "Golems")?.Units
            Dim elems = AllRaces.FirstOrDefault(Function(r) r.RaceName = "Elementals")?.Units
            Dim drags = AllRaces.FirstOrDefault(Function(r) r.RaceName = "Dragons")?.Units
            Dim constructs = AllRaces.FirstOrDefault(Function(r) r.RaceName = "Constructs")?.Units
            Dim beasts = AllRaces.FirstOrDefault(Function(r) r.RaceName = "Beasts")?.Units

            If forest IsNot Nothing Then dict("Druid") = forest
            If undead IsNot Nothing Then dict("Necromancer") = undead
            If golems IsNot Nothing Then dict("Golemancer") = golems
            If elems IsNot Nothing Then dict("Elementalist") = elems
            If constructs IsNot Nothing Then dict("Construct Master") = constructs
            If beasts IsNot Nothing Then dict("Beastlord") = beasts

            Return dict
        End Get
    End Property



    Private Sub ProcessSummoners()
        For Each p In Players
            If p.Armies Is Nothing Then Continue For
            For Each a In p.Armies
                Dim unitsSnapshot = a.Units.ToList()
                For Each u In unitsSnapshot
                    If u IsNot Nothing AndAlso u.IsSummoner AndAlso Not String.IsNullOrWhiteSpace(u.SummonerFaction) Then
                        SummonCreaturesForUnit(u, a)
                        u.Level += 1
                    End If
                Next
            Next
        Next
    End Sub


    Private Sub SummonCreaturesForUnit(summoner As Unit, ownerArmy As Army)
        ' Skip if invalid
        If String.IsNullOrEmpty(summoner.SummonerFaction) Then Exit Sub
        If Not SummonerRosters.ContainsKey(summoner.SummonerFaction) Then Exit Sub

        Dim roster = SummonerRosters(summoner.SummonerFaction)
        If roster Is Nothing OrElse roster.Count = 0 Then Exit Sub

        ' === Budget: 2 points per level ===
        Dim level As Integer = If(summoner.Level > 0, summoner.Level, 1)
        Dim budget As Integer = level * 3

        ' Randomiser seeded so results vary but are consistent per turn/summoner
        Dim seed As Integer = (Math.Max(1, currentTurnNumber) * 97) Xor
                          (ownerArmy.X * 73856093) Xor
                          (ownerArmy.Y * 19349663) Xor
                          (If(summoner.Name, "").GetHashCode())
        Dim rnd As New Random(seed)

        ' === Spend budget ===
        Dim remaining As Integer = budget
        Dim guard As Integer = 10000

        Do While remaining > 0 AndAlso guard > 0
            guard -= 1

            ' Only pick from affordable units
            Dim affordable = roster.Where(Function(u) u.Power > 0 AndAlso u.Power <= remaining).ToList()
            If affordable.Count = 0 Then Exit Do

            Dim pick As UnitStats = affordable(rnd.Next(affordable.Count))

            ' Always summon exactly 1 unit per loop
            Dim count As Integer = 1

            ' Merge into existing army if unit type already present
            Dim existing = ownerArmy.Units.FirstOrDefault(Function(u) u.Name = pick.Name)
            If existing IsNot Nothing Then
                existing.Size += count
            Else
                ownerArmy.Units.Add(New Unit(pick, ownerArmy.Race, count))
            End If

            remaining -= pick.Power
        Loop

        If guard = 0 Then
            Debug.WriteLine("Guard triggered in SummonCreaturesForUnit — possible bad roster config.")
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
        Public Property AIControlled As Boolean
        Public Property CatchUpCooldown As Integer  ' turns until next catch-up recruit allowed
        Public Property Gems As Integer
        Public Property Amber As Integer
        Public Property Wine As Integer
        Public Property Furs As Integer

    End Class

    Public Class Market
        Public Property GemPrice As Double = 50
        Public Property AmberPrice As Double = 25
        Public Property WinePrice As Double = 15
        Public Property FurPrice As Double = 10
        Public Property IronPrice As Double = 5
        Public Property WoodPrice As Double = 5
    End Class


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

    Private currentTurnNumber As Integer = 1  ' Update this as part of your turn logic

    Public AllRaces As New List(Of RaceUnits)

    Private Shared rnd As New Random()


#End Region

#Region "=== Form Lifecycle ==="

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        CreateTerrainCache()

        InitializeRaceUnits()

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


    Public Sub InitializeRaceUnits()
        AllRaces = New List(Of RaceUnits)

        ' ----------------- Dwarves -----------------
        Dim dwarfUnits As New RaceUnits With {
        .RaceName = "Dwarf",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Ironbolters", .Type = UnitType.Archer, .HP = 5, .Melee = 1, .Ranged = 1, .Armour = "Chainmail", .Shield = Nothing, .Cost = "I:2, W:1", .ShortName = "a"},
            New UnitStats With {.Name = "Ironshield", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Iron", .Cost = "I:2", .ShortName = "li"},
            New UnitStats With {.Name = "Stormforged Hammerguards", .Type = UnitType.HeavyInfantry, .HP = 15, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = Nothing, .Cost = "I:3", .ShortName = "hi", .CanCharge = True},
            New UnitStats With {.Name = "Ironhorn Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Iron", .Cost = "I:3, M:1", .ShortName = "lc", .CanChase = True},
            New UnitStats With {.Name = "Ironhorn Maulers", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = Nothing, .Cost = "I:3, M:1", .ShortName = "hc", .CanCharge = True}
        }
    }
        AllRaces.Add(dwarfUnits)

        ' ----------------- Orcs -----------------
        Dim orcUnits As New RaceUnits With {
        .RaceName = "Orc",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Orc Shortbows", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 1, .Armour = "None", .Shield = Nothing, .Cost = "I:1, W:1", .ShortName = "a"},
            New UnitStats With {.Name = "War Grunts", .Type = UnitType.LightInfantry, .HP = 6, .Melee = 2, .Ranged = 0, .Armour = "None", .Shield = Nothing, .Cost = "I:1", .ShortName = "li"},
            New UnitStats With {.Name = "Orc Berserkers", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Chainmail", .Shield = Nothing, .Cost = "I:2", .ShortName = "hi", .CanCharge = True},
            New UnitStats With {.Name = "Fangriders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 2, .Ranged = 0, .Armour = "None", .Shield = "Wooden", .Cost = "I:1, W:1, M:1", .ShortName = "lc", .CanChase = True},
            New UnitStats With {.Name = "Bloodwolf Riders", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Chainmail", .Shield = Nothing, .Cost = "I:2, M:1", .ShortName = "hc", .CanCharge = True}
        }
    }
        AllRaces.Add(orcUnits)

        ' ----------------- Humans -----------------
        Dim humanUnits As New RaceUnits With {
        .RaceName = "Human",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Silver Bowmen", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 1, .Armour = "None", .Shield = Nothing, .Cost = "I:1, W:1", .ShortName = "a"},
            New UnitStats With {.Name = "Guardian Spearmen", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Wooden", .Cost = "I:2, W:1", .ShortName = "li"},
            New UnitStats With {.Name = "Ironstorm Infantry", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = Nothing, .Cost = "I:4", .ShortName = "hi", .CanCharge = True},
            New UnitStats With {.Name = "Swiftblade Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Wooden", .Cost = "I:2, W:1, M:1", .ShortName = "lc", .CanChase = True},
            New UnitStats With {.Name = "Steel Vanguard", .Type = UnitType.HeavyCavalry, .HP = 15, .Melee = 3, .Ranged = 0, .Armour = "Plate", .Shield = Nothing, .Cost = "I:5, M:1", .ShortName = "hc", .CanCharge = True}
        }
    }
        AllRaces.Add(humanUnits)

        ' ----------------- Elves -----------------
        Dim elfUnits As New RaceUnits With {
        .RaceName = "Elf",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Moonbow Archers", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 2, .Armour = "None", .Shield = Nothing, .Cost = "I:1, W:1", .ShortName = "a"},
            New UnitStats With {.Name = "Forest Sentinels", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Wooden", .Cost = "I:2, W:1", .ShortName = "li"},
            New UnitStats With {.Name = "Moonblade Guardians", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = "Iron", .Cost = "I:4", .ShortName = "hi", .CanCharge = True},
            New UnitStats With {.Name = "Moonbow Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 1, .Ranged = 2, .Armour = "None", .Shield = Nothing, .Cost = "I:1, W:1, M:1", .ShortName = "lc", .CanChase = True},
            New UnitStats With {.Name = "Stormleaf Chargers", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = "Iron", .Cost = "I:4, M:1", .ShortName = "hc", .CanCharge = True}
        }
    }
        AllRaces.Add(elfUnits)

        ' === Mercenary Factions ===

        ' Kobolds
        Dim kobolds As New RaceUnits With {
    .RaceName = "Kobolds",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Kobold Infantry", .ShortName = "KobInf", .Power = 1, .HP = 2, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Armour = "None", .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Kobold Bowmen", .ShortName = "KobBow", .Power = 1, .HP = 2, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Armour = "None", .Type = UnitType.Archer},
        New UnitStats With {.Name = "Kobold Raider", .ShortName = "KobRaid", .Power = 2, .HP = 3, .Melee = 2, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightCavalry, .CanChase = True, .Armour = "None", .Shield = "Wooden"}
    }
}
        AllRaces.Add(kobolds)


        ' === Lizardmen ===
        Dim lizardmen As New RaceUnits With {
    .RaceName = "Lizardmen",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Lizardman Warrior", .ShortName = "LizWar", .Power = 3, .HP = 15, .Melee = 4, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Lizardman Skirmisher", .ShortName = "LizSki", .Power = 3, .HP = 10, .Melee = 1, .Ranged = 1, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Lizardman Priest", .ShortName = "LizPri", .Power = 10, .HP = 10, .Melee = 1, .Ranged = 2, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Lizardman Guard", .ShortName = "LizGua", .Power = 20, .HP = 30, .Melee = 3, .Ranged = 0, .DefencePoints = 3, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Raptor Rider", .ShortName = "LizRap", .Power = 20, .HP = 10, .Melee = 1, .Ranged = 0, .DefencePoints = 3, .FoodCost = 1, .Type = UnitType.LightCavalry, .CanCharge = True, .CanChase = True}
    }
}
        AllRaces.Add(lizardmen)


        ' === Skulkrin ===
        Dim skulkrin As New RaceUnits With {
    .RaceName = "Skulkrin",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Skulkrin Scrapper", .ShortName = "SkuScr", .Power = 1, .HP = 5, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Skulkrin Slinger", .ShortName = "SkuSli", .Power = 2, .HP = 5, .Melee = 0, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer}
    }
}
        AllRaces.Add(skulkrin)


        ' === Faction: Gnomes ===
        Dim gnomes As New RaceUnits With {
    .RaceName = "Gnomes",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Gnome Infantry", .ShortName = "GnoInf", .Power = 2, .HP = 8, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Gnome Crossbowmen", .ShortName = "GnoXbw", .Power = 3, .HP = 7, .Melee = 1, .Ranged = 2, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Gnome Heavyguard", .ShortName = "GnoHvy", .Power = 5, .HP = 12, .Melee = 3, .Ranged = 0, .DefencePoints = 4, .FoodCost = 1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Gnome Gyrocopter", .ShortName = "GnoGyro", .Power = 8, .HP = 15, .Melee = 2, .Ranged = 4, .DefencePoints = 2, .FoodCost = 1, .Flying = True, .CanChase = True, .Type = UnitType.LightCavalry}
    }
}
        AllRaces.Add(gnomes)


        ' Peasants
        Dim peasants As New RaceUnits With {
    .RaceName = "Peasants",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Peasant Militia", .ShortName = "PeaMil", .Power = 1, .HP = 2, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Peasant Slingers", .ShortName = "PeaSli", .Power = 1, .HP = 2, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer}
    }
}
        AllRaces.Add(peasants)


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

        ' Ogres
        Dim ogres As New RaceUnits With {
    .RaceName = "Ogres",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Ogre Brute", .ShortName = "OgreBru", .Power = 8, .HP = 25, .Melee = 10, .Ranged = 0, .DefencePoints = 2, .FoodCost = 2, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Ogre Crusher", .ShortName = "OgreCru", .Power = 10, .HP = 35, .Melee = 15, .Ranged = 0, .DefencePoints = 2, .FoodCost = 2, .Type = UnitType.HeavyInfantry}
    }
}
        AllRaces.Add(ogres)

        ' Gnolls
        Dim gnolls As New RaceUnits With {
    .RaceName = "Gnolls",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Gnoll Spearthrower", .ShortName = "GnoSp", .Power = 2, .HP = 6, .Melee = 1, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Gnoll Raider", .ShortName = "GnoRa", .Power = 3, .HP = 10, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightCavalry, .CanChase = True},
        New UnitStats With {.Name = "Gnoll Warleader", .ShortName = "GnoWa", .Power = 4, .HP = 12, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .CanCharge = True}
    }
}
        AllRaces.Add(gnolls)

        ' Goblins
        Dim goblins As New RaceUnits With {
    .RaceName = "Goblins",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Goblin Skirmishers", .ShortName = "GobSki", .Power = 1, .HP = 2, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Goblin Slingers", .ShortName = "GobSli", .Power = 2, .HP = 2, .Melee = 0, .Ranged = 1, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Goblin Spider Riders", .ShortName = "GobSpi", .Power = 3, .HP = 10, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightCavalry, .CanChase = True},
        New UnitStats With {.Name = "Hobgoblin Brutes", .ShortName = "HobBru", .Power = 6, .HP = 20, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .Type = UnitType.HeavyInfantry}
    }
}
        AllRaces.Add(goblins)

        ' === Werecreatures ===
        Dim werecreatures As New RaceUnits With {
    .RaceName = "Werecreatures",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Wererat", .ShortName = "WerRat", .Power = 10, .HP = 20, .Melee = 6, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Werewolf", .ShortName = "WerWlf", .Power = 25, .HP = 40, .Melee = 12, .Ranged = 0, .DefencePoints = 2, .FoodCost = 2, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Weretiger", .ShortName = "WerTig", .Power = 40, .HP = 60, .Melee = 18, .Ranged = 0, .DefencePoints = 3, .FoodCost = 3, .Type = UnitType.HeavyInfantry, .CanCharge = True, .CanChase = True},
        New UnitStats With {.Name = "Werebear", .ShortName = "WerBer", .Power = 60, .HP = 80, .Melee = 22, .Ranged = 0, .DefencePoints = 4, .FoodCost = 4, .Type = UnitType.HeavyInfantry, .CanCharge = True}
    }
}
        AllRaces.Add(werecreatures)

        ' === Trolls ===
        Dim trolls As New RaceUnits With {
    .RaceName = "Trolls",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Cave Troll", .ShortName = "CavTro", .Power = 20, .HP = 50, .Melee = 12, .Ranged = 0, .DefencePoints = 2, .FoodCost = 2, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Forest Troll", .ShortName = "ForTro", .Power = 35, .HP = 80, .Melee = 18, .Ranged = 0, .DefencePoints = 3, .FoodCost = 3, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Ice Troll", .ShortName = "IceTro", .Power = 50, .HP = 120, .Melee = 24, .Ranged = 10, .DefencePoints = 4, .FoodCost = 4, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "War Troll", .ShortName = "WarTro", .Power = 70, .HP = 160, .Melee = 32, .Ranged = 0, .DefencePoints = 5, .FoodCost = 5, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Dire Troll", .ShortName = "DirTro", .Power = 100, .HP = 220, .Melee = 42, .Ranged = 0, .DefencePoints = 6, .FoodCost = 6, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Elder Troll", .ShortName = "EldTro", .Power = 140, .HP = 300, .Melee = 55, .Ranged = 15, .DefencePoints = 7, .FoodCost = 8, .Type = UnitType.HeavyInfantry, .CanCharge = True}
    }
}
        AllRaces.Add(trolls)


        ' === Giants ===
        Dim giants As New RaceUnits With {
    .RaceName = "Giants",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Hill Giant", .ShortName = "HilGia", .Power = 80, .HP = 200, .Melee = 60, .Ranged = 0, .DefencePoints = 4, .FoodCost = 6, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Stone Giant", .ShortName = "StoGia", .Power = 120, .HP = 250, .Melee = 70, .Ranged = 20, .DefencePoints = 5, .FoodCost = 7, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Frost Giant", .ShortName = "FroGia", .Power = 180, .HP = 300, .Melee = 90, .Ranged = 30, .DefencePoints = 6, .FoodCost = 8, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Fire Giant", .ShortName = "FirGia", .Power = 220, .HP = 320, .Melee = 100, .Ranged = 40, .DefencePoints = 7, .FoodCost = 9, .Type = UnitType.HeavyInfantry, .CanCharge = True},
        New UnitStats With {.Name = "Cloud Giant", .ShortName = "CloGia", .Power = 300, .HP = 400, .Melee = 120, .Ranged = 60, .DefencePoints = 8, .FoodCost = 10, .Type = UnitType.HeavyInfantry, .CanCharge = True, .CanChase = True},
        New UnitStats With {.Name = "Storm Giant", .ShortName = "StoGnt", .Power = 400, .HP = 500, .Melee = 150, .Ranged = 80, .DefencePoints = 10, .FoodCost = 12, .Type = UnitType.HeavyInfantry, .CanCharge = True, .CanChase = True}
    }
}
        AllRaces.Add(giants)


        Dim dragons As New RaceUnits With {
    .RaceName = "Dragons",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "White Dragon", .ShortName = "WhiDra", .Power = 1000, .HP = 250, .Melee = 200, .Ranged = 120, .DefencePoints = 8, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Brass Dragon", .ShortName = "BraDra", .Power = 1100, .HP = 280, .Melee = 220, .Ranged = 180, .DefencePoints = 9, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Black Dragon", .ShortName = "BlaDra", .Power = 1200, .HP = 300, .Melee = 320, .Ranged = 150, .DefencePoints = 10, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Copper Dragon", .ShortName = "CopDra", .Power = 1300, .HP = 320, .Melee = 280, .Ranged = 220, .DefencePoints = 11, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Green Dragon", .ShortName = "GreDra", .Power = 1400, .HP = 350, .Melee = 350, .Ranged = 250, .DefencePoints = 12, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Blue Dragon", .ShortName = "BluDra", .Power = 1500, .HP = 400, .Melee = 500, .Ranged = 300, .DefencePoints = 12, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Red Dragon", .ShortName = "RedDra", .Power = 1600, .HP = 450, .Melee = 800, .Ranged = 500, .DefencePoints = 14, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Bronze Dragon", .ShortName = "BroDra", .Power = 1700, .HP = 480, .Melee = 850, .Ranged = 550, .DefencePoints = 14, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Silver Dragon", .ShortName = "SilDra", .Power = 1800, .HP = 520, .Melee = 900, .Ranged = 600, .DefencePoints = 15, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Gold Dragon", .ShortName = "GolDra", .Power = 1900, .HP = 600, .Melee = 1000, .Ranged = 700, .DefencePoints = 15, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Flying = True},
        New UnitStats With {.Name = "Ancient Dragon", .ShortName = "AncDra", .Power = 6000, .HP = 2500, .Melee = 5000, .Ranged = 5000, .DefencePoints = 16, .FoodCost = 3, .CanChase = True, .CanCharge = True, .Flying = True}
    }
}
        AllRaces.Add(dragons)


        ' === Summoner Faction: Druid (Elf-themed summons) ===
        Dim druids As New RaceUnits With {
    .RaceName = "Druidic Spirits",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Faun", .ShortName = "Faun", .Power = 1, .HP = 4, .Melee = 1, .Ranged = 0, .DefencePoints = 1, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Satyr", .ShortName = "Satyr", .Power = 3, .HP = 15, .Melee = 3, .Ranged = 1, .DefencePoints = 2, .FoodCost = 1, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Centaur", .ShortName = "Centaur", .Power = 4, .HP = 20, .Melee = 2, .Ranged = 2, .DefencePoints = 2, .FoodCost = 1, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Unicorn", .ShortName = "Unicrn", .Power = 25, .HP = 125, .Melee = 15, .Ranged = 5, .DefencePoints = 10, .FoodCost = 0, .CanChase = True, .Type = UnitType.HeavyCavalry},
        New UnitStats With {.Name = "Ent", .ShortName = "Ent", .Power = 49, .HP = 250, .Melee = 25, .Ranged = 0, .DefencePoints = 15, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Ancient Ent", .ShortName = "AncEnt", .Power = 64, .HP = 350, .Melee = 40, .Ranged = 0, .DefencePoints = 20, .FoodCost = 0, .Type = UnitType.HeavyInfantry}
    }
}
        AllRaces.Add(druids)


        ' === Summoner Faction: Necromancer (rescued from NecromancerSummonerRoster) ===
        Dim necromancers As New RaceUnits With {
    .RaceName = "Undead",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Zombie", .ShortName = "Zombie", .Power = 1, .HP = 5, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 0, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Skeleton", .ShortName = "Skeleton", .Power = 4, .HP = 20, .Melee = 2, .Ranged = 0, .DefencePoints = 2, .FoodCost = 0, .CanChase = True, .CanCharge = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Shadow", .ShortName = "Shadow", .Power = 6, .HP = 30, .Melee = 2, .Ranged = 0, .DefencePoints = 3, .FoodCost = 0, .Flying = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Ghost", .ShortName = "Ghost", .Power = 8, .HP = 40, .Melee = 3, .Ranged = 0, .DefencePoints = 4, .FoodCost = 0, .Flying = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Wight", .ShortName = "Wight", .Power = 9, .HP = 45, .Melee = 4, .Ranged = 0, .DefencePoints = 5, .FoodCost = 0, .CanChase = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Spectre", .ShortName = "Spectre", .Power = 12, .HP = 60, .Melee = 5, .Ranged = 0, .DefencePoints = 7, .FoodCost = 0, .Flying = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Wraith", .ShortName = "Wraith", .Power = 16, .HP = 80, .Melee = 7, .Ranged = 0, .DefencePoints = 10, .FoodCost = 0, .Flying = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Banshee", .ShortName = "Banshee", .Power = 20, .HP = 100, .Melee = 8, .Ranged = 2, .DefencePoints = 10, .FoodCost = 0, .Flying = True, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Ghoul", .ShortName = "Ghoul", .Power = 25, .HP = 125, .Melee = 10, .Ranged = 0, .DefencePoints = 15, .FoodCost = 0, .CanChase = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Bone Golem", .ShortName = "BnGolm", .Power = 36, .HP = 180, .Melee = 10, .Ranged = 0, .DefencePoints = 15, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Vampire", .ShortName = "Vampire", .Power = 49, .HP = 250, .Melee = 20, .Ranged = 0, .DefencePoints = 15, .FoodCost = 0, .Flying = True, .Type = UnitType.HeavyCavalry},
        New UnitStats With {.Name = "Lich", .ShortName = "Lich", .Power = 64, .HP = 300, .Melee = 20, .Ranged = 10, .DefencePoints = 15, .FoodCost = 0, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Death Knight", .ShortName = "DthKnt", .Power = 81, .HP = 400, .Melee = 100, .Ranged = 0, .DefencePoints = 15, .FoodCost = 0, .CanCharge = True, .Type = UnitType.HeavyCavalry},
        New UnitStats With {.Name = "Bone Dragon", .ShortName = "BnDrgn", .Power = 100, .HP = 500, .Melee = 1000, .Ranged = 500, .DefencePoints = 10, .FoodCost = 0, .CanChase = True, .CanCharge = True, .Flying = True, .Type = UnitType.HeavyCavalry}
    }
}
        AllRaces.Add(necromancers)


        ' === Summoner Faction: Beastlord ===
        Dim beasts As New RaceUnits With {
    .RaceName = "Beasts",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Snake", .ShortName = "Snake", .Power = 1, .HP = 1, .Melee = 1, .Ranged = 0, .DefencePoints = 1, .FoodCost = 0, .CanCharge = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Boar", .ShortName = "Boar", .Power = 2, .HP = 4, .Melee = 1, .Ranged = 0, .DefencePoints = 2, .FoodCost = 1, .CanCharge = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Wolf", .ShortName = "Wolf", .Power = 3, .HP = 4, .Melee = 2, .Ranged = 0, .DefencePoints = 3, .FoodCost = 1, .CanChase = True, .CanCharge = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Black Bear", .ShortName = "BlkBer", .Power = 6, .HP = 12, .Melee = 3, .Ranged = 0, .DefencePoints = 5, .FoodCost = 1, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "White Ape", .ShortName = "WhtApe", .Power = 8, .HP = 14, .Melee = 4, .Ranged = 0, .DefencePoints = 4, .FoodCost = 1, .CanCharge = True, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Lion", .ShortName = "Lion", .Power = 12, .HP = 18, .Melee = 6, .Ranged = 0, .DefencePoints = 4, .FoodCost = 1, .CanChase = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Tiger", .ShortName = "Tiger", .Power = 16, .HP = 22, .Melee = 8, .Ranged = 0, .DefencePoints = 5, .FoodCost = 1, .CanChase = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Giant Eagle", .ShortName = "GtEgl", .Power = 20, .HP = 30, .Melee = 5, .Ranged = 0, .DefencePoints = 3, .FoodCost = 1, .Flying = True, .CanChase = True, .Type = UnitType.LightCavalry},
        New UnitStats With {.Name = "Forest Gryphon", .ShortName = "FrsGrp", .Power = 28, .HP = 50, .Melee = 10, .Ranged = 0, .DefencePoints = 6, .FoodCost = 1, .Flying = True, .CanChase = True, .CanCharge = True, .Type = UnitType.HeavyCavalry},
        New UnitStats With {.Name = "War Elephant", .ShortName = "WrElph", .Power = 36, .HP = 120, .Melee = 12, .Ranged = 0, .DefencePoints = 12, .FoodCost = 2, .CanChase = True, .CanCharge = True, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Wyvern", .ShortName = "Wyvern", .Power = 49, .HP = 150, .Melee = 15, .Ranged = 5, .DefencePoints = 10, .FoodCost = 2, .Flying = True, .CanChase = True, .CanCharge = True, .Type = UnitType.HeavyCavalry}
    }
}
        AllRaces.Add(beasts)


        ' === Summoner Faction: Golemancer ===
        Dim golemancers As New RaceUnits With {
    .RaceName = "Golems",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Mud Golem", .ShortName = "MudGol", .Power = 1, .HP = 10, .Melee = 1, .Ranged = 0, .DefencePoints = 1, .FoodCost = 0, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Stone Golem", .ShortName = "StnGol", .Power = 6, .HP = 30, .Melee = 2, .Ranged = 0, .DefencePoints = 4, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Iron Golem", .ShortName = "IrnGol", .Power = 12, .HP = 60, .Melee = 3, .Ranged = 0, .DefencePoints = 8, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Flesh Golem", .ShortName = "FlhGol", .Power = 14, .HP = 70, .Melee = 4, .Ranged = 0, .DefencePoints = 5, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Adamant Golem", .ShortName = "AdmGol", .Power = 20, .HP = 100, .Melee = 4, .Ranged = 0, .DefencePoints = 12, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Crystal Golem", .ShortName = "CryGol", .Power = 28, .HP = 140, .Melee = 4, .Ranged = 2, .DefencePoints = 14, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Obsidian Golem", .ShortName = "ObsGol", .Power = 36, .HP = 180, .Melee = 5, .Ranged = 0, .DefencePoints = 15, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Mithril Golem", .ShortName = "MthGol", .Power = 44, .HP = 220, .Melee = 6, .Ranged = 0, .DefencePoints = 16, .FoodCost = 0, .Type = UnitType.HeavyInfantry}
    }
}
        AllRaces.Add(golemancers)

        ' === Summoner Faction: Construct Master ===
        Dim constructs As New RaceUnits With {
    .RaceName = "Constructs",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Wooden Construct", .ShortName = "WodCon", .Power = 1, .HP = 6, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 0, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Clay Construct", .ShortName = "ClyCon", .Power = 2, .HP = 12, .Melee = 1, .Ranged = 0, .DefencePoints = 2, .FoodCost = 0, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Stone Construct", .ShortName = "StnCon", .Power = 6, .HP = 30, .Melee = 2, .Ranged = 0, .DefencePoints = 5, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Iron Construct", .ShortName = "IrnCon", .Power = 12, .HP = 60, .Melee = 3, .Ranged = 0, .DefencePoints = 8, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Magic Knight", .ShortName = "MagKnt", .Power = 16, .HP = 80, .Melee = 6, .Ranged = 2, .DefencePoints = 10, .FoodCost = 0, .CanCharge = True, .Type = UnitType.HeavyCavalry},
        New UnitStats With {.Name = "Runed Guardian", .ShortName = "RunGrd", .Power = 25, .HP = 120, .Melee = 8, .Ranged = 0, .DefencePoints = 12, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Arcane Sentinel", .ShortName = "ArcSen", .Power = 36, .HP = 180, .Melee = 10, .Ranged = 5, .DefencePoints = 14, .FoodCost = 0, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Mithril Construct", .ShortName = "MthCon", .Power = 49, .HP = 350, .Melee = 15, .Ranged = 0, .DefencePoints = 16, .FoodCost = 0, .Type = UnitType.HeavyInfantry}
    }
}
        AllRaces.Add(constructs)


        ' === Summoner Faction: Elementalists ===
        Dim elementalists As New RaceUnits With {
    .RaceName = "Elementals",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Wisp", .ShortName = "Wisp", .Power = 1, .HP = 2, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 0, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Flicker", .ShortName = "Flick", .Power = 3, .HP = 5, .Melee = 2, .Ranged = 0, .DefencePoints = 1, .FoodCost = 0, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Earth Elemental", .ShortName = "ErtEle", .Power = 8, .HP = 40, .Melee = 2, .Ranged = 0, .DefencePoints = 6, .FoodCost = 0, .Type = UnitType.HeavyInfantry},
        New UnitStats With {.Name = "Air Elemental", .ShortName = "AirEle", .Power = 10, .HP = 30, .Melee = 2, .Ranged = 2, .DefencePoints = 4, .FoodCost = 0, .Flying = True, .Type = UnitType.Archer},
        New UnitStats With {.Name = "Fire Elemental", .ShortName = "FirEle", .Power = 12, .HP = 35, .Melee = 2, .Ranged = 2, .DefencePoints = 3, .FoodCost = 0, .Type = UnitType.LightInfantry},
        New UnitStats With {.Name = "Water Elemental", .ShortName = "WatEle", .Power = 14, .HP = 50, .Melee = 2, .Ranged = 0, .DefencePoints = 5, .FoodCost = 0, .Type = UnitType.HeavyInfantry}
    }
}
        AllRaces.Add(elementalists)


        Dim heroes As New RaceUnits With {
    .RaceName = "Heroes",
    .Units = New List(Of UnitStats) From {
        New UnitStats With {.Name = "Druid", .ShortName = "Druid", .Power = 5, .HP = 200, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry, .IsSummoner = True, .IsHero = True, .SummonerFaction = "Druidic Spirits"},
        New UnitStats With {.Name = "Necromancer", .ShortName = "Necro", .Power = 5, .HP = 200, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry, .IsSummoner = True, .IsHero = True, .SummonerFaction = "Undead"},
        New UnitStats With {.Name = "Golemancer", .ShortName = "Golem", .Power = 5, .HP = 250, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.HeavyInfantry, .IsSummoner = True, .IsHero = True, .SummonerFaction = "Golems"},
        New UnitStats With {.Name = "Elementalist", .ShortName = "Elem", .Power = 5, .HP = 180, .Melee = 1, .Ranged = 0, .DefencePoints = 0, .FoodCost = 1, .Type = UnitType.LightInfantry, .IsSummoner = True, .IsHero = True, .SummonerFaction = "Elementals"}
    }
}
        AllRaces.Add(heroes)


    End Sub

    Public Sub InitializePlayers()
        Players = New List(Of Player)()

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
                System.Diagnostics.Debug.WriteLine($"Warning: Race '{p.Race}' not found in AllRaces. Armies will start empty.")
                Players.Add(p)
                Continue For
            End If

            ' Create 3 armies at starting corner
            For a As Integer = 1 To 3
                Dim army As New Army With {
                .X = startPos.X,
                .Y = startPos.Y,
                .Race = p.Race
            }

                ' Templates we’ll need
                Dim liTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.LightInfantry)
                Dim archerTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.Archer)
                Dim lcTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.LightCavalry)

                If a = 1 Then
                    ' --- Army #1: 500 LI + 500 Archers + 500 Light Cavalry ---
                    army.Units.Add(New Unit(liTemplate, army.Race, 500))
                    army.Units.Add(New Unit(archerTemplate, army.Race, 500))
                    army.Units.Add(New Unit(lcTemplate, army.Race, 500))

                    Dim hero As Unit = CreateSummonerUnit("Druid", 1, p.Race)  ' or "Necromancer"/"Runesmith"/"Mage"
                    army.Units.Add(hero)
                Else
                    ' --- Armies #2 and #3: 100 LI + 100 Light Cavalry ---
                    army.Units.Add(New Unit(liTemplate, army.Race, 100))
                    army.Units.Add(New Unit(lcTemplate, army.Race, 100))
                End If

                ' Add army to player
                p.Armies.Add(army)

                ' Debug log
                Dim logLine As String = $"{p.Race} Army {a} at ({army.X},{army.Y}): " &
                String.Join(", ", army.Units.Select(Function(u) $"{u.Name}({u.Size})")) &
                $" | TotalSoldiers = {army.TotalSoldiers}"
                System.Diagnostics.Debug.WriteLine(logLine)
                rtbInfo.AppendText(logLine & Environment.NewLine)
            Next

            Players.Add(p)
        Next
    End Sub


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

                    If isSpecial Then
                        a.HasUsedSpecial = True

                        ' Ensure a special action exists; if missing, let AI decide (includes emergency recruit)
                        If p.AIControlled AndAlso stepIndex >= a.MoveQueue.Count Then
                            Dim aiArmyIndex As Integer = p.Armies.IndexOf(a)
                            If aiArmyIndex < 0 Then aiArmyIndex = 0
                            AIRecruitArmy(a, p, aiArmyIndex)
                        End If

                        ' Execute special if present
                        If stepIndex < a.MoveQueue.Count Then
                            Dim cmd As ArmyCommand = a.MoveQueue(stepIndex)
                            Dim moveCommand As String = If(cmd.Command, "").ToUpper().Trim()

                            Select Case moveCommand
                                Case "RECRUIT"
                                    Dim unitShortName As String = If(cmd.Parameter, "").Trim().ToLower()
                                    Dim raceUnits As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName.Equals(p.Race, StringComparison.OrdinalIgnoreCase))
                                    If raceUnits IsNot Nothing Then
                                        Dim templateUnit As UnitStats =
                                        raceUnits.Units.FirstOrDefault(Function(u) u.ShortName IsNot Nothing AndAlso u.ShortName.Equals(unitShortName, StringComparison.OrdinalIgnoreCase))
                                        If templateUnit IsNot Nothing Then
                                            RecruitArmyUnits(a, p, templateUnit)
                                        End If
                                    End If
                                    ' Add other specials here if needed...
                            End Select
                        End If

                    Else
                        ' --- Movement step: only armies >= 500 can move ---
                        If a.TotalSoldiers < 500 Then Continue For

                        If stepIndex < a.MoveQueue.Count Then
                            Dim cmd As ArmyCommand = a.MoveQueue(stepIndex)
                            Dim offset As Point = DirectionToOffset(If(cmd.Command, ""))
                            Dim newX As Integer = Math.Max(0, Math.Min(mapSize - 1, a.X + offset.X))
                            Dim newY As Integer = Math.Max(0, Math.Min(mapSize - 1, a.Y + offset.Y))

                            ' Capture only if orthogonally connected
                            If Map(newX, newY, 1) <> p.PlayerNumber AndAlso IsCaptureValidOrthogonal(newX, newY, p.PlayerNumber) Then
                                Map(newX, newY, 1) = p.PlayerNumber
                            End If

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


    Public Sub RecruitArmyUnits(army As Army, player As Player, unitTemplate As UnitStats)
        If army Is Nothing OrElse player Is Nothing OrElse unitTemplate Is Nothing Then Exit Sub

        ' --- Determine desired max recruitable per turn (5% of population, minimum 1) ---
        Dim maxPerTurn As Integer = Math.Max(1, CInt(Math.Floor(player.Population * 0.05)))

        ' Start with population cap
        Dim recruitAmount As Integer = Math.Min(maxPerTurn, player.Population)

        ' --- Parse cost string robustly: supports "I:2, W:1, M:1" and textual fallbacks ---
        Dim ironRequired As Integer = 0
        Dim woodRequired As Integer = 0
        Dim mountsRequired As Integer = 0

        If Not String.IsNullOrWhiteSpace(unitTemplate.Cost) Then
            For Each rawPart In unitTemplate.Cost.Split(","c)
                Dim part As String = rawPart.Trim()
                If part.Length = 0 Then Continue For

                Dim lower As String = part.ToLowerInvariant()
                Dim n As Integer
                Dim idx = part.IndexOf(":"c)

                If lower.StartsWith("i") OrElse lower.Contains("iron") Then
                    If idx >= 0 AndAlso Integer.TryParse(part.Substring(idx + 1).Trim(), n) Then
                        ironRequired += Math.Max(0, n)
                    Else
                        ironRequired += 1
                    End If
                ElseIf lower.StartsWith("w") OrElse lower.Contains("wood") Then
                    If idx >= 0 AndAlso Integer.TryParse(part.Substring(idx + 1).Trim(), n) Then
                        woodRequired += Math.Max(0, n)
                    Else
                        woodRequired += 1
                    End If
                ElseIf lower.StartsWith("m") OrElse lower.Contains("mount") OrElse lower.Contains("horse") OrElse
                   lower.Contains("wolf") OrElse lower.Contains("elk") OrElse lower.Contains("ram") Then
                    If idx >= 0 AndAlso Integer.TryParse(part.Substring(idx + 1).Trim(), n) Then
                        mountsRequired += Math.Max(0, n)
                    Else
                        mountsRequired += 1
                    End If
                End If
            Next
        End If

        ' --- Limit by available resources ---
        If ironRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Iron \ ironRequired)
        If woodRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Wood \ woodRequired)
        If mountsRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Mounts \ mountsRequired)

        ' --- Skip if nothing can be recruited ---
        If recruitAmount <= 0 Then
            Console.WriteLine($"{player.Race} wanted to recruit {unitTemplate.Name} but lacked resources/pop.")
            Exit Sub
        End If

        ' --- Check if unit already exists in army ---
        Dim existingUnit As Unit = army.Units.FirstOrDefault(Function(u) u.Name = unitTemplate.Name AndAlso u.Type = unitTemplate.Type)

        If existingUnit IsNot Nothing Then
            existingUnit.Size += recruitAmount
            ' Ensure standard troops have a sane FoodCost (default 1.0 if unset/negative)
            If existingUnit.FoodCost < 0 Then existingUnit.FoodCost = 1.0
        Else
            ' Use constructor instead of long With block
            Dim newUnit As New Unit(unitTemplate, army.Race, recruitAmount)
            ' Override FoodCost if needed (default 1.0 for standard troops)
            If newUnit.FoodCost < 0 Then newUnit.FoodCost = 1.0
            army.Units.Add(newUnit)
        End If

        ' --- Deduct population and resources ---
        player.Population -= recruitAmount
        If ironRequired > 0 Then player.Iron -= ironRequired * recruitAmount
        If woodRequired > 0 Then player.Wood -= woodRequired * recruitAmount
        If mountsRequired > 0 Then player.Mounts -= mountsRequired * recruitAmount

        Console.WriteLine($"{player.Race} recruits {recruitAmount} {unitTemplate.Name} in army at ({army.X},{army.Y})")
    End Sub


    Private Function GenerateAIMoves(army As Army, player As Player, maxSteps As Integer) As List(Of ArmyCommand)
        Dim moves As New List(Of ArmyCommand)()
        Dim mapSize As Integer = Map.GetLength(0)

        ' Only move if army has >= 500 men
        If army.TotalSoldiers < 500 Then Return moves

        Dim homePos As Point = GetStartingCornerCenter(player.PlayerNumber)
        Dim currentPos As Point = New Point(army.X, army.Y)

        For stepIndex As Integer = 0 To maxSteps - 2 ' Reserve last step for special/recruitment
            Dim bestTile As Point? = Nothing
            Dim bestScore As Double = Double.MinValue

            ' --- Step 1: Evaluate immediate orthogonal tiles first (N/E/S/W) ---
            Dim adjOffsets As Point() = {New Point(0, -1), New Point(1, 0), New Point(0, 1), New Point(-1, 0)}

            For Each dirOffset In adjOffsets
                Dim nx As Integer = currentPos.X + dirOffset.X
                Dim ny As Integer = currentPos.Y + dirOffset.Y
                If nx < 0 OrElse nx >= mapSize OrElse ny < 0 OrElse ny >= mapSize Then Continue For

                ' Skip blocked by active army
                Dim blockingArmy As Army = Players.SelectMany(Function(p) p.Armies) _
                                             .FirstOrDefault(Function(a) a.X = nx AndAlso a.Y = ny AndAlso a.TotalSoldiers >= 500)
                If blockingArmy IsNot Nothing Then Continue For

                ' --- Skip already-owned tiles for capture scoring ---
                If Map(nx, ny, 1) = player.PlayerNumber Then Continue For

                ' --- Score tile ---
                Dim score As Double = 100 ' adjacency bonus

                ' Terrain bonus
                Select Case player.Race.ToLower()
                    Case "elf" : If Map(nx, ny, 0) = 1 Then score += 10
                    Case "dwarf" : If Map(nx, ny, 0) = 3 Then score += 10
                    Case "orc" : If Map(nx, ny, 0) = 2 Then score += 10
                    Case "human" : If Map(nx, ny, 0) = 0 Then score += 10
                End Select

                ' Home-bias
                Dim distHome As Integer = Math.Abs(nx - homePos.X) + Math.Abs(ny - homePos.Y)
                score += Math.Max(0, 20 - distHome)

                ' Update best
                If score > bestScore Then
                    bestScore = score
                    bestTile = New Point(nx, ny)
                End If
            Next

            ' --- Step 2: BFS search within ±5 for nearest unowned tile if no adjacent tile found ---
            If bestTile Is Nothing Then
                Dim bfsQueue As New Queue(Of Point)
                Dim visited(mapSize - 1, mapSize - 1) As Boolean
                bfsQueue.Enqueue(currentPos)
                visited(currentPos.X, currentPos.Y) = True

                While bfsQueue.Count > 0
                    Dim pt As Point = bfsQueue.Dequeue()
                    If Math.Abs(pt.X - currentPos.X) > 5 OrElse Math.Abs(pt.Y - currentPos.Y) > 5 Then Continue While

                    Dim orthoOffsets As Point() = {New Point(0, -1), New Point(1, 0), New Point(0, 1), New Point(-1, 0)}
                    For Each dirOffset In orthoOffsets
                        Dim nx As Integer = pt.X + dirOffset.X
                        Dim ny As Integer = pt.Y + dirOffset.Y
                        If nx < 0 OrElse nx >= mapSize OrElse ny < 0 OrElse ny >= mapSize Then Continue For
                        If visited(nx, ny) Then Continue For
                        visited(nx, ny) = True

                        ' Skip blocked by active army
                        Dim blockingArmy As Army = Players.SelectMany(Function(p) p.Armies) _
                                                 .FirstOrDefault(Function(a) a.X = nx AndAlso a.Y = ny AndAlso a.TotalSoldiers >= 500)
                        If blockingArmy IsNot Nothing Then Continue For

                        ' Skip already-owned tiles for capture scoring
                        If Map(nx, ny, 1) = player.PlayerNumber Then Continue For

                        ' Score tile
                        Dim score As Double = 0
                        Dim distHome As Integer = Math.Abs(nx - homePos.X) + Math.Abs(ny - homePos.Y)
                        score += Math.Max(0, 50 - distHome) ' minor home bias

                        ' Terrain bonus
                        Select Case player.Race.ToLower()
                            Case "elf" : If Map(nx, ny, 0) = 1 Then score += 5
                            Case "dwarf" : If Map(nx, ny, 0) = 3 Then score += 5
                            Case "orc" : If Map(nx, ny, 0) = 2 Then score += 5
                            Case "human" : If Map(nx, ny, 0) = 0 Then score += 5
                        End Select

                        If score > bestScore Then
                            bestScore = score
                            bestTile = New Point(nx, ny)
                        End If

                        bfsQueue.Enqueue(New Point(nx, ny))
                    Next
                End While
            End If

            ' --- Step 3: Fallback to map centre if nothing found ---
            If bestTile Is Nothing Then
                bestTile = New Point(mapSize \ 2, mapSize \ 2)
            End If

            ' --- Step 4: Convert bestTile to an ORTHOGONAL move ---
            Dim dx As Integer = bestTile.Value.X - currentPos.X
            Dim dy As Integer = bestTile.Value.Y - currentPos.Y

            ' Prioritize moving along the axis with largest difference
            Dim moveX As Integer = 0
            Dim moveY As Integer = 0
            If Math.Abs(dx) >= Math.Abs(dy) AndAlso dx <> 0 Then
                moveX = Math.Sign(dx)
            ElseIf dy <> 0 Then
                moveY = Math.Sign(dy)
            End If

            Dim direction As String = ""
            Select Case moveY
                Case -1
                    direction = "N"
                Case 1
                    direction = "S"
            End Select
            If moveX <> 0 Then
                Select Case moveX
                    Case -1 : direction = "W"
                    Case 1 : direction = "E"
                End Select
            End If

            If direction <> "" Then
                moves.Add(New ArmyCommand With {.Command = direction, .Parameter = ""})
                currentPos.X += moveX
                currentPos.Y += moveY
            Else
                Exit For ' already at best tile
            End If
        Next

        Return moves
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
    Public Sub ResolveCombat()
        Dim armiesAlreadyInBattle As New HashSet(Of Army)()
        Dim processedLocations As New HashSet(Of Point)()

        ' Loop through each player
        For Each p As Player In Players
            If p.Armies Is Nothing Then Continue For

            For Each a As Army In p.Armies
                ' Skip armies that cannot fight or already battled
                If a.TotalSoldiers < 500 OrElse armiesAlreadyInBattle.Contains(a) Then Continue For

                Dim grid As List(Of Point) = GetCombatGrid(a)

                For Each pt As Point In grid
                    If processedLocations.Contains(pt) Then Continue For

                    ' Gather all armies on this tile that can fight
                    Dim allArmiesHere As New List(Of Army)
                    For Each pl As Player In Players
                        If pl.Armies IsNot Nothing Then
                            For Each ar As Army In pl.Armies
                                If ar.TotalSoldiers >= 500 AndAlso GetCombatGrid(ar).Contains(pt) AndAlso Not armiesAlreadyInBattle.Contains(ar) Then
                                    allArmiesHere.Add(ar)
                                End If
                            Next
                        End If
                    Next

                    ' Skip if less than 2 distinct players are present
                    Dim distinctPlayers As New HashSet(Of String)(allArmiesHere.Select(Function(ar) ar.Race))
                    If distinctPlayers.Count < 2 Then Continue For

                    processedLocations.Add(pt)

                    ' --- Merge armies per player temporarily for battle ---
                    Dim mergedArmies As New List(Of Army)
                    Dim mergedToOriginal As New Dictionary(Of Army, List(Of Army))

                    For Each race As String In distinctPlayers
                        Dim playerArmies As List(Of Army) = allArmiesHere.Where(Function(ar) ar.Race = race).ToList()

                        Dim mergedArmy As New Army With {
                        .Race = race,
                        .X = playerArmies(0).X,
                        .Y = playerArmies(0).Y
                    }

                        ' Copy units from each source army into the merged army
                        For Each ua As Army In playerArmies
                            For Each u As Unit In ua.Units
                                mergedArmy.Units.Add(New Unit(u)) ' use clone constructor
                            Next
                        Next

                        mergedArmies.Add(mergedArmy)
                        mergedToOriginal(mergedArmy) = playerArmies
                    Next

                    ' --- Immutable pre-battle snapshot (deep copy) BEFORE Battle() mutates anything ---
                    Dim startSnapshot As List(Of Army) = CloneArmiesForSnapshot(mergedArmies)

                    ' --- Conduct battle ---
                    Dim battleLog As BattleLog = Battle(mergedArmies)
                    If battleLog Is Nothing Then Continue For

                    ' --- Distribute casualties back proportionally to original armies ---
                    For Each mergedArmy As Army In mergedArmies
                        Dim originalArmies As List(Of Army) = mergedToOriginal(mergedArmy)

                        For Each mergedUnit As Unit In mergedArmy.Units
                            ' Total size of this unit across all original armies (pre-battle)
                            Dim totalOriginalSize As Integer =
                            originalArmies.Sum(Function(origArmy) origArmy.Units.
                                Where(Function(u) u.Name = mergedUnit.Name AndAlso u.Type = mergedUnit.Type).
                                Sum(Function(u) u.Size))

                            If totalOriginalSize = 0 Then Continue For

                            ' Sum casualties this unit took across all phases in the merged battle
                            Dim mergedCasualties As Integer =
                            battleLog.PhaseEntries.Values.
                                Sum(Function(phaseList) phaseList.
                                    Where(Function(e) e.Defender.Name = mergedUnit.Name AndAlso e.Defender.Type = mergedUnit.Type).
                                    Sum(Function(e) e.Casualties))

                            ' Apply back to each original army proportionally
                            For Each origArmy As Army In originalArmies
                                For Each u As Unit In origArmy.Units
                                    If u.Name = mergedUnit.Name AndAlso u.Type = mergedUnit.Type Then
                                        Dim proportion As Double = 0.0
                                        If totalOriginalSize > 0 Then
                                            proportion = CDbl(u.Size) / CDbl(totalOriginalSize)
                                        End If
                                        Dim casualtiesToApply As Integer = CInt(Math.Round(mergedCasualties * proportion))
                                        u.Size -= casualtiesToApply
                                        If u.Size < 0 Then u.Size = 0
                                    End If
                                Next
                            Next
                        Next
                    Next

                    ' --- Retreat handling for the real armies on the map ---
                    Dim maxStrength As Integer = Integer.MinValue
                    For Each army As Army In allArmiesHere
                        If army.TotalSoldiers > maxStrength Then maxStrength = army.TotalSoldiers
                    Next

                    Dim strongestCount As Integer = 0
                    For Each ar As Army In allArmiesHere
                        If ar.TotalSoldiers = maxStrength Then
                            strongestCount += 1
                        End If
                    Next

                    ' Weaker (or tied-at-top in a multiway-tie) armies go home
                    For Each army As Army In allArmiesHere
                        Dim sendHome As Boolean = (army.TotalSoldiers < maxStrength) OrElse
                          (army.TotalSoldiers = maxStrength AndAlso strongestCount > 1)

                        If sendHome Then
                            SendArmyBackToSpawn(army)          ' now teleports + clears + flags
                            battleLog.RecordRetreat(army)
                        End If
                    Next

                    ' --- Mark these armies as having battled this tick ---
                    For Each army As Army In allArmiesHere
                        armiesAlreadyInBattle.Add(army)
                    Next

                    ' --- Generate and display compact report (NOTE PARAM ORDER) ---
                    rtbInfo.Clear()
                    Dim compactReport As String = GenerateCompactPhaseReport(battleLog, mergedArmies, startSnapshot)
                    rtbInfo.AppendText(compactReport)

                    ' Done with this hotspot
                    Exit For
                Next
            Next
        Next
    End Sub


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


#Region "=== Gameplay Logic ==="

    Public Sub CollectResources()
        Dim rows As Integer = Map.GetLength(0)
        Dim cols As Integer = Map.GetLength(1)

        For Each p In Players
            ' --- Reset per-turn collection ---
            p.FoodCollectedThisTurn = 0
            p.IronCollectedThisTurn = 0
            p.WoodCollectedThisTurn = 0
            p.MountsCollectedThisTurn = 0

            ' We'll accumulate mounts in floating point, then floor AFTER bonus
            Dim mountsCollectedRaw As Double = 0.0

            ' --- Count owned squares ---
            Dim ownedSquares As Integer = 0
            For x As Integer = 0 To rows - 1
                For y As Integer = 0 To cols - 1
                    If Map(x, y, 1) = p.PlayerNumber Then
                        ownedSquares += 1
                    End If
                Next
            Next

            ' Avoid division by zero
            Dim popPerSquare As Integer = 0
            If ownedSquares > 0 Then
                popPerSquare = p.Population \ ownedSquares
            End If

            ' --- Determine preferred terrain ---
            Dim preferredTerrain As Integer
            Select Case p.Race.ToLower()
                Case "elf" : preferredTerrain = 1
                Case "dwarf" : preferredTerrain = 3
                Case "orc" : preferredTerrain = 2
                Case "human" : preferredTerrain = 0
                Case Else : preferredTerrain = -1
            End Select

            ' --- Loop through owned squares only ---
            For x As Integer = 0 To rows - 1
                For y As Integer = 0 To cols - 1
                    If Map(x, y, 1) = p.PlayerNumber Then
                        Dim terrain As Integer = Map(x, y, 0)

                        ' Base resources per square
                        Dim foodPerSquare As Integer = 1
                        Dim ironPerSquare As Integer = 1
                        Dim woodPerSquare As Integer = 1
                        Dim mountPerSquare As Double = 0.0

                        ' Racial terrain bonus (extra food + mounts only on favoured)
                        If terrain = preferredTerrain Then
                            foodPerSquare += 1
                            mountPerSquare = 0.5
                        End If

                        ' Accumulate (note: mounts go to a raw double)
                        p.FoodCollectedThisTurn += foodPerSquare * popPerSquare
                        p.IronCollectedThisTurn += ironPerSquare * popPerSquare
                        p.WoodCollectedThisTurn += woodPerSquare * popPerSquare
                        mountsCollectedRaw += mountPerSquare * popPerSquare
                    End If
                Next
            Next

            ' --- Apply territory bonus (0.5% per owned tile) ---
            Dim territoryBonus As Double = ownedSquares * 0.005
            p.FoodCollectedThisTurn = CInt(p.FoodCollectedThisTurn * (1 + territoryBonus))
            p.IronCollectedThisTurn = CInt(p.IronCollectedThisTurn * (1 + territoryBonus))
            p.WoodCollectedThisTurn = CInt(p.WoodCollectedThisTurn * (1 + territoryBonus))

            ' Apply divisors to iron/wood/mounts AFTER bonus
            Const IRON_DIVISOR As Integer = 40
            Const WOOD_DIVISOR As Integer = 40
            Const MOUNTS_DIVISOR As Integer = 60

            p.IronCollectedThisTurn = CInt(Math.Floor(p.IronCollectedThisTurn / IRON_DIVISOR))
            p.WoodCollectedThisTurn = CInt(Math.Floor(p.WoodCollectedThisTurn / WOOD_DIVISOR))
            p.MountsCollectedThisTurn = CInt(Math.Floor((mountsCollectedRaw * (1 + territoryBonus)) / MOUNTS_DIVISOR))

            ' --- Add to cumulative totals ---
            p.Iron += p.IronCollectedThisTurn
            p.Wood += p.WoodCollectedThisTurn
            p.Mounts += p.MountsCollectedThisTurn
            'p.Food = p.FoodCollectedThisTurn
            p.GoldCollectedThisTurn = p.Population \ 100
            p.Gold += p.GoldCollectedThisTurn
        Next
    End Sub


    Public Sub GrowPopulationAndFeedEverybody()
        ' Loop through all players
        For Each p In Players
            ' Calculate total food required for armies
            Dim armyFoodRequirement As Integer = 0
            If p.Armies IsNot Nothing Then
                For Each a In p.Armies
                    armyFoodRequirement += CInt(Math.Floor(a.Units.Sum(Function(u) u.Size * u.FoodCost)))
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

        ' --- 3. Produce trade goods (based on updated population) ---
        ProduceTradeGoods()

        ' --- 4. Summoners act ---
        ProcessSummoners()

        ' --- 5. Execute army movements step by step ---
        ProcessTurn()

        ' --- 6. Refresh map display ---
        pnlMap.Invalidate()

        ' --- 7. Update empire/resource info in RichTextBox ---
        UpdateResourceInfo()

        rtbPlayerSummary.Clear()
        rtbPlayerSummary.AppendText(GenerateEmpireSummary)

        currentTurnNumber += 1

        ' === 6. Generate a new mercenary offer for this turn ===
        CurrentMercOffer = GenerateMercenaryOffer(currentTurnNumber)

        If CurrentMercOffer IsNot Nothing Then
            Debug.WriteLine("Turn " & currentTurnNumber & " Mercenary Offer: " & CurrentMercOffer.ToString())
        End If


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
                        tileTotals(pt) += a.TotalSoldiers
                    Else
                        tileTotals(pt) = a.TotalSoldiers
                        tileOwner(pt) = p.PlayerNumber
                    End If
                Next
            End If
        Next 'D

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

    Private Sub UpdateResourceInfo()
        rtbResourceInfo.Clear()

        For Each p In Players
            rtbResourceInfo.AppendText($"Player {p.PlayerNumber + 1} - {p.Race}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Population: {p.Population}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Food Collected: {p.FoodCollectedThisTurn}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Iron Collected: {p.IronCollectedThisTurn}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Wood Collected: {p.WoodCollectedThisTurn}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Mounts Collected: {p.MountsCollectedThisTurn}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Total Mounts: {p.Mounts}" & Environment.NewLine)

            ' --- Trade goods ---
            rtbResourceInfo.AppendText($"Gems: {p.Gems}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Amber: {p.Amber}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Wine: {p.Wine}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Furs: {p.Furs}" & Environment.NewLine)

            ' --- Army sizes ---
            If p.Armies IsNot Nothing AndAlso p.Armies.Count > 0 Then
                rtbResourceInfo.AppendText("Armies:" & Environment.NewLine)
                For i As Integer = 0 To p.Armies.Count - 1
                    Dim a = p.Armies(i)
                    rtbResourceInfo.AppendText($"  Army {i + 1}: {a.TotalSoldiers} soldiers at ({a.X},{a.Y})" & Environment.NewLine)
                Next
            Else
                rtbResourceInfo.AppendText("No armies" & Environment.NewLine)
            End If

            rtbResourceInfo.AppendText(Environment.NewLine)
        Next
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
                ApplyProportionalDamage(defArmy, attackers, phaseName, unitSnapshot, battleLog)
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
            ApplyProportionalDamage(defArmy, winnerLC, "Chase", chaseSnapshot, battleLog)
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
                                   attackingUnits As List(Of Unit),
                                   phase As String,
                                   unitSnapshot As Dictionary(Of Unit, Integer),
                                   battleLog As BattleLog)

        ' --- Initialize dictionary to accumulate casualties per defender unit ---
        Dim calculatedCasualties As New Dictionary(Of Unit, Long)
        For Each defUnit In defArmy.Units
            calculatedCasualties(defUnit) = 0
        Next

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
                            Debug.WriteLine($"[DEBUG] Flying unit {atkUnit.Name} detected in battle (Size={atkUnit.Size})")
                            atkExplanation = $"{atkUnit.Melee} melee (flying, double in chase)"
                        Else
                            atkExplanation = $"{atkUnit.Melee} melee for chase"
                        End If
                    Else
                        Continue For
                    End If
            End Select

            ' --- Total HP-weighted size of defenders ---
            Dim totalDefWeight As Double = defArmy.Units.Sum(Function(u) unitSnapshot(u) * u.GetEffectiveHP())
            If totalDefWeight = 0 Then Continue For

            ' --- Apply proportional damage to each defender unit ---
            For Each defUnit In defArmy.Units
                Dim sizeBefore As Integer = unitSnapshot(defUnit) ' snapshot at phase start
                Dim unitWeight As Double = sizeBefore * defUnit.GetEffectiveHP()

                ' Raw damage proportional to weight
                Dim rawDamage As Double = atkValue * atkSize * (unitWeight / totalDefWeight)

                ' Mitigation fraction + explanation
                Dim mitResult = GetUnitMitigation(defUnit)
                Dim mitigationValue As Double = mitResult.Mitigation
                Dim mitigationExplanation As String = mitResult.Explanation

                ' Final damage after mitigation
                Dim finalDamage As Double = rawDamage * (1 - mitigationValue)

                ' Calculate casualties (cannot exceed unit size)
                Dim casualties As Integer = Math.Min(sizeBefore, CInt(Math.Floor(finalDamage / defUnit.GetEffectiveHP())))

                ' --- Record entry in BattleLog ---
                If battleLog IsNot Nothing Then
                    Dim entry As New BattleEntry With {
                    .Attacker = atkUnit,
                    .Defender = defUnit,
                    .Phase = phase,
                    .SizeBefore = sizeBefore,
                    .SizeAfter = Math.Max(0, sizeBefore - CInt(calculatedCasualties(defUnit) + casualties)),
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
        Next
    End Sub


    Public Function GetUnitMitigation(unit As Unit) As (Mitigation As Double, Explanation As String)
        Dim mitigation As Double = 0.0
        Dim parts As New List(Of String)

        ' Normalize nullable strings
        Dim armour As String = If(unit.Armour, String.Empty)
        Dim shield As String = If(unit.Shield, String.Empty)
        Dim unitName As String = If(unit.Name, String.Empty)

        ' Armour mitigation
        Select Case armour.ToLowerInvariant()
            Case "chainmail"
                mitigation += 0.25
                parts.Add("25% chainmail")
            Case "plate"
                mitigation += 0.5
                parts.Add("50% plate")
        End Select

        ' Shield mitigation
        Select Case shield.ToLowerInvariant()
            Case "wooden"
                mitigation += 0.1
                parts.Add("10% wooden shield")
            Case "iron"
                mitigation += 0.2
                parts.Add("20% iron shield")
        End Select

        ' DefencePoints mitigation — use effective value for heroes
        Dim effectiveDef As Integer = unit.GetEffectiveDefencePoints()
        If effectiveDef > 0 Then
            Dim defMit As Double = effectiveDef * 0.05
            mitigation += defMit
            parts.Add($"{effectiveDef * 5}% defence points")
        End If

        ' Cap total mitigation at 80%
        If mitigation > 0.8 Then mitigation = 0.8

        Dim explanation As String = If(parts.Count > 0, String.Join(" + ", parts), "No mitigation")
        Return (mitigation, explanation)
    End Function


    Public Sub AIRecruitArmy(army As Army, player As Player, armyIndex As Integer)
        If army Is Nothing OrElse player Is Nothing Then Exit Sub

        ' Role by index (used for flavor if you later want to bias random choices)
        Dim role As String
        Select Case armyIndex
            Case 0 : role = "Offensive"
            Case 1 : role = "Flexible"
            Case 2 : role = "Defensive"
            Case Else : role = "Flexible"
        End Select

        Dim raceUnits As RaceUnits =
        AllRaces.FirstOrDefault(Function(r) r.RaceName.Equals(player.Race, StringComparison.OrdinalIgnoreCase))
        If raceUnits Is Nothing Then Exit Sub

        ' ---------- EMERGENCY: army <500 -> force a recruit if affordable ----------
        If army.TotalSoldiers < 500 Then
            ' Preferred type by race (your "best" list)
            Dim preferred As UnitType
            Select Case player.Race.ToLowerInvariant()
                Case "elf" : preferred = UnitType.Archer
                Case "orc" : preferred = UnitType.LightInfantry
                Case "dwarf" : preferred = UnitType.HeavyInfantry
                Case "human" : preferred = UnitType.HeavyCavalry
                Case Else : preferred = UnitType.LightInfantry
            End Select

            ' Try preferred first
            Dim pick As UnitStats = raceUnits.Units.FirstOrDefault(Function(u) u.Type = preferred)
            If pick Is Nothing AndAlso raceUnits.Units.Count > 0 Then pick = raceUnits.Units(0)

            If pick IsNot Nothing AndAlso MaxRecruitableUnits(player, pick) > 0 AndAlso player.Population > 0 Then
                army.MoveQueue.Add(New ArmyCommand With {.Command = "RECRUIT", .Parameter = pick.ShortName})
                Exit Sub
            End If

            ' Fallback to absolute cheapest affordable
            Dim cheapest As UnitStats = GetCheapestAvailableUnit(player)
            If cheapest IsNot Nothing AndAlso MaxRecruitableUnits(player, cheapest) > 0 AndAlso player.Population > 0 Then
                army.MoveQueue.Add(New ArmyCommand With {.Command = "RECRUIT", .Parameter = cheapest.ShortName})
                Exit Sub
            End If
            ' Nothing affordable => no recruit this turn
        End If
        ' ---------- /EMERGENCY ----------

        ' Tactical context (optional for future flavor)
        Dim nearbyEnemies = Players.SelectMany(Function(pp) pp.Armies) _
        .Where(Function(a) a.Race <> player.Race AndAlso Math.Abs(a.X - army.X) <= 5 AndAlso Math.Abs(a.Y - army.Y) <= 5) _
        .ToList()
        Dim underThreat As Boolean = nearbyEnemies.Any(Function(ea) ea.TotalSoldiers >= army.TotalSoldiers)
        Dim hasOpportunity As Boolean = nearbyEnemies.Any(Function(ea) ea.TotalSoldiers < army.TotalSoldiers)

        ' === Gate: random 25% OR badly-behind (vs strongest) with cooldown ===
        Dim randomRecruit As Boolean = (rnd.Next(0, 100) < 25)

        Dim ourStrength As Integer = player.Armies.Sum(Function(a) a.TotalSoldiers)
        Dim enemyStrongest As Integer =
        Players.Where(Function(pp) Not pp.Race.Equals(player.Race, StringComparison.OrdinalIgnoreCase)) _
               .GroupBy(Function(pp) pp.Race, StringComparer.OrdinalIgnoreCase) _
               .Select(Function(g) g.SelectMany(Function(pp) pp.Armies).Sum(Function(a) a.TotalSoldiers)) _
               .DefaultIfEmpty(0) _
               .Max()

        ' "Badly behind" := strictly below 75% of the strongest rival
        Dim badlyBehind As Boolean = (enemyStrongest > 0 AndAlso ourStrength < CInt(Math.Floor(enemyStrongest * 0.75)))
        Dim mayUseCatchUp As Boolean = (badlyBehind AndAlso player.CatchUpCooldown <= 0)

        ' If neither random nor catch-up applies, do nothing this special step
        If Not randomRecruit AndAlso Not mayUseCatchUp Then Exit Sub

        If mayUseCatchUp Then
            ' Start a small cooldown to avoid spamming catch-up every turn
            player.CatchUpCooldown = 2
        End If

        ' === Choose WHAT to recruit ===
        Dim chosenUnit As UnitStats = Nothing

        If mayUseCatchUp Then
            ' Catch-up: cheapest affordable to ensure growth
            chosenUnit = GetCheapestAvailableUnit(player)
        Else
            ' Random: 50% preferred, else random among the other four types
            Dim preferred As UnitType
            Select Case player.Race.ToLowerInvariant()
                Case "elf" : preferred = UnitType.Archer
                Case "orc" : preferred = UnitType.LightInfantry
                Case "dwarf" : preferred = UnitType.HeavyInfantry
                Case "human" : preferred = UnitType.HeavyCavalry
                Case Else : preferred = UnitType.LightInfantry
            End Select

            If rnd.NextDouble() < 0.5 Then
                chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = preferred)
            Else
                Dim allTypes As New List(Of UnitType)([Enum].GetValues(GetType(UnitType)).Cast(Of UnitType)())
                allTypes.Remove(preferred)
                Dim randomType As UnitType = allTypes(rnd.Next(allTypes.Count))
                chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = randomType)
            End If

            If chosenUnit Is Nothing AndAlso raceUnits.Units.Count > 0 Then
                chosenUnit = raceUnits.Units(rnd.Next(raceUnits.Units.Count))
            End If
        End If

        ' Enqueue only if affordable and population available
        If chosenUnit IsNot Nothing Then
            Dim maxByResources As Integer = MaxRecruitableUnits(player, chosenUnit)
            If maxByResources > 0 AndAlso player.Population > 0 Then
                army.MoveQueue.Add(New ArmyCommand With {.Command = "RECRUIT", .Parameter = chosenUnit.ShortName})
            End If
        End If
    End Sub



    Private Function MaxRecruitableUnits(player As Player, unit As UnitStats) As Integer
        If player Is Nothing OrElse unit Is Nothing Then Return 0

        ' --- 1) Base cap: 5% of population, minimum 1 ---
        Dim desiredRecruit As Integer = Math.Max(1, CInt(Math.Floor(player.Population * 0.05)))

        ' --- 2) Parse costs like "I:2, W:1, M:1" (case-insensitive) ---
        Dim reqIron As Integer = 0
        Dim reqWood As Integer = 0
        Dim reqMounts As Integer = 0

        If Not String.IsNullOrWhiteSpace(unit.Cost) Then
            For Each part In unit.Cost.Split(","c)
                Dim token As String = part.Trim().ToUpperInvariant()
                If token.StartsWith("I:") Then reqIron += CInt(Val(token.Substring(2)))
                If token.StartsWith("W:") Then reqWood += CInt(Val(token.Substring(2)))
                If token.StartsWith("M:") Then reqMounts += CInt(Val(token.Substring(2)))
            Next
        End If

        ' --- 3) Cap by resources (per-unit costs) ---
        Dim maxByPop As Integer = Math.Min(desiredRecruit, player.Population)

        Dim maxByIron As Integer = If(reqIron > 0, player.Iron \ reqIron, maxByPop)
        Dim maxByWood As Integer = If(reqWood > 0, player.Wood \ reqWood, maxByPop)
        Dim maxByMounts As Integer = If(reqMounts > 0, player.Mounts \ reqMounts, maxByPop)

        Dim actual As Integer = Math.Min(maxByPop, Math.Min(maxByIron, Math.Min(maxByWood, maxByMounts)))

        If actual < 0 Then actual = 0
        Return actual
    End Function


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
        ' We assume mergedArmies and startSnapshot are in the same order (they are created that way).
        Dim unitOrderByArmy As New Dictionary(Of Army, List(Of Unit))()
        Dim startSizesByArmy As New Dictionary(Of Army, List(Of Integer))()
        Dim indexByKey As New Dictionary(Of Army, Dictionary(Of String, Integer))()

        For i As Integer = 0 To startSnapshot.Count - 1
            Dim snapA = startSnapshot(i)
            Dim liveA = mergedArmies(i)

            ' Stable order = snapshot order
            unitOrderByArmy(liveA) = New List(Of Unit)(
            snapA.Units.Select(Function(u) New Unit(u) With {.Size = u.Size})
        )
            startSizesByArmy(liveA) = New List(Of Integer)(snapA.Units.Select(Function(u) u.Size))

            ' index lookup by key (Name|Type)
            Dim idxMap As New Dictionary(Of String, Integer)(StringComparer.OrdinalIgnoreCase)
            For j As Integer = 0 To snapA.Units.Count - 1
                idxMap(KeyOf(snapA.Units(j))) = j
            Next
            indexByKey(liveA) = idxMap
        Next

        ' ---------- Start of Battle (from immutable snapshot ONLY) ----------
        sb.AppendLine("Start of Battle:")
        For i As Integer = 0 To startSnapshot.Count - 1
            Dim snapA = startSnapshot(i)
            Dim parts = New List(Of String)
            For Each u In snapA.Units
                parts.Add($"{u.Name} ({u.Size})")
            Next
            Dim total0 As Integer = snapA.Units.Sum(Function(u) u.Size)
            sb.AppendLine($"{snapA.Race} Army: {String.Join(", ", parts)} | Total: {total0}")
        Next
        sb.AppendLine(New String("-"c, 60))

        ' ---------- Phase-by-phase: apply casualties to a working copy of sizes ----------
        Dim currentSizesByArmy As New Dictionary(Of Army, Integer())()
        For Each liveA In mergedArmies
            currentSizesByArmy(liveA) = startSizesByArmy(liveA).ToArray() ' copy
        Next

        Dim phases As String() = {"Ranged", "Charge", "Melee", "Chase"}

        For Each phase In phases
            ' Skip entire phase if zero total casualties (either side)
            If Not PhaseHadCasualties(battleLog, phase) Then
                Continue For
            End If

            ' 1) Phase title + narrative summary FIRST
            Dim phaseIndex As Integer = Array.IndexOf(phases, phase)
            sb.AppendLine($"{phase} Phase")

            Dim summaryLine As String = BuildPhaseSummary(battleLog, phase, phaseIndex)
            If Not String.IsNullOrEmpty(summaryLine) Then
                sb.AppendLine(summaryLine)
                sb.AppendLine() ' spacer
            End If

            ' 2) Apply casualties for THIS phase to the working copy
            If battleLog IsNot Nothing AndAlso battleLog.PhaseEntries.ContainsKey(phase) Then
                For Each entry In battleLog.PhaseEntries(phase)
                    Dim owningArmy As Army = Nothing
                    If ownerByUnit.TryGetValue(entry.Defender, owningArmy) Then
                        Dim key As String = entry.Defender.Name & "|" & CInt(entry.Defender.Type).ToString()
                        Dim idx As Integer
                        If indexByKey(owningArmy).TryGetValue(key, idx) Then
                            Dim arr = currentSizesByArmy(owningArmy)
                            Dim before As Integer = arr(idx)
                            Dim after As Integer = Math.Max(0, before - entry.Casualties)
                            arr(idx) = after
                        End If
                    End If
                Next
            End If

            ' 3) Now print "After <Phase> Phase" header and the updated unit lists
            sb.AppendLine($"After {phase} Phase:")
            For i As Integer = 0 To startSnapshot.Count - 1
                Dim snapA = startSnapshot(i)
                Dim liveA = mergedArmies(i)
                Dim names = unitOrderByArmy(liveA)
                Dim sizes = currentSizesByArmy(liveA)

                Dim parts As New List(Of String)
                For j As Integer = 0 To names.Count - 1
                    parts.Add($"{names(j).Name} ({sizes(j)})")
                Next
                Dim totalNow As Integer = sizes.Sum()
                sb.AppendLine($"{snapA.Race} Army: {String.Join(", ", parts)} | Total: {totalNow}")
            Next

            sb.AppendLine(New String("-"c, 60))
        Next

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

        ' ---------- Result (compare by Race, not by object reference) ----------
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
        Dim seed As Integer = (Math.Max(1, currentTurnNumber) * 97) + (phaseIndex * 17)
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
            ' --- Header ---
            sb.AppendLine($"{p.Race} Player (Player {p.PlayerNumber + 1})")
            sb.AppendLine($"Population: {p.Population}")
            sb.AppendLine($"Iron: {p.Iron} (+{p.IronCollectedThisTurn} this turn)")
            sb.AppendLine($"Wood: {p.Wood} (+{p.WoodCollectedThisTurn} this turn)")

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

            ' --- Trade goods (totals only) ---
            sb.AppendLine($"Gems: {p.Gems}")
            sb.AppendLine($"Amber: {p.Amber}")
            sb.AppendLine($"Wine: {p.Wine}")
            sb.AppendLine($"Furs: {p.Furs}")

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

        ' --- Racial preferences (kept simple and readable) ---
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

        ' Small local parser for "I:x, W:y, M:z"
        Dim parseCosts =
        Function(costSpec As String) As (i As Integer, w As Integer, m As Integer, total As Integer)
            Dim ri As Integer = 0, rw As Integer = 0, rm As Integer = 0
            If Not String.IsNullOrWhiteSpace(costSpec) Then
                For Each part In costSpec.Split(","c)
                    Dim t As String = part.Trim().ToUpperInvariant()
                    If t.StartsWith("I:") Then ri += CInt(Val(t.Substring(2)))
                    If t.StartsWith("W:") Then rw += CInt(Val(t.Substring(2)))
                    If t.StartsWith("M:") Then rm += CInt(Val(t.Substring(2)))
                Next
            End If
            Return (ri, rw, rm, ri + rw + rm)
        End Function

        ' Collect affordable candidates
        Dim preferredAffordable As New List(Of (u As UnitStats, tot As Integer, m As Integer, i As Integer, w As Integer))()
        Dim globalAffordable As New List(Of (u As UnitStats, tot As Integer, m As Integer, i As Integer, w As Integer))()

        For Each u In raceUnits.Units
            Dim c = parseCosts(u.Cost)
            Dim canAfford As Boolean =
            (c.i = 0 OrElse player.Iron >= c.i) AndAlso
            (c.w = 0 OrElse player.Wood >= c.w) AndAlso
            (c.m = 0 OrElse player.Mounts >= c.m)

            If Not canAfford Then Continue For

            Dim tup = (u, c.total, c.m, c.i, c.w)
            If preferredTypes.Contains(u.Type) Then
                preferredAffordable.Add(tup)
            Else
                globalAffordable.Add(tup)
            End If
        Next

        ' Pick best from a list using deterministic tie-breaking
        Dim pickBest =
        Function(lst As List(Of (u As UnitStats, tot As Integer, m As Integer, i As Integer, w As Integer))) As UnitStats
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

        ' Prefer race-flavoured cheap picks; otherwise, any cheapest affordable
        Dim chosen As UnitStats = pickBest(preferredAffordable)
        If chosen Is Nothing Then chosen = pickBest(globalAffordable)

        Return chosen
    End Function



    Private Sub ParseCost(spec As String, ByRef iron As Integer, ByRef wood As Integer, ByRef mounts As Integer)
        iron = 0 : wood = 0 : mounts = 0
        For Each part In spec.Split(","c)
            Dim p = part.Trim().ToUpperInvariant()
            If p.StartsWith("I:") Then iron += CInt(Val(p.Substring(2)))
            If p.StartsWith("W:") Then wood += CInt(Val(p.Substring(2)))
            If p.StartsWith("M:") Then mounts += CInt(Val(p.Substring(2)))
        Next
    End Sub


#Region "Market Stuff"

    ' === Trade Goods Production ===
    Public Sub ProduceTradeGoods()
        For Each p In Players
            Select Case p.Race.ToLowerInvariant()
                Case "dwarf"
                    Dim produced As Integer = Math.Max(1, p.Population \ 2000)
                    p.Gems += produced
                    Debug.WriteLine($"[MARKET] {p.Race} Player produced {produced} Gems (Total: {p.Gems})")

                Case "elf"
                    Dim produced As Integer = Math.Max(1, p.Population \ 2000)
                    p.Amber += produced
                    Debug.WriteLine($"[MARKET] {p.Race} Player produced {produced} Amber (Total: {p.Amber})")

                Case "human"
                    Dim produced As Integer = Math.Max(1, p.Population \ 2000)
                    p.Wine += produced
                    Debug.WriteLine($"[MARKET] {p.Race} Player produced {produced} Wine (Total: {p.Wine})")

                Case "orc"
                    Dim produced As Integer = Math.Max(1, p.Population \ 2000)
                    p.Furs += produced
                    Debug.WriteLine($"[MARKET] {p.Race} Player produced {produced} Furs (Total: {p.Furs})")
            End Select
        Next
    End Sub


    Private Const FeeRate As Double = 0.1

    Private Function ApplyDiminishingReturns(goldHeld As Integer, profit As Double) As Integer
        Dim efficiency As Double = 1000.0 / (1000.0 + goldHeld)
        Return CInt(profit * efficiency)
    End Function

    Public Sub SellGoods(p As Player, good As String, amount As Integer, m As Market)
        Dim price As Double
        Select Case good.ToLower()
            Case "gems" : price = m.GemPrice : p.Gems -= amount
            Case "amber" : price = m.AmberPrice : p.Amber -= amount
            Case "wine" : price = m.WinePrice : p.Wine -= amount
            Case "furs" : price = m.FurPrice : p.Furs -= amount
            Case "iron" : price = m.IronPrice : p.Iron -= amount
            Case "wood" : price = m.WoodPrice : p.Wood -= amount
        End Select

        Dim gross As Double = price * amount
        Dim afterFee As Double = gross * (1 - FeeRate)
        Dim profit As Integer = ApplyDiminishingReturns(p.Gold, afterFee)
        p.Gold += profit
    End Sub

    Public Sub BuyGoods(p As Player, good As String, amount As Integer, m As Market)
        Dim price As Double
        Select Case good.ToLower()
            Case "gems" : price = m.GemPrice : p.Gems += amount
            Case "amber" : price = m.AmberPrice : p.Amber += amount
            Case "wine" : price = m.WinePrice : p.Wine += amount
            Case "furs" : price = m.FurPrice : p.Furs += amount
            Case "iron" : price = m.IronPrice : p.Iron += amount
            Case "wood" : price = m.WoodPrice : p.Wood += amount
        End Select

        Dim cost As Double = price * amount
        Dim afterFee As Double = cost * (1 + FeeRate)
        p.Gold -= CInt(afterFee)
    End Sub

    Public Sub UpdatePrices(m As Market, demand As Dictionary(Of String, Integer), supply As Dictionary(Of String, Integer))
        m.GemPrice = AdjustPrice(m.GemPrice, demand("gems"), supply("gems"), 500)
        m.AmberPrice = AdjustPrice(m.AmberPrice, demand("amber"), supply("amber"), 500)
        m.WinePrice = AdjustPrice(m.WinePrice, demand("wine"), supply("wine"), 500)
        m.FurPrice = AdjustPrice(m.FurPrice, demand("furs"), supply("furs"), 500)
        m.IronPrice = AdjustPrice(m.IronPrice, demand("iron"), supply("iron"), 2000)
        m.WoodPrice = AdjustPrice(m.WoodPrice, demand("wood"), supply("wood"), 2000)
    End Sub

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


#End Region


#Region "Mercenary Stuff"

    Private CurrentMercOffer As MercenaryArmy

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


    Private Sub AwardMercenariesToPlayer(offer As MercenaryArmy, winner As Player)
        If winner Is Nothing OrElse winner.Armies Is Nothing OrElse winner.Armies.Count = 0 Then Exit Sub
        Dim target As Army = winner.Armies(0) ' For now, always Army 1

        For Each s In offer.Units
            If s.Hero IsNot Nothing Then
                ' === Hero mercenary ===
                target.Units.Add(s.Hero)

            ElseIf s.Template IsNot Nothing Then
                ' === Normal mercenary ===
                Dim t As UnitStats = s.Template

                ' Merge if this type already exists
                Dim existing = target.Units.FirstOrDefault(Function(u) u.Name = t.Name AndAlso u.Type = t.Type)
                If existing IsNot Nothing Then
                    existing.Size += s.Count
                Else
                    target.Units.Add(New Unit(t, winner.Race, s.Count))
                End If
            End If
        Next
    End Sub

    Private Function GenerateMercenaryOffer(turnNumber As Integer) As MercenaryArmy
        Dim rnd As New Random()

        ' === 0. 25% chance: Hero mercenary ===
        Dim heroTypes As List(Of String) =
        AllRaces.SelectMany(Function(r) r.Units).
                 Where(Function(u) u.IsHero).
                 Select(Function(u) u.Name).
                 ToList()

        If heroTypes.Count > 0 AndAlso rnd.NextDouble() < 0.25 Then
            Dim heroType As String = heroTypes(rnd.Next(heroTypes.Count))

            Dim mercArmy As New MercenaryArmy With {
            .Faction = $"{heroType} (Hero)",
            .Units = New List(Of MercenaryStack),
            .MinBid = 1
        }

            ' Hero at level = current turn
            Dim heroUnit As Unit = CreateSummonerUnit(heroType, turnNumber, "Neutral")
            mercArmy.Units.Add(New MercenaryStack With {.Hero = heroUnit})

            Return mercArmy
        End If

        ' === 1. Budget scales with turn ===
        Dim basePower As Integer = 150
        Dim powerGrowth As Integer = 50
        Dim maxBudget As Integer = basePower + (turnNumber * powerGrowth)

        Dim mercFactions As String() = {"Lizardmen", "Barbarians", "Ogres", "Gnolls", "Dragons", "Goblins", "Kobolds", "Peasants", "Gnomes", "Beasts", "Giants", "Werecreatures", "Trolls", "Skulkrin"}
        Dim mercArmyNormal As MercenaryArmy = Nothing

        Dim outerGuard As Integer = 1000 ' prevent infinite faction loop
        Do While outerGuard > 0
            outerGuard -= 1

            ' === 2. Pick faction ===
            Dim faction As String = mercFactions(rnd.Next(mercFactions.Length))
            Dim roster As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName.Equals(faction, StringComparison.OrdinalIgnoreCase))
            If roster Is Nothing OrElse roster.Units.Count = 0 Then Continue Do

            mercArmyNormal = New MercenaryArmy With {
            .Faction = faction,
            .Units = New List(Of MercenaryStack),
            .MinBid = 1
        }

            Dim remaining As Integer = maxBudget
            Dim cheapestPower As Integer = roster.Units.Min(Function(u) u.Power)

            ' === Normal factions ===
            Dim hasFodder As Boolean = roster.Units.Any(Function(u) u.Power <= 2)

            ' --- Allowed block sizes
            Dim blockSizes As Integer()
            If hasFodder Then
                blockSizes = {10} ' Only groups of 10
            Else
                blockSizes = {10, 5, 1} ' fallback allowed
            End If

            Dim pickedAnything As Boolean = False
            Dim innerGuard As Integer = 10000

            Do While remaining >= cheapestPower AndAlso innerGuard > 0
                innerGuard -= 1

                ' === Bias toward earlier units in the roster ===
                Dim weighted As New List(Of UnitStats)
                For i As Integer = 0 To roster.Units.Count - 1
                    Dim u = roster.Units(i)
                    ' More weight for earlier units (weaker/scrubbier ones if ordered that way)
                    Dim weight As Integer = Math.Max(1, (roster.Units.Count - i) * 5)
                    For n As Integer = 1 To weight
                        weighted.Add(u)
                    Next
                Next

                Dim pick As UnitStats = weighted(rnd.Next(weighted.Count))

                ' Skip units that don't fit budget
                If pick.Power <= 0 OrElse pick.Power > remaining Then Continue Do

                ' Find biggest block that fits
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
                Debug.WriteLine("Guard triggered in GenerateMercenaryOffer inner loop — possible bad roster config.")
            End If

            ' Guarantee at least 1 unit if nothing was picked
            If Not pickedAnything Then
                Dim cheapest = roster.Units.OrderBy(Function(u) u.Power).First()
                mercArmyNormal.Units.Add(New MercenaryStack With {.Template = cheapest, .Count = 1})
            End If

            ' === Return if valid ===
            If mercArmyNormal.Units.Count > 0 Then Exit Do
        Loop

        If outerGuard = 0 Then
            Debug.WriteLine("Guard triggered in GenerateMercenaryOffer outer loop — possible issue with faction selection.")
        End If

        Return mercArmyNormal
    End Function



    Private Sub ResolveMercenaryAuction(bids As Dictionary(Of Player, Integer))
        ' No active offer?
        If CurrentMercOffer Is Nothing OrElse CurrentMercOffer.Units.Count = 0 Then Exit Sub

        ' No bids at all -> discard the offer
        If bids Is Nothing OrElse bids.Count = 0 Then
            Debug.WriteLine("No bids received; mercenary offer dismissed.")
            CurrentMercOffer = Nothing
            Exit Sub
        End If

        ' === Filter invalid bids ===
        Dim candidateBids = bids.
        Where(Function(kv) kv.Value > 0 AndAlso kv.Key IsNot Nothing AndAlso kv.Key.Gold >= kv.Value).
        ToList()

        If candidateBids.Count = 0 Then
            Debug.WriteLine("No valid bids (must be >0 and within available gold); mercenary offer dismissed.")
            CurrentMercOffer = Nothing
            Exit Sub
        End If

        ' === Winner selection ===
        Dim topAmount = candidateBids.Max(Function(kv) kv.Value)
        Dim topBidders = candidateBids.Where(Function(kv) kv.Value = topAmount).ToList()

        Dim winnerKV As KeyValuePair(Of Player, Integer)
        If topBidders.Count = 1 Then
            winnerKV = topBidders(0)
        Else
            ' Tie-breaker: richest bidder, then lowest PlayerNumber
            winnerKV = topBidders.OrderByDescending(Function(kv) kv.Key.Gold).
                              ThenBy(Function(kv) kv.Key.PlayerNumber).
                              First()
        End If

        Dim winner As Player = winnerKV.Key
        Dim amount As Integer = winnerKV.Value

        ' Deduct gold and award units
        winner.Gold -= amount
        If winner.Gold < 0 Then winner.Gold = 0

        AwardMercenariesToPlayer(CurrentMercOffer, winner)
        Debug.WriteLine($"Mercenaries hired by Player {winner.PlayerNumber} ({winner.Race}) for {amount} gold!")

        ' Clear current offer
        CurrentMercOffer = Nothing
    End Sub



#End Region

    Public Function CreateSummonerUnit(faction As String, level As Integer, ownerRace As String) As Unit
        Return New Unit(faction, level, ownerRace)
    End Function



End Class