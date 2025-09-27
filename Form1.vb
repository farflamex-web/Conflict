Option Strict On
Option Explicit On


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

        Public Property SpecialCharacters As New List(Of SpecialCharacter)

        ' Add this property
        Public Property Race As String
    End Class

    Public Class Unit
        Public Property Name As String
        Public Property ShortName As String
        Public Property HP As Integer
        Public Property Melee As Integer
        Public Property Ranged As Integer

        Public Property Power As Integer ' For summoned creatures only
        Public Property DefencePoints As Integer ' For summoned creatures only

        Public Property Size As Integer
        Public Property Armour As String
        Public Property Shield As String
        Public Property Type As UnitType
        Public Property HasUsedCharge As Boolean = False
        Public Property Race As String

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
    End Class

    Public Class ArmyCommand
        Public Property Command As String       ' e.g., "MOVE", "RECRUIT", "DISBAND"
        Public Property Parameter As String     ' e.g., "Ironfoot", "li", "HeavyInfantry"
    End Class

    Public Class SpecialCharacter
        ' === Basic properties ===
        Public Property Name As String
        Public Property Type As String           ' "Summoner", "Commander", etc.
        Public Property Level As Integer
        Public Property XP As Integer
        Public Property Race As String
        Public Property Army As Army             ' Army this character belongs to

        ' === The summoner's own roster of creatures ===
        Public Property Roster As List(Of UnitStats)

        ' === Embedded monster rosters for each race ===
        Public Shared ElfSummonerRoster As New List(Of UnitStats) From {
        New UnitStats With {.Name = "Woodland Sprite", .Power = 1, .HP = 5, .Melee = 1, .Ranged = 0, .DefencePoints = 0},
        New UnitStats With {.Name = "Centaur", .Power = 4, .HP = 20, .Melee = 1, .Ranged = 2, .DefencePoints = 1},
        New UnitStats With {.Name = "Forest Wolf", .Power = 9, .HP = 45, .Melee = 6, .Ranged = 0, .DefencePoints = 3},
        New UnitStats With {.Name = "Giant Eagle", .Power = 16, .HP = 80, .Melee = 10, .Ranged = 5, .DefencePoints = 0},
        New UnitStats With {.Name = "Forest Gryphon", .Power = 25, .HP = 125, .Melee = 15, .Ranged = 10, .DefencePoints = 0},
        New UnitStats With {.Name = "Shadow Panther", .Power = 36, .HP = 180, .Melee = 20, .Ranged = 0, .DefencePoints = 16},
        New UnitStats With {.Name = "Unicorn", .Power = 49, .HP = 250, .Melee = 30, .Ranged = 10, .DefencePoints = 10},
        New UnitStats With {.Name = "Ent", .Power = 64, .HP = 300, .Melee = 35, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Ancient Ent", .Power = 81, .HP = 400, .Melee = 100, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Forest Dragon", .Power = 100, .HP = 500, .Melee = 1000, .Ranged = 500, .DefencePoints = 10}
    }

        Public Shared NecromancerSummonerRoster As New List(Of UnitStats) From {
        New UnitStats With {.Name = "Zombie", .Power = 1, .HP = 5, .Melee = 1, .Ranged = 0, .DefencePoints = 0},
        New UnitStats With {.Name = "Skeleton", .Power = 4, .HP = 20, .Melee = 2, .Ranged = 0, .DefencePoints = 2},
        New UnitStats With {.Name = "Spectre", .Power = 9, .HP = 45, .Melee = 4, .Ranged = 0, .DefencePoints = 5},
        New UnitStats With {.Name = "Wraith", .Power = 16, .HP = 80, .Melee = 7, .Ranged = 0, .DefencePoints = 10},
        New UnitStats With {.Name = "Ghoul", .Power = 25, .HP = 125, .Melee = 10, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Bone Golem", .Power = 36, .HP = 180, .Melee = 10, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Vampire", .Power = 49, .HP = 250, .Melee = 20, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Lich", .Power = 64, .HP = 300, .Melee = 20, .Ranged = 10, .DefencePoints = 15},
        New UnitStats With {.Name = "Death Knight", .Power = 81, .HP = 400, .Melee = 100, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Bone Dragon", .Power = 100, .HP = 500, .Melee = 1000, .Ranged = 500, .DefencePoints = 10}
    }

        Public Shared DwarfSummonerRoster As New List(Of UnitStats) From {
        New UnitStats With {.Name = "Gnome Infantry", .Power = 1, .HP = 5, .Melee = 1, .Ranged = 0, .DefencePoints = 0},
        New UnitStats With {.Name = "Gnome Crossbowmen", .Power = 4, .HP = 20, .Melee = 1, .Ranged = 1, .DefencePoints = 2},
        New UnitStats With {.Name = "Stone Construct", .Power = 9, .HP = 45, .Melee = 1, .Ranged = 0, .DefencePoints = 10},
        New UnitStats With {.Name = "Iron Sentinel", .Power = 16, .HP = 80, .Melee = 1, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Stone Giant", .Power = 25, .HP = 125, .Melee = 10, .Ranged = 0, .DefencePoints = 13},
        New UnitStats With {.Name = "Mithril Warden", .Power = 36, .HP = 180, .Melee = 20, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Mountain Giant", .Power = 49, .HP = 250, .Melee = 35, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Diamond Golem", .Power = 64, .HP = 300, .Melee = 50, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Storm Giant", .Power = 81, .HP = 400, .Melee = 100, .Ranged = 25, .DefencePoints = 15},
        New UnitStats With {.Name = "Mountain Drake", .Power = 100, .HP = 500, .Melee = 1000, .Ranged = 500, .DefencePoints = 15}
    }

        Public Shared HumanSummonerRoster As New List(Of UnitStats) From {
        New UnitStats With {.Name = "War Dog", .Power = 1, .HP = 5, .Melee = 1, .Ranged = 0, .DefencePoints = 0},
        New UnitStats With {.Name = "Halfling Slingers", .Power = 4, .HP = 20, .Melee = 1, .Ranged = 1, .DefencePoints = 2},
        New UnitStats With {.Name = "Clay Golem", .Power = 9, .HP = 45, .Melee = 1, .Ranged = 0, .DefencePoints = 8},
        New UnitStats With {.Name = "Earth Elemental", .Power = 16, .HP = 80, .Melee = 1, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Magic Knight", .Power = 25, .HP = 125, .Melee = 13, .Ranged = 0, .DefencePoints = 12},
        New UnitStats With {.Name = "Fire Elemental", .Power = 36, .HP = 180, .Melee = 21, .Ranged = 0, .DefencePoints = 15},
        New UnitStats With {.Name = "Hippogriff", .Power = 49, .HP = 250, .Melee = 24, .Ranged = 10, .DefencePoints = 15},
        New UnitStats With {.Name = "Bronze Dragon", .Power = 100, .HP = 300, .Melee = 50, .Ranged = 100, .DefencePoints = 15},
        New UnitStats With {.Name = "Silver Dragon", .Power = 100, .HP = 400, .Melee = 100, .Ranged = 500, .DefencePoints = 15},
        New UnitStats With {.Name = "Gold Dragon", .Power = 100, .HP = 500, .Melee = 1000, .Ranged = 1000, .DefencePoints = 15}
    }

        ' === Constructor ===
        Public Sub New(race As String, type As String, Optional level As Integer = 1)
            Me.Race = race
            Me.Type = type
            Me.Level = level
            Me.XP = 0
            Me.Name = SpecialCharacter.GenerateRandomName(race)

            ' Attach correct roster
            Select Case race.ToLower()
                Case "elf", "elves"
                    Me.Roster = New List(Of UnitStats)(ElfSummonerRoster)
                Case "orc", "orcs", "necromancer"
                    Me.Roster = New List(Of UnitStats)(NecromancerSummonerRoster)
                Case "dwarf", "dwarves"
                    Me.Roster = New List(Of UnitStats)(DwarfSummonerRoster)
                Case "human", "humans"
                    Me.Roster = New List(Of UnitStats)(HumanSummonerRoster)
                Case Else
                    Me.Roster = New List(Of UnitStats)()
            End Select
        End Sub

        ' === Shared method to generate a name ===
        Public Shared Function GenerateRandomName(race As String) As String
            Return Form1.GenerateName(race)
        End Function

        ' === Level up method ===
        Public Sub LevelUp()
            Level += 1
        End Sub

        ' === Summon creatures method ===
        Public Sub SummonCreatures()
            If Army Is Nothing OrElse Roster Is Nothing Then Return

            For Each creature In Roster
                If creature.Power <= Level Then
                    Dim numToSummon As Integer = Level \ creature.Power
                    If numToSummon <= 0 Then Continue For

                    Dim existingUnit As Unit = Army.Units.FirstOrDefault(Function(u) u.Name = creature.Name)
                    If existingUnit IsNot Nothing Then
                        existingUnit.Size += numToSummon
                    Else
                        Dim newUnit As New Unit With {
                        .Name = creature.Name,
                        .HP = creature.HP,
                        .Melee = creature.Melee,
                        .Ranged = creature.Ranged,
                        .Size = numToSummon,
                        .Armour = "None",
                        .Shield = Nothing,
                        .Type = UnitType.Archer, ' you can refine per creature type later
                        .Race = Army.Race
                    }
                        Army.Units.Add(newUnit)
                    End If
                End If
            Next
        End Sub
    End Class


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
        Public Property Food As Integer
        Public Property FoodCollectedThisTurn As Integer
        Public Property Iron As Integer
        Public Property IronCollectedThisTurn As Integer
        Public Property Wood As Integer
        Public Property WoodCollectedThisTurn As Integer
        Public Property Mounts As Integer
        Public Property MountsCollectedThisTurn As Double
        Public Property AIControlled As Boolean
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
            New UnitStats With {.Name = "Ironbolters", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 1, .Armour = "Chainmail", .Shield = Nothing, .Cost = "1 iron(axe),1 iron(chainmail),1 wood(shield)", .ShortName = "a"},
            New UnitStats With {.Name = "Ironshield", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Iron", .Cost = "1 iron(axe),1 iron(chainmail),1 iron(shield)", .ShortName = "li"},
            New UnitStats With {.Name = "Stormforged Hammerguards", .Type = UnitType.HeavyInfantry, .HP = 15, .Melee = 2, .Ranged = 0, .Armour = "Stormforged Plate", .Shield = Nothing, .Cost = "1 iron(2H axe),2 iron(plate)", .ShortName = "hi"},
            New UnitStats With {.Name = "Ironhorn Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Iron", .Cost = "1 iron(axe),1 iron(chainmail),1 iron(shield),1 mount", .ShortName = "lc"},
            New UnitStats With {.Name = "Ironhorn Maulers", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(forgehammer),2 iron(plate),1 mount", .ShortName = "hc"}
        }
    }
        AllRaces.Add(dwarfUnits)

        ' ----------------- Orcs -----------------
        Dim orcUnits As New RaceUnits With {
        .RaceName = "Orc",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Orc Shortbows", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 1, .Armour = "None", .Shield = Nothing, .Cost = "1 iron(axe),1 wood(shortbow)", .ShortName = "a"},
            New UnitStats With {.Name = "War Grunts", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 2, .Ranged = 0, .Armour = "None", .Shield = "Wooden", .Cost = "1 iron(axe),1 wood(shield)", .ShortName = "li"},
            New UnitStats With {.Name = "Orc Berserkers", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Chainmail", .Shield = Nothing, .Cost = "1 iron(2H axe),1 chainmail", .ShortName = "hi"},
            New UnitStats With {.Name = "Fangriders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 2, .Ranged = 0, .Armour = "None", .Shield = "Wooden", .Cost = "1 iron(axe),1 wood(shield),1 mount", .ShortName = "lc"},
            New UnitStats With {.Name = "Bloodwolf Riders", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Chainmail", .Shield = Nothing, .Cost = "1 iron(2H axe),1 chainmail,1 mount", .ShortName = "hc"}
        }
    }
        AllRaces.Add(orcUnits)

        ' ----------------- Humans -----------------
        Dim humanUnits As New RaceUnits With {
        .RaceName = "Human",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Silver Bowmen", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 1, .Armour = "None", .Shield = Nothing, .Cost = "1 iron(sword),1 wood(bow)", .ShortName = "a"},
            New UnitStats With {.Name = "Guardian Spearmen", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Wooden", .Cost = "1 iron(spear),1 chainmail,1 wood(shield)", .ShortName = "li"},
            New UnitStats With {.Name = "Ironstorm Infantry", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(2H sword),2 iron(plate)", .ShortName = "hi"},
            New UnitStats With {.Name = "Swiftblade Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Wooden", .Cost = "1 iron(sword),1 chainmail,1 wood(shield),1 mount", .ShortName = "lc"},
            New UnitStats With {.Name = "Steel Vanguard", .Type = UnitType.HeavyCavalry, .HP = 15, .Melee = 3, .Ranged = 0, .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(lance),2 iron(plate),1 mount", .ShortName = "hc"}
        }
    }
        AllRaces.Add(humanUnits)

        ' ----------------- Elves -----------------
        Dim elfUnits As New RaceUnits With {
        .RaceName = "Elf",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Moonbow Archers", .Type = UnitType.Archer, .HP = 4, .Melee = 1, .Ranged = 2, .Armour = "None", .Shield = Nothing, .Cost = "1 iron(sword),1 wood(bow)", .ShortName = "a"},
            New UnitStats With {.Name = "Forest Sentinels", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Armour = "Chainmail", .Shield = "Wooden", .Cost = "1 iron(sword),1 chainmail,1 wood(shield)", .ShortName = "li"},
            New UnitStats With {.Name = "Moonblade Guardians", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(moonblade),2 iron(plate)", .ShortName = "hi"},
            New UnitStats With {.Name = "Moonbow Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 1, .Ranged = 2, .Armour = "None", .Shield = Nothing, .Cost = "1 iron(sword),1 wood(bow),1 mount", .ShortName = "lc"},
            New UnitStats With {.Name = "Stormleaf Chargers", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Armour = "Plate", .Shield = "Iron", .Cost = "2 iron(plate),1 iron(lance),1 mount", .ShortName = "hc"}
        }
    }
        AllRaces.Add(elfUnits)
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
            .AIControlled = True
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
                .Race = p.Race,
                .SpecialCharacters = New List(Of SpecialCharacter)()
            }

                ' --- First army: 500 LI + 500 Archers ---
                If a = 1 Then
                    ' Light Infantry
                    Dim liTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.LightInfantry)
                    Dim liUnit As New Unit With {
                    .Name = liTemplate.Name,
                    .HP = liTemplate.HP,
                    .Melee = liTemplate.Melee,
                    .Ranged = liTemplate.Ranged,
                    .Size = 500,
                    .Armour = liTemplate.Armour,
                    .Shield = liTemplate.Shield,
                    .Type = liTemplate.Type,
                    .Race = army.Race
                }
                    army.Units.Add(liUnit)

                    ' Archers
                    Dim archerTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.Archer)
                    Dim archerUnit As New Unit With {
                    .Name = archerTemplate.Name,
                    .HP = archerTemplate.HP,
                    .Melee = archerTemplate.Melee,
                    .Ranged = archerTemplate.Ranged,
                    .Size = 500,
                    .Armour = archerTemplate.Armour,
                    .Shield = archerTemplate.Shield,
                    .Type = archerTemplate.Type,
                    .Race = army.Race
                }
                    army.Units.Add(archerUnit)

                    ' --- Add one summoner to this army ---
                    Dim summoner As New SpecialCharacter(p.Race, "Summoner", 1) With {
                        .Army = army
                    }
                    army.SpecialCharacters.Add(summoner)

                    ' Add to army's special characters list
                    army.SpecialCharacters.Add(summoner)
                Else
                    ' Other armies: single Light Infantry with 100 men
                    Dim liTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.LightInfantry)
                    Dim liUnit As New Unit With {
                    .Name = liTemplate.Name,
                    .HP = liTemplate.HP,
                    .Melee = liTemplate.Melee,
                    .Ranged = liTemplate.Ranged,
                    .Size = 100,
                    .Armour = liTemplate.Armour,
                    .Shield = liTemplate.Shield,
                    .Type = liTemplate.Type,
                    .Race = army.Race
                }
                    army.Units.Add(liUnit)
                End If

                ' Add army to player
                p.Armies.Add(army)

                ' Optional: Debug log
                Dim logLine As String = $"{p.Race} Army {a} at ({army.X},{army.Y}): " &
                                    String.Join(", ", army.Units.Select(Function(u) $"{u.Name}({u.Size})")) &
                                    $" | TotalSoldiers = {army.TotalSoldiers}"
                System.Diagnostics.Debug.WriteLine(logLine)
                rtbInfo.AppendText(logLine & Environment.NewLine)

                ' Optional: log special characters
                If army.SpecialCharacters.Count > 0 Then
                    For Each sc In army.SpecialCharacters
                        System.Diagnostics.Debug.WriteLine($"Special Character: {sc.Type} {sc.Name} (Level {sc.Level}) in Army {a}")
                    Next
                End If
            Next

            Players.Add(p)
        Next
    End Sub


#End Region

#Region "=== Process Turn ==="

    Public Sub ProcessTurn()
        Dim maxSteps As Integer = 5
        Dim mapSize As Integer = Map.GetLength(0)

        ' --- Step 0: Summoners summon at start of turn ---
        For Each p In Players
            For Each a In p.Armies
                If a.SpecialCharacters IsNot Nothing Then
                    For Each sc In a.SpecialCharacters
                        If sc.Type.ToLower() = "summoner" Then
                            sc.SummonCreatures()   ' Adds creatures directly to the army
                            sc.Level += 1          ' Level up after summoning
                        End If
                    Next
                End If
            Next
        Next

        ' --- Loop through each movement step ---
        For stepIndex As Integer = 0 To maxSteps - 1
            For Each p In Players
                If p.Armies IsNot Nothing Then
                    For Each a In p.Armies
                        ' Skip armies that cannot move (less than 500 soldiers)
                        If a.TotalSoldiers < 500 Then Continue For

                        ' Skip armies that retreated this turn
                        If a.RetreatedThisTurn Then Continue For

                        ' Fill AI move queue if empty
                        If p.AIControlled AndAlso a.MoveQueue.Count = 0 Then
                            Dim plannedMoves As List(Of ArmyCommand) = GenerateAIMoves(a, p, maxSteps)
                            a.MoveQueue.AddRange(plannedMoves)
                        End If

                        ' Execute move for this step if exists
                        If stepIndex < a.MoveQueue.Count Then
                            Dim cmd As ArmyCommand = a.MoveQueue(stepIndex)
                            Dim moveCommand As String = cmd.Command.ToUpper().Trim()

                            ' Last step reserved for recruitment/special action
                            Dim isSpecial As Boolean = (stepIndex = maxSteps - 1)
                            If isSpecial Then
                                a.HasUsedSpecial = True
                                Select Case moveCommand
                                    Case "RECRUIT"
                                        Dim unitShortName As String = cmd.Parameter
                                        Dim raceUnits As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName = p.Race)
                                        If raceUnits IsNot Nothing Then
                                            Dim templateUnit As UnitStats = raceUnits.Units.FirstOrDefault(Function(u) u.ShortName = unitShortName)
                                            If templateUnit IsNot Nothing Then
                                                RecruitArmyUnits(a, p, templateUnit)
                                            End If
                                        End If
                                    Case "DISBAND"
                                    ' Placeholder
                                    Case "TRAIN"
                                    ' Placeholder
                                    Case "PATROL"
                                        ' Placeholder
                                End Select
                            Else
                                ' Regular movement
                                Dim offset As Point = DirectionToOffset(moveCommand)
                                Dim newX = Math.Max(0, Math.Min(mapSize - 1, a.X + offset.X))
                                Dim newY = Math.Max(0, Math.Min(mapSize - 1, a.Y + offset.Y))

                                ' Capture only N/E/S/W adjacent
                                If Map(newX, newY, 1) <> p.PlayerNumber AndAlso IsCaptureValidOrthogonal(newX, newY, p.PlayerNumber) Then
                                    Map(newX, newY, 1) = p.PlayerNumber
                                End If

                                ' Update army position
                                a.X = newX
                                a.Y = newY
                            End If
                        End If
                    Next
                End If
            Next

            ' Resolve combat after each move step
            ResolveCombat()
        Next

        ' Clear move queues and reset flags
        For Each p In Players
            For Each a In p.Armies
                a.MoveQueue.Clear()
                a.HasUsedSpecial = False
                a.RetreatedThisTurn = False
            Next
        Next
    End Sub


    Public Sub RecruitArmyUnits(army As Army, player As Player, unitTemplate As UnitStats)
        ' --- Determine max recruitable per turn (5% of population) ---
        Dim maxPerTurn As Integer = Math.Max(1, CInt(player.Population * 0.05))

        ' Limit by available population
        Dim recruitAmount As Integer = Math.Min(maxPerTurn, player.Population)

        ' --- Parse unit cost for resources ---
        Dim ironRequired As Integer = 0
        Dim woodRequired As Integer = 0
        Dim mountsRequired As Integer = 0

        For Each part In unitTemplate.Cost.Split(","c)
            Dim p As String = part.Trim().ToLower()
            If p.Contains("iron") Then ironRequired += 1
            If p.Contains("wood") Then woodRequired += 1
            If p.Contains("mount") OrElse p.Contains("horse") OrElse p.Contains("wolf") OrElse p.Contains("elk") OrElse p.Contains("ram") Then
                mountsRequired += 1
            End If
        Next

        ' --- Limit by available resources ---
        If ironRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Iron \ ironRequired)
        If woodRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Wood \ woodRequired)
        If mountsRequired > 0 Then recruitAmount = Math.Min(recruitAmount, player.Mounts \ mountsRequired)

        ' --- Skip if nothing can be recruited ---
        If recruitAmount <= 0 Then Exit Sub

        ' --- Check if unit already exists in army ---
        Dim existingUnit As Unit = army.Units.FirstOrDefault(Function(u) u.Name = unitTemplate.Name AndAlso u.Type = unitTemplate.Type)

        If existingUnit IsNot Nothing Then
            ' Add to existing unit's size
            existingUnit.Size += recruitAmount
        Else
            ' Create new unit only if it doesn't exist
            Dim newUnit As New Unit With {
            .Name = unitTemplate.Name,
            .HP = unitTemplate.HP,
            .Melee = unitTemplate.Melee,
            .Ranged = unitTemplate.Ranged,
            .Size = recruitAmount,
            .Armour = unitTemplate.Armour,
            .Shield = unitTemplate.Shield,
            .Type = unitTemplate.Type,
            .Race = army.Race
        }
            army.Units.Add(newUnit)
        End If

        ' --- Deduct population and resources ---
        player.Population -= recruitAmount
        player.Iron -= ironRequired * recruitAmount
        player.Wood -= woodRequired * recruitAmount
        player.Mounts -= mountsRequired * recruitAmount

        ' --- Log recruitment ---
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
                                mergedArmy.Units.Add(New Unit With {
                                .Name = u.Name,
                                .Type = u.Type,
                                .HP = u.HP,
                                .Melee = u.Melee,
                                .Ranged = u.Ranged,
                                .Size = u.Size,
                                .Armour = u.Armour,
                                .Shield = u.Shield,
                                .Race = u.Race
                            })
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
                        Dim sendHome As Boolean = False
                        If army.TotalSoldiers < maxStrength Then
                            sendHome = True
                        ElseIf army.TotalSoldiers = maxStrength AndAlso strongestCount > 1 Then
                            sendHome = True
                        End If

                        If sendHome Then
                            SendArmyBackToSpawn(army)
                            ClearArmyMoveQueue(army)
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
        For dx As Integer = -2 To 2
            For dy As Integer = -2 To 2
                points.Add(New Point(a.X + dx, a.Y + dy))
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

                        ' --- Base resources per square ---
                        Dim foodPerSquare As Integer = 1
                        Dim ironPerSquare As Integer = 1
                        Dim woodPerSquare As Integer = 1
                        Dim mountPerSquare As Double = 0

                        ' --- Apply racial bonus ---
                        If terrain = preferredTerrain Then
                            foodPerSquare += 1
                            mountPerSquare = 0.5
                        End If

                        ' --- Add resources based on population per square ---
                        p.FoodCollectedThisTurn += foodPerSquare * popPerSquare
                        p.IronCollectedThisTurn += ironPerSquare * popPerSquare
                        p.WoodCollectedThisTurn += woodPerSquare * popPerSquare
                        p.MountsCollectedThisTurn += mountPerSquare * popPerSquare
                    End If
                Next
            Next

            ' --- Apply territory bonus (2% per owned tile) ---
            Dim territoryBonus As Double = ownedSquares * 0.005
            p.FoodCollectedThisTurn = CInt(p.FoodCollectedThisTurn * (1 + territoryBonus))
            p.IronCollectedThisTurn = CInt(p.IronCollectedThisTurn * (1 + territoryBonus))
            p.WoodCollectedThisTurn = CInt(p.WoodCollectedThisTurn * (1 + territoryBonus))
            p.MountsCollectedThisTurn = p.MountsCollectedThisTurn * (1 + territoryBonus)

            ' --- Add to cumulative totals ---
            p.Iron += p.IronCollectedThisTurn
            p.Wood += p.WoodCollectedThisTurn
            p.Mounts += CInt(Math.Floor(p.MountsCollectedThisTurn))   ' convert to integer
            p.Food = p.FoodCollectedThisTurn
        Next
    End Sub


    Public Sub GrowPopulationAndFeedEverybody()
        ' Loop through all players
        For Each p In Players
            ' Calculate total food required for armies
            Dim armyFoodRequirement As Integer = 0
            If p.Armies IsNot Nothing Then
                For Each a In p.Armies
                    armyFoodRequirement += a.TotalSoldiers ' 1-to-1 food per soldier
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


    ' Private helper to fill a 5x5 block with exact terrain distribution for a player
    Private Sub FillStartingBlock(startX As Integer, startY As Integer, playerID As Integer, favTerrain As Integer)
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
        ProcessTurn()

        ' --- 4. Refresh map display ---
        pnlMap.Invalidate()

        ' --- 5. Update empire/resource info in RichTextBox ---
        UpdateResourceInfo()
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
            rtbResourceInfo.AppendText($"Mounts Collected: {p.MountsCollectedThisTurn:F1}" & Environment.NewLine)
            rtbResourceInfo.AppendText($"Total Mounts: {p.Mounts}" & Environment.NewLine)

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

        ' --- Create battle log ---
        Dim battleLog As New BattleLog(activeArmies)

        ' --- Combat phases ---
        For Each phaseName In New String() {"Ranged", "Charge", "Melee", "Chase"}
            ' Snapshot of units for proportional damage
            Dim unitSnapshot As New Dictionary(Of Unit, Integer)
            For Each army In activeArmies
                For Each u In army.Units
                    unitSnapshot(u) = u.Size
                Next
            Next

            ' Apply proportional damage for each defending army
            For Each defArmy In activeArmies
                Dim attackers As List(Of Unit) = activeArmies.Where(Function(a) a IsNot defArmy) _
                                                    .SelectMany(Function(a) a.Units).ToList()
                ApplyProportionalDamage(defArmy, attackers, phaseName, unitSnapshot, battleLog)
            Next
        Next

        ' --- Return only the BattleLog ---
        Return battleLog
    End Function

    Public Sub SendArmyBackToSpawn(army As Army)
        ''' <summary>
        ''' Moves the army back to its spawn location after being defeated.
        ''' Uses the army's Race property to determine starting coordinates:
        ''' - Elf: top-left
        ''' - Dwarf: top-right
        ''' - Orc: bottom-left
        ''' - Human: bottom-right
        ''' </summary>

        Select Case army.Race.ToLower()
            Case "elf"
                army.X = 0
                army.Y = 0
            Case "dwarf"
                army.X = 24
                army.Y = 0
            Case "orc"
                army.X = 0
                army.Y = 24
            Case "human"
                army.X = 24
                army.Y = 24
        End Select
    End Sub


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



    Public Sub ApplyProportionalDamage(defArmy As Army, attackingUnits As List(Of Unit),
                                   phase As String, unitSnapshot As Dictionary(Of Unit, Integer),
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
                        If atkUnit.Race.ToLower() = "elf" AndAlso atkUnit.Type = UnitType.Archer Then
                            atkValue += 1
                            atkExplanation &= " +1 elven bonus"
                        End If
                    Else
                        Continue For
                    End If
                Case "charge"
                    atkValue = atkUnit.Melee + GetChargeBonus(atkUnit)
                    atkExplanation = If(GetChargeBonus(atkUnit) = 0,
                                $"{atkUnit.Melee} melee (no charge bonus)",
                                $"{atkUnit.Melee} melee + {GetChargeBonus(atkUnit)} charge bonus")
                Case "melee"
                    atkValue = atkUnit.Melee
                    atkExplanation = $"{atkUnit.Melee} melee"
                Case "chase"
                    If atkUnit.Type = UnitType.LightCavalry Then
                        atkValue = atkUnit.Melee * 2
                        atkExplanation = $"{atkUnit.Melee} melee x2 for chase"
                    Else
                        Continue For
                    End If
            End Select

            ' --- Total HP-weighted size of defenders ---
            Dim totalDefWeight As Double = defArmy.Units.Sum(Function(u) unitSnapshot(u) * u.HP)
            If totalDefWeight = 0 Then Continue For

            ' --- Apply proportional damage to each defender unit ---
            For Each defUnit In defArmy.Units
                Dim sizeBefore As Integer = unitSnapshot(defUnit) ' snapshot at phase start
                Dim unitWeight As Double = sizeBefore * defUnit.HP

                ' Raw damage proportional to weight
                Dim rawDamage As Double = atkValue * atkSize * (unitWeight / totalDefWeight)

                ' Mitigation fraction + explanation
                Dim mitResult = GetUnitMitigation(defUnit)
                Dim mitigationValue As Double = mitResult.Mitigation
                Dim mitigationExplanation As String = mitResult.Explanation

                ' Final damage after mitigation
                Dim finalDamage As Double = rawDamage * (1 - mitigationValue)

                ' Calculate casualties (cannot exceed unit size)
                Dim casualties As Integer = Math.Min(sizeBefore, CInt(Math.Floor(finalDamage / defUnit.HP)))

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


    Public Function GetChargeBonus(unit As Unit) As Integer
        ''' <summary>
        ''' Returns the charge-phase bonus for a given unit.
        ''' 
        ''' Phase behavior:
        ''' - Dwarf Stormforged Hammerguards (Heavy Infantry): +1 attack in charge phase.
        ''' - Human Steel Vanguard (Heavy Cavalry): +2 attack in charge phase (armoured charge).
        ''' - All other units: 0.
        ''' </summary>

        Select Case unit.Name.ToLower()
            Case "stormforged hammerguards"
                Return 1
            Case "steel vanguard"
                Return 2
            Case Else
                Return 0
        End Select
    End Function

    Public Function GetUnitMitigation(unit As Unit) As (Mitigation As Double, Explanation As String)
        Dim mitigation As Double = 0
        Dim parts As New List(Of String)

        ' Armour mitigation
        Select Case unit.Armour?.ToLower()
            Case "chainmail"
                mitigation += 0.25
                parts.Add("25% chainmail")
            Case "plate"
                mitigation += 0.5
                parts.Add("50% plate")
        End Select

        ' Shield mitigation
        Select Case unit.Shield?.ToLower()
            Case "wooden"
                mitigation += 0.1
                parts.Add("10% wooden shield")
            Case "iron"
                mitigation += 0.2
                parts.Add("20% iron shield")
        End Select

        ' Race/unit-specific bonuses (legacy)
        If unit.Name.ToLower() = "stormforged hammerguards" Then
            mitigation += 0.1
            parts.Add("10% stormforged bonus")
        ElseIf unit.Name.ToLower() = "war grunts" Then
            mitigation += 0.2
            parts.Add("20% war grunt bonus")
        End If

        ' --- NEW: DefencePoints mitigation ---
        ' Each DefencePoint = 5% mitigation
        If TypeOf unit Is Unit Then
            Dim defPoints As Integer = 0
            If unit.GetType().GetProperty("DefencePoints") IsNot Nothing Then
                defPoints = CInt(unit.GetType().GetProperty("DefencePoints").GetValue(unit))
            End If
            If defPoints > 0 Then
                Dim defMit As Double = defPoints * 0.05
                mitigation += defMit
                parts.Add($"{defPoints * 5}% defence points")
            End If
        End If

        ' Cap mitigation at 80%
        If mitigation > 0.8 Then mitigation = 0.8

        Dim explanation As String = If(parts.Count > 0, String.Join(" + ", parts), "No mitigation")
        Return (mitigation, explanation)
    End Function


    Public Sub AIRecruitArmy(army As Army, player As Player, armyIndex As Integer)
        Dim role As String
        Select Case armyIndex
            Case 0 : role = "Offensive"
            Case 1 : role = "Flexible"
            Case 2 : role = "Defensive"
            Case Else : role = "Flexible"
        End Select

        Dim nearbyEnemies = Players.SelectMany(Function(p) p.Armies) _
        .Where(Function(a) a.Race <> player.Race AndAlso Math.Abs(a.X - army.X) <= 5 AndAlso Math.Abs(a.Y - army.Y) <= 5).ToList()

        Dim weakNearbyEnemies = nearbyEnemies.Where(Function(ea) ea.TotalSoldiers < army.TotalSoldiers).ToList()
        Dim strongNearbyEnemies = nearbyEnemies.Where(Function(ea) ea.TotalSoldiers >= army.TotalSoldiers).ToList()
        Dim underThreat As Boolean = strongNearbyEnemies.Count > 0
        Dim hasOpportunity As Boolean = weakNearbyEnemies.Count > 0

        Dim raceUnits As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName = player.Race)
        If raceUnits Is Nothing Then Exit Sub

        Dim chosenUnit As UnitStats = Nothing
        Select Case role
            Case "Offensive"
                If hasOpportunity Then
                    chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.HeavyInfantry OrElse u.Type = UnitType.HeavyCavalry)
                Else
                    chosenUnit = raceUnits.Units.FirstOrDefault()
                End If
            Case "Flexible"
                If underThreat Then
                    chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.LightInfantry OrElse u.Type = UnitType.Archer)
                ElseIf hasOpportunity Then
                    chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.HeavyInfantry OrElse u.Type = UnitType.HeavyCavalry)
                Else
                    chosenUnit = raceUnits.Units(rnd.Next(raceUnits.Units.Count))
                End If
            Case "Defensive"
                chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.LightInfantry OrElse u.Type = UnitType.Archer)
        End Select

        If chosenUnit IsNot Nothing Then
            Dim maxPerTurn As Integer = Math.Max(1, CInt(player.Population * 0.05))

            Dim ironRequired As Integer = 0, woodRequired As Integer = 0, mountsRequired As Integer = 0
            For Each part In chosenUnit.Cost.Split(","c)
                Dim pStr = part.Trim().ToLower()
                If pStr.Contains("iron") Then ironRequired += 1
                If pStr.Contains("wood") Then woodRequired += 1
                If pStr.Contains("mount") OrElse pStr.Contains("horse") OrElse pStr.Contains("wolf") OrElse pStr.Contains("elk") OrElse pStr.Contains("ram") Then mountsRequired += 1
            Next

            Dim maxByResources As Integer = Math.Min(maxPerTurn,
                                        Math.Min(
                                            If(ironRequired > 0, player.Iron \ ironRequired, maxPerTurn),
                                            Math.Min(
                                                If(woodRequired > 0, player.Wood \ woodRequired, maxPerTurn),
                                                If(mountsRequired > 0, player.Mounts \ mountsRequired, maxPerTurn)
                                            )
                                        )
                                    )

            If maxByResources > 0 AndAlso player.Population > 0 Then
                Dim cmd As New ArmyCommand With {
                .Command = "RECRUIT",
                .Parameter = chosenUnit.ShortName
            }
                army.MoveQueue.Add(cmd)
            End If
        End If
    End Sub



    Private Function GenerateBattleSummary(armies As List(Of Army),
                                       Optional title As String = "Battle Summary",
                                       Optional battleLog As BattleLog = Nothing) As String
        Dim sb As New Text.StringBuilder()

        ' --- Header ---
        sb.AppendLine($"=== {title} ===")

        ' List of races involved
        Dim racesInvolved As String = String.Join(" vs ", armies.Select(Function(a) a.Race))

        ' Average grid location
        Dim avgX As Integer = CInt(armies.Average(Function(a) a.X))
        Dim avgY As Integer = CInt(armies.Average(Function(a) a.Y))

        sb.AppendLine($"Battle at Grid ({avgX},{avgY}) between: {racesInvolved}")
        sb.AppendLine(New String("-"c, 80))

        ' --- Units line ---
        Dim unitsTitle As String = If(title.ToLower().Contains("start"), "Units at Start of Battle", "Units at End of Battle")
        sb.AppendLine(unitsTitle & ":")

        Dim maxUnits As Integer = armies.Max(Function(a) a.Units.Count)
        For unitIndex As Integer = 0 To maxUnits - 1
            Dim line As New Text.StringBuilder()
            For Each army In armies
                If unitIndex < army.Units.Count Then
                    Dim u = army.Units(unitIndex)
                    line.Append(u.Name & $" ({u.Size})".PadRight(25))
                Else
                    line.Append("".PadRight(25))
                End If
            Next
            sb.AppendLine(line.ToString())
        Next

        sb.AppendLine(New String("-"c, 80))

        ' --- Determine result based on armies sent home (only for end-of-battle) ---
        If battleLog IsNot Nothing AndAlso Not title.ToLower().Contains("start") Then
            Dim survivingArmies = armies.Except(battleLog.Retreats).ToList()
            Dim resultText As String = ""

            If survivingArmies.Count = 1 AndAlso battleLog.Retreats.Count >= 1 Then
                resultText = $"{survivingArmies.First().Race} Army Victory"
            ElseIf survivingArmies.Count = 0 AndAlso battleLog.Retreats.Count >= 1 Then
                resultText = "Draw (All armies forced to retreat)"
            End If

            sb.AppendLine()
            sb.AppendLine("Result: " & resultText)
            sb.AppendLine()
        End If

        Return sb.ToString()
    End Function

    Public Function GenerateBattleReportGrouped(battleLog As BattleLog, battleArmies As List(Of Army)) As String
        Dim report As New Text.StringBuilder()
        report.AppendLine("=== Detailed Battle Report ===")
        report.AppendLine()

        Dim phases As String() = {"Ranged", "Charge", "Melee", "Chase"}

        For phaseIndex As Integer = 0 To phases.Length - 1
            Dim phase As String = phases(phaseIndex)

            ' --- Phase-end marker for previous phase ---
            If phaseIndex > 0 Then
                Dim prevPhase As String = phases(phaseIndex - 1)
                report.AppendLine($"--- {prevPhase} Phase End ---")
                report.AppendLine()
            End If

            report.AppendLine($"--- Phase: {phase} ---")

            ' --- Detailed entries for this phase ---
            If battleLog.PhaseEntries.ContainsKey(phase) AndAlso battleLog.PhaseEntries(phase).Count > 0 Then
                ' Group entries by defending unit
                Dim groupedByDefender = battleLog.PhaseEntries(phase).GroupBy(Function(e) e.Defender)

                For Each group In groupedByDefender
                    Dim defUnit = group.Key
                    Dim defUnitName As String = $"{defUnit.Name} : {defUnit.Type}"
                    Dim phaseStartSize As Integer = group.First().SizeBefore

                    ' --- Defender line ---
                    report.AppendLine($"Defender: {defUnitName} (Size at start of phase: {phaseStartSize})")

                    ' --- Attacker details ---
                    For Each entry In group
                        Dim atkUnit = entry.Attacker
                        Dim atkUnitName As String = $"{atkUnit.Name} : {atkUnit.Type}"
                        Dim atkStartSize As Integer = entry.AttackerSizeAtPhaseStart  ' Correct attacker size

                        Dim rawDamageText As String = $"{Math.Round(entry.RawDamage, 2)}"
                        If Not String.IsNullOrEmpty(entry.RawDamageExplanation) Then
                            rawDamageText &= $" ({entry.RawDamageExplanation})"
                        End If

                        Dim mitigationPct As Integer
                        If entry.RawDamage <> 0 Then
                            mitigationPct = CInt((1 - (entry.FinalDamage / entry.RawDamage)) * 100)
                        Else
                            mitigationPct = 0
                        End If

                        Dim casualtiesText As String = $"{entry.Casualties} ({defUnit.HP} HP each so {Math.Round(entry.FinalDamage, 0)} / {defUnit.HP} = {entry.Casualties})"

                        report.AppendLine($"Attacker: {atkUnitName} (Size at start of phase: {atkStartSize})")
                        report.AppendLine($"    Raw Damage: {rawDamageText}")
                        report.AppendLine($"    {defUnit.Name} Mitigation: {mitigationPct}% ({entry.MitigationExplanation})")
                        report.AppendLine($"    Final Damage: {Math.Round(entry.FinalDamage, 2)}")
                        report.AppendLine($"    Casualties To {defUnit.Name}: {casualtiesText}")
                        report.AppendLine($"    Size After Attack: {entry.SizeAfter}")
                        report.AppendLine() ' blank line after attacker
                    Next

                    Dim totalDamagePhase As Double = group.Sum(Function(e) e.FinalDamage)
                    Dim totalCasualtiesPhase As Integer = group.Sum(Function(e) e.Casualties)

                    report.AppendLine($"  >>> Total Damage Taken by {defUnit.Name}: {Math.Round(totalDamagePhase, 2)} | Total Casualties: {totalCasualtiesPhase}")
                    report.AppendLine() ' extra blank line before next defender
                Next
            Else
                report.AppendLine("No actions in this phase.")
            End If

            report.AppendLine()

            ' --- Units at End of Phase (snapshot from PhaseEntries) ---
            report.AppendLine("Units at End of Phase:")
            Dim unitLines As New List(Of String)
            Dim maxUnits As Integer = battleArmies.Max(Function(a) a.Units.Count)

            ' Build a snapshot dictionary: Army -> List of unit sizes at end of this phase
            Dim phaseSnapshot As New Dictionary(Of Army, List(Of Integer))
            For Each army In battleArmies
                phaseSnapshot(army) = army.Units.Select(Function(u)
                                                            ' Look up all entries where this unit was a defender in this phase
                                                            Dim entries = battleLog.PhaseEntries(phase).Where(Function(e) e.Defender Is u).ToList()
                                                            If entries.Count > 0 Then
                                                                Return entries.Last().SizeAfter ' Take final size for this phase
                                                            Else
                                                                Return u.Size ' fallback (if unit not involved this phase)
                                                            End If
                                                        End Function).ToList()
            Next

            ' Build aligned lines for each unit index
            For i As Integer = 0 To maxUnits - 1
                Dim line As New System.Text.StringBuilder()
                For Each army In battleArmies
                    If i < army.Units.Count Then
                        Dim sizeAtPhaseEnd As Integer = phaseSnapshot(army)(i)
                        Dim u = army.Units(i)
                        line.Append($"{u.Name} ({sizeAtPhaseEnd})".PadRight(30))
                    Else
                        line.Append("".PadRight(30))
                    End If
                Next
                unitLines.Add(line.ToString())
            Next

            For Each l In unitLines
                report.AppendLine(l)
            Next
            report.AppendLine(New String("-"c, 80)) ' separator
        Next

        ' --- Retreats ---
        If battleLog.Retreats.Count > 0 Then
            report.AppendLine("--- Retreats ---")
            For Each army In battleLog.Retreats
                report.AppendLine($"Army of {army.Race} sent back to home at ({army.X},{army.Y}). Remaining soldiers: {army.TotalSoldiers}")
            Next
            report.AppendLine()
        End If

        ' --- End-of-battle summary at very bottom ---
        report.AppendLine(GenerateBattleSummary(battleArmies, "End of Battle", battleLog))

        report.AppendLine("=== End of Detailed Battle Report ===")
        Return report.ToString()
    End Function



    Private Function MaxRecruitableUnits(player As Player, unit As UnitStats) As Integer
        ' --- Step 1: Desired recruitment based on 5% of population, minimum 1 ---
        Dim desiredRecruit As Integer = Math.Max(1, CInt(player.Population * 0.05))

        ' --- Step 2: Parse resource requirements ---
        Dim requiredIron As Integer = 0
        Dim requiredWood As Integer = 0
        Dim requiredMounts As Integer = 0

        Dim parts() As String = unit.Cost.Split(","c)
        For Each part In parts
            Dim p As String = part.Trim().ToLower()
            If p.Contains("iron") Then requiredIron += 1
            If p.Contains("wood") Then requiredWood += 1
            If p.Contains("mount") OrElse p.Contains("horse") OrElse p.Contains("wolf") OrElse p.Contains("elk") OrElse p.Contains("ram") Then
                requiredMounts += 1
            End If
        Next

        ' --- Step 3: Max units per resource ---
        Dim maxByPop As Integer = desiredRecruit
        Dim maxByIron As Integer = If(requiredIron > 0, player.Iron \ requiredIron, maxByPop)
        Dim maxByWood As Integer = If(requiredWood > 0, player.Wood \ requiredWood, maxByPop)
        Dim maxByMounts As Integer = If(requiredMounts > 0, player.Mounts \ requiredMounts, maxByPop)

        ' --- Step 4: Actual maximum recruitable units ---
        Dim actualRecruit As Integer = Math.Min(maxByPop, Math.Min(maxByIron, Math.Min(maxByWood, maxByMounts)))

        ' --- Step 5: Starvation / growth check ---
        ' Approximate remaining food after recruiting (simplified)
        Dim armyFoodRequirement As Integer = 0
        For Each a In player.Armies
            armyFoodRequirement += a.TotalSoldiers
        Next

        Dim remainingFoodAfterRecruit As Integer = player.FoodCollectedThisTurn - (armyFoodRequirement + actualRecruit)
        ' Each 20 surplus food generates 1 growth
        Dim estimatedGrowth As Integer = If(remainingFoodAfterRecruit > 0, remainingFoodAfterRecruit \ 20, 0)

        ' Limit recruitment if it would eliminate growth
        If estimatedGrowth <= 0 Then
            ' Optionally reduce actualRecruit to leave at least 1 growth
            actualRecruit = Math.Max(0, actualRecruit - Math.Abs(estimatedGrowth))
        End If

        ' Ensure at least 0
        If actualRecruit < 0 Then actualRecruit = 0

        Return actualRecruit
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

    Public Sub PrintCompressedBattleReport(battleArmies As List(Of Army), Optional battleLog As BattleLog = Nothing)
        If battleArmies Is Nothing OrElse battleArmies.Count = 0 Then Return

        rtbInfo.Clear()

        Dim sb As New Text.StringBuilder()
        sb.AppendLine("=== Compressed Battle Report ===")

        ' List involved races
        Dim racesInvolved As String = String.Join(" vs ", battleArmies.Select(Function(a) a.Race))
        Dim avgX As Integer = CInt(battleArmies.Average(Function(a) a.X))
        Dim avgY As Integer = CInt(battleArmies.Average(Function(a) a.Y))
        sb.AppendLine($"Battle at Grid ({avgX},{avgY}) between: {racesInvolved}")
        sb.AppendLine(New String("-"c, 60))

        ' Max number of units in any army
        Dim maxUnits As Integer = battleArmies.Max(Function(a) a.Units.Count)

        ' Header: Unit columns per army
        For i As Integer = 0 To maxUnits - 1
            Dim line As New Text.StringBuilder()
            For Each army In battleArmies
                If i < army.Units.Count Then
                    Dim u As Unit = army.Units(i)
                    line.Append($"{u.Name} ({u.Size})".PadRight(25))
                Else
                    line.Append("".PadRight(25))
                End If
            Next
            sb.AppendLine(line.ToString())
        Next
        sb.AppendLine(New String("-"c, 60))

        ' Battle result if battleLog provided
        If battleLog IsNot Nothing Then
            Dim retreatRaces As New HashSet(Of String)(
                battleLog.Retreats.Select(Function(a) a.Race),
                StringComparer.OrdinalIgnoreCase
    )
            Dim survivingArmies = battleArmies.Where(Function(a) Not retreatRaces.Contains(a.Race)).ToList()

            Dim resultText As String = ""
            If survivingArmies.Count = 1 AndAlso retreatRaces.Count >= 1 Then
                resultText = $"{survivingArmies.First().Race} Army Victory"
            ElseIf survivingArmies.Count = 0 AndAlso retreatRaces.Count >= 1 Then
                resultText = "Draw (All armies forced to retreat)"
            ElseIf survivingArmies.Count > 1 Then
                resultText = "Battle unresolved (multiple survivors)"
            End If
            sb.AppendLine($"Result: {resultText}")
        End If


        sb.AppendLine()
        ' Output to console and RichTextBox
        Console.WriteLine(sb.ToString())
        rtbInfo.AppendText(sb.ToString())
    End Sub

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
            unitOrderByArmy(liveA) = New List(Of Unit)(snapA.Units.Select(Function(u)
                                                                              ' create a lightweight shell for naming only (we just need Name/Type to label lines)
                                                                              Return New Unit With {.Name = u.Name, .Type = u.Type}
                                                                          End Function))
            startSizesByArmy(liveA) = New List(Of Integer)(snapA.Units.Select(Function(u) u.Size))
            ' index lookup by key
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
            sb.AppendLine($"After {phase} Phase:")

            ' subtract casualties from current sizes using battleLog entries for THIS phase
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

            ' emit lines using snapshot order and the working sizes (never reading live armies)
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

            sb.AppendLine("Summary: [Phase summary placeholder]")
            sb.AppendLine(New String("-"c, 60))
        Next

        ' ---------- Retreats (print each once). Keep your existing RecordRetreat calls in ResolveCombat only. ----------
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
        Dim retreatRaces As New HashSet(Of String)(
    battleLog.Retreats.Select(Function(a) a.Race),
    StringComparer.OrdinalIgnoreCase
)

        Dim survivors = mergedArmies.Where(Function(ma) Not retreatRaces.Contains(ma.Race)).ToList()

        Dim resultText As String = ""
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
                na.Units.Add(New Unit With {
                .Name = u.Name,
                .ShortName = u.ShortName,
                .HP = u.HP,
                .Melee = u.Melee,
                .Ranged = u.Ranged,
                .Power = u.Power,
                .DefencePoints = u.DefencePoints,
                .Size = u.Size,
                .Armour = u.Armour,
                .Shield = u.Shield,
                .Type = u.Type,
                .Race = u.Race
            })
            Next
            clone.Add(na)
        Next
        Return clone
    End Function

    Public Function GenerateName(race As String) As String
        Dim rnd As New Random()

        Select Case race.ToLower()
            Case "dwarf", "dwarves"
                Dim dwarfPrefixes As String() = {"Br", "Th", "Dur", "Tor", "Kr", "Mor", "Gund", "Rur", "Bal", "Brom",
                                             "Thal", "Grum", "Khar", "Dorn", "Varr", "Brod", "Thrak", "Drak", "Grim", "Ston"}
                Dim dwarfSuffixes As String() = {"in", "ek", "ar", "orn", "un", "ir", "ok", "ak",
                                             "or", "im", "ur", "an", "eg", "al", "on", "irn",
                                             "um", "og", "urk", "irk"}
                Return dwarfPrefixes(rnd.Next(dwarfPrefixes.Length)) & dwarfSuffixes(rnd.Next(dwarfSuffixes.Length))

            Case "orc", "orcs"
                Dim orcPrefixes As String() = {"Gr", "Kr", "Zor", "Thok", "Vrog", "Brak", "Drog", "Mork", "Urg", "Rag",
                                           "Skul", "Gash", "Karg", "Thruk", "Vor", "Zrag", "Drok", "Gorg", "Mak", "Shak"}
                Dim orcSuffixes As String() = {"ug", "ak", "ok", "uk", "og", "ish", "ash", "uth", "ork", "agh",
                                           "rak", "mok", "zug", "nar", "ar", "osh", "ekh", "urk", "ik", "oth"}
                Return orcPrefixes(rnd.Next(orcPrefixes.Length)) & orcSuffixes(rnd.Next(orcSuffixes.Length))

            Case "human", "humans"
                Dim humanPrefixes As String() = {"Al", "Da", "Ro", "Mar", "Gal", "Jon", "Ed", "Thom", "Wil", "Ger",
                                             "Hen", "Bar", "Luc", "Ric", "Cor", "Ste", "Pet", "Arn", "Rob", "Leo"}
                Dim humanMiddles As String() = {"ri", "lo", "va", "de", "mi", "theo", "li", "ra", "ve", "na",
                                            "do", "ge", "bri", "ta", "la", "vin", "ma", "theo", "se", "ari"}
                Dim humanSuffixes As String() = {"an", "ius", "or", "el", "ard", "us", "ien", "al", "ot", "ir",
                                             "as", "en", "ur", "ion", "am", "oris", "ali", "eron", "eval", "iel"}
                Return humanPrefixes(rnd.Next(humanPrefixes.Length)) &
                   humanMiddles(rnd.Next(humanMiddles.Length)) &
                   humanSuffixes(rnd.Next(humanSuffixes.Length))

            Case "elf", "elves"
                Dim elfPrefixes As String() = {"Ae", "Ela", "Syl", "Fael", "Tha", "Cael", "Luth", "Mir", "Al", "Gal",
                                           "Eri", "Itha", "Lor", "Vael", "Olo", "Thal", "Ara", "Sel", "Nae", "Ori"}
                Dim elfMiddles As String() = {"riel", "lae", "syl", "tha", "mir", "ven", "lia", "nor", "thael", "dor",
                                          "vyn", "ri", "nae", "lor", "var", "ion", "sel", "mar", "el", "dir"}
                Dim elfSuffixes As String() = {"ion", "iel", "enor", "ilas", "evar", "anor", "ael", "elor", "ethas", "irion",
                                           "aris", "endil", "aniel", "oris", "uin", "ariel", "amar", "ethiel", "iros", "aen"}
                Return elfPrefixes(rnd.Next(elfPrefixes.Length)) &
                   elfMiddles(rnd.Next(elfMiddles.Length)) &
                   elfSuffixes(rnd.Next(elfSuffixes.Length))

            Case Else
                Return "Unnamed " & race
        End Select
    End Function


End Class