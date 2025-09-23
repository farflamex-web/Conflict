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

        ' Add this property
        Public Property Race As String
    End Class

    Public Class Unit
        Public Property Name As String
        Public Property HP As Integer
        Public Property Melee As Integer
        Public Property Ranged As Integer
        Public Property Special As String
        Public Property HasUsedCharge As Boolean = False
        Public Property Size As Integer
        Public Property Armour As String
        Public Property Shield As String
        Public Property Type As UnitType
    End Class


    Public Class UnitStats
        Public Property Name As String
        Public Property Race As String
        Public Property Type As UnitType
        Public Property HP As Integer
        Public Property Melee As Integer
        Public Property Ranged As Integer
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

        ' Record a single unit's action and casualties
        Public Sub Record(attacker As Unit, defender As Unit, damage As Double, phase As String)
            If Not PhaseEntries.ContainsKey(phase) Then PhaseEntries(phase) = New List(Of BattleEntry)
            PhaseEntries(phase).Add(New BattleEntry With {
                                   .Attacker = attacker,
                                   .Defender = defender,
                                   .Damage = damage,
                                   .Phase = phase
                               })
        End Sub

        ' Record army retreat
        Public Sub RecordRetreat(army As Army)
            Retreats.Add(army)
        End Sub
    End Class


    ' --- Updated BattleEntry ---
    Public Class BattleEntry
        Public Property Attacker As Unit
        Public Property Defender As Unit
        Public Property Damage As Double
        Public Property Phase As String
        Public Property SizeBefore As Integer   ' Snapshot at start of phase
        Public Property SizeAfter As Integer    ' Updated after damage applied
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
            New UnitStats With {.Name = "Ironbolters", .Type = UnitType.Archer, .HP = 5, .Melee = 1, .Ranged = 1, .Special = "Round 1", .Armour = "Chainmail", .Shield = Nothing, .Cost = "1 iron(axe),1 iron(chainmail),1 wood(shield)", .ShortName = "a"},
            New UnitStats With {.Name = "Ironshield", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Special = "Shielded melee infantry", .Armour = "Chainmail", .Shield = "Iron", .Cost = "1 iron(axe),1 iron(chainmail),1 iron(shield)", .ShortName = "li"},
            New UnitStats With {.Name = "Ironfoot", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Special = "+1 first round (charge)", .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(2H axe),2 iron(plate)", .ShortName = "hi"},
            New UnitStats With {.Name = "Ironhorn Riders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 1, .Ranged = 0, .Special = "+1 when chasing", .Armour = "Chainmail", .Shield = "Iron", .Cost = "1 iron(axe),1 iron(chainmail),1 iron(shield)", .ShortName = "lc"},
            New UnitStats With {.Name = "Ironhorn Maulers", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Special = "+1 charge first round", .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(forgehammer),2 iron(plate)", .ShortName = "hc"}
        }
    }
        AllRaces.Add(dwarfUnits)

        ' ----------------- Orcs -----------------
        Dim orcUnits As New RaceUnits With {
        .RaceName = "Orc",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Orc Shortbows", .Type = UnitType.Archer, .HP = 5, .Melee = 1, .Ranged = 1, .Special = "Round 1", .Armour = "None", .Shield = Nothing, .Cost = "1 iron(axe),1 wood(shortbow)", .ShortName = "a"},
            New UnitStats With {.Name = "Orc Grunts", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 2, .Ranged = 0, .Special = "Aggressive light melee", .Armour = "None", .Shield = "Wooden", .Cost = "1 iron(axe),1 wood(shield)", .ShortName = "li"},
            New UnitStats With {.Name = "Orc Berserkers", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 3, .Ranged = 0, .Special = "+1 first round (charge)", .Armour = "Chainmail", .Shield = Nothing, .Cost = "1 iron(2H axe),1 chainmail", .ShortName = "hi"},
            New UnitStats With {.Name = "Fangriders", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 2, .Ranged = 0, .Special = "+1 when chasing", .Armour = "None", .Shield = "Wooden", .Cost = "1 iron(axe),1 wood(shield),1 wolf", .ShortName = "lc"},
            New UnitStats With {.Name = "Bloodwolf Riders", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 3, .Ranged = 0, .Special = "+1 charge first round", .Armour = "Chainmail", .Shield = Nothing, .Cost = "1 iron(2H axe),1 chainmail,1 wolf", .ShortName = "hc"}
        }
    }
        AllRaces.Add(orcUnits)

        ' ----------------- Humans -----------------
        Dim humanUnits As New RaceUnits With {
        .RaceName = "Human",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Archer", .Type = UnitType.Archer, .HP = 5, .Melee = 1, .Ranged = 1, .Special = "Round 1", .Armour = "None", .Shield = Nothing, .Cost = "1 iron(sword),1 wood(bow)", .ShortName = "a"},
            New UnitStats With {.Name = "Spearman", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Special = "+1 vs cavalry", .Armour = "Chainmail", .Shield = "Wooden", .Cost = "1 iron(spear),1 chainmail,1 wood(shield)", .ShortName = "li"},
            New UnitStats With {.Name = "Heavy Infantry", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Special = "+1 first round (charge)", .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(2H sword),2 iron(plate)", .ShortName = "hi"},
            New UnitStats With {.Name = "Light Cavalry", .Type = UnitType.LightCavalry, .HP = 5, .Melee = 1, .Ranged = 0, .Special = "+1 when chasing", .Armour = "Chainmail", .Shield = "Wooden", .Cost = "1 iron(sword),1 chainmail,1 wood(shield),1 horse", .ShortName = "lc"},
            New UnitStats With {.Name = "Heavy Cavalry", .Type = UnitType.HeavyCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Special = "+1 charge first round", .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(lance),2 iron(plate),1 horse", .ShortName = "hc"}
        }
    }
        AllRaces.Add(humanUnits)

        ' ----------------- Elves -----------------
        Dim elfUnits As New RaceUnits With {
        .RaceName = "Elf",
        .Units = New List(Of UnitStats) From {
            New UnitStats With {.Name = "Archer", .Type = UnitType.Archer, .HP = 5, .Melee = 1, .Ranged = 2, .Special = "Round 1", .Armour = "None", .Shield = Nothing, .Cost = "1 iron(sword),1 wood(bow)", .ShortName = "a"},
            New UnitStats With {.Name = "Forest Sentinel", .Type = UnitType.LightInfantry, .HP = 5, .Melee = 1, .Ranged = 0, .Special = Nothing, .Armour = "Chainmail", .Shield = "Wooden", .Cost = "1 iron(sword),1 chainmail,1 wood(shield)", .ShortName = "li"},
            New UnitStats With {.Name = "Ranger", .Type = UnitType.HeavyInfantry, .HP = 10, .Melee = 2, .Ranged = 0, .Special = "+1 first round (charge)", .Armour = "Plate", .Shield = Nothing, .Cost = "1 iron(moonblade),2 iron(plate)", .ShortName = "hi"},
            New UnitStats With {.Name = "Forest Knight", .Type = UnitType.LightCavalry, .HP = 10, .Melee = 2, .Ranged = 0, .Special = "+1 charge first round", .Armour = "Plate", .Shield = "Iron", .Cost = "1 iron(moonlance),2 iron(plate),1 iron(shield),1 elk", .ShortName = "lc"},
            New UnitStats With {.Name = "Forest Archer", .Type = UnitType.HeavyCavalry, .HP = 5, .Melee = 1, .Ranged = 2, .Special = "Round 1", .Armour = "Chainmail", .Shield = "Wooden", .Cost = "1 iron(sword),1 chainmail,1 wood(shield),1 elk", .ShortName = "hc"}
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
                .Race = p.Race
            }

                ' Start with Light Infantry (type = LightInfantry)
                Dim lightInfTemplate As UnitStats = raceUnits.GetUnitByType(UnitType.LightInfantry)
                Dim unit As New Unit With {
                .Name = lightInfTemplate.Name,
                .HP = lightInfTemplate.HP,
                .Melee = lightInfTemplate.Melee,
                .Ranged = lightInfTemplate.Ranged,
                .Special = lightInfTemplate.Special,
                .Size = 500,
                .Armour = lightInfTemplate.Armour,
                .Shield = lightInfTemplate.Shield
            }
                army.Units.Add(unit)

                ' Add army to player
                p.Armies.Add(army)

                ' Optional: Debug log
                Dim logLine As String = $"{p.Race} Army {a} at ({army.X},{army.Y}): {unit.Name}({unit.Size}) | TotalSoldiers = {army.TotalSoldiers}"
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

        ' Loop over each step
        For stepIndex As Integer = 0 To maxSteps - 1
            For Each p In Players
                If p.Armies IsNot Nothing Then
                    For Each a In p.Armies

                        ' --- Fill MoveQueue if AI-controlled ---
                        If p.AIControlled AndAlso a.MoveQueue.Count = 0 Then
                            a.MoveQueue = GenerateAIMoves(a, p, maxSteps)
                        End If

                        ' --- Execute move for this step if exists ---
                        If stepIndex < a.MoveQueue.Count Then
                            Dim moveCommand As String = a.MoveQueue(stepIndex).Command.ToUpper().Trim()
                            Dim dx As Integer = 0
                            Dim dy As Integer = 0
                            Dim isSpecial As Boolean = False

                            ' Determine if 5th move / special action
                            If stepIndex = maxSteps - 1 Then
                                Dim validDirs As String() = {"N", "NE", "E", "SE", "S", "SW", "W", "NW"}
                                If Not validDirs.Contains(moveCommand) Then isSpecial = True
                            End If

                            If isSpecial Then
                                a.HasUsedSpecial = True
                                Dim owner = Players.First(Function(pl) pl.Armies.Contains(a))

                                Select Case moveCommand

                                    Case "RECRUIT"
                                        ' Ensure the army has population to recruit
                                        If owner.Population <= 0 Then
                                            Console.WriteLine(owner.Race & "'s army at (" & a.X & "," & a.Y & ") cannot recruit: no population left.")
                                            Exit Select
                                        End If

                                        ' --- Get the parameter for the unit to recruit ---
                                        Dim unitName As String = ""
                                        Dim currentCmd As ArmyCommand = a.MoveQueue(stepIndex)
                                        If currentCmd IsNot Nothing Then
                                            unitName = currentCmd.Parameter
                                        End If

                                        ' Default to Light Infantry if no parameter given
                                        If String.IsNullOrWhiteSpace(unitName) Then unitName = "LightInfantry"

                                        ' --- Get the race units ---
                                        Dim raceUnits As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName = owner.Race)
                                        If raceUnits Is Nothing Then
                                            Console.WriteLine("Recruitment failed: Race not found for " & owner.Race)
                                            Exit Select
                                        End If

                                        ' --- Try to find the unit ---
                                        Dim unitType As UnitType
                                        Dim templateUnit As UnitStats = Nothing

                                        ' 1) Try enum parse
                                        If [Enum].TryParse(unitName, True, unitType) Then
                                            RecruitArmyUnits(a, owner, unitType)
                                            Exit Select
                                        End If

                                        ' 2) Try full unit name
                                        templateUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Name.Equals(unitName, StringComparison.OrdinalIgnoreCase))
                                        If templateUnit IsNot Nothing Then
                                            RecruitArmyUnits(a, owner, templateUnit.Type)
                                            Exit Select
                                        End If

                                        ' 3) Try short name
                                        templateUnit = raceUnits.Units.FirstOrDefault(Function(u) u.ShortName.Equals(unitName, StringComparison.OrdinalIgnoreCase))
                                        If templateUnit IsNot Nothing Then
                                            RecruitArmyUnits(a, owner, templateUnit.Type)
                                            Exit Select
                                        End If

                                        ' 4) Unknown input
                                        Console.WriteLine("Unknown unit type to recruit: " & unitName)


                                    Case "DISBAND"
                                        ' --- Total soldiers in the army ---
                                        Dim totalArmySize As Integer = a.Units.Sum(Function(u) u.Size)
                                        If totalArmySize = 0 Then Exit Select

                                        ' --- Determine 5% of total army to disband ---
                                        Dim disbandAmount As Integer = Math.Max(1, CInt(totalArmySize * 0.05))
                                        Dim totalAddedBack As Integer = 0

                                        ' --- Remove proportionally from each unit ---
                                        For Each u In a.Units
                                            Dim portion As Integer = CInt(Math.Round(disbandAmount * (u.Size / totalArmySize)))
                                            portion = Math.Min(portion, u.Size) ' Ensure we don't remove more than unit size
                                            u.Size -= portion
                                            totalAddedBack += portion
                                        Next

                                        ' --- Return disbanded soldiers to civilian population ---
                                        owner.Population += totalAddedBack
                                        Console.WriteLine($"{owner.Race}'s army at ({a.X},{a.Y}) disbands {totalAddedBack} soldiers proportionally back to population.")

                                    Case "TRAIN"
                                        ' Placeholder
                                        Console.WriteLine($"{owner.Race}'s army at ({a.X},{a.Y}) trains (placeholder).")

                                    Case "PATROL"
                                        ' Placeholder
                                        Console.WriteLine($"{owner.Race}'s army at ({a.X},{a.Y}) patrols (placeholder).")

                                    Case Else
                                        Console.WriteLine($"{owner.Race}'s army at ({a.X},{a.Y}) performs unknown special action: {moveCommand}")
                                End Select

                            Else
                                ' --- Movement ---
                                Dim offset As Point = DirectionToOffset(moveCommand)
                                dx = offset.X
                                dy = offset.Y

                                Dim newX = Math.Max(0, Math.Min(mapSize - 1, a.X + dx))
                                Dim newY = Math.Max(0, Math.Min(mapSize - 1, a.Y + dy))
                                a.X = newX
                                a.Y = newY

                                ' Capture adjacent tiles (N/E/S/W)
                                Dim captured As Boolean = False
                                Dim directions As Point() = {New Point(0, -1), New Point(1, 0), New Point(0, 1), New Point(-1, 0)}
                                For Each d In directions
                                    Dim adjX = newX + d.X
                                    Dim adjY = newY + d.Y
                                    If adjX >= 0 AndAlso adjX < mapSize AndAlso adjY >= 0 AndAlso adjY < mapSize Then
                                        If Map(adjX, adjY, 1) = p.PlayerNumber Then
                                            Map(newX, newY, 1) = p.PlayerNumber
                                            captured = True
                                            Exit For
                                        End If
                                    End If
                                Next

                                Console.WriteLine($"{p.Race}'s army moves {moveCommand} to ({newX},{newY})" &
                                              If(captured, " and captures tile", ""))
                            End If
                        End If
                    Next
                End If
            Next

            ' --- Resolve combat after each step ---
            ResolveCombat()
        Next

        ' --- Clear move queues and reset special flags ---
        For Each p In Players
            For Each a In p.Armies
                a.MoveQueue.Clear()
                a.HasUsedSpecial = False
            Next
        Next
    End Sub


    Public Sub RecruitArmyUnits(army As Army, player As Player, unitType As UnitType)
        ' --- Skip recruitment if no population available ---
        If player.Population <= 0 Then
            Console.WriteLine(player.Race & " has no population left to recruit.")
            Return
        End If

        ' --- Find the race units ---
        Dim raceUnits As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName = player.Race)
        If raceUnits Is Nothing Then
            Console.WriteLine("Recruitment failed: Race not found for " & player.Race)
            Return
        End If

        ' --- Get the template unit for this type ---
        Dim templateUnit As UnitStats = raceUnits.GetUnitByType(unitType)
        If templateUnit Is Nothing Then
            Console.WriteLine("Recruitment failed: Unit type not found: " & unitType.ToString())
            Return
        End If

        ' --- Determine recruitment amount: 5% of population, minimum 1 ---
        Dim recruitAmount As Integer = Math.Max(1, CInt(player.Population * 0.05))

        ' --- Cap recruitment to remaining population ---
        recruitAmount = Math.Min(recruitAmount, player.Population)

        ' --- Add to army: either existing unit or new unit ---
        Dim armyUnit As Unit = army.Units.FirstOrDefault(Function(u) u.Name = templateUnit.Name)
        If armyUnit IsNot Nothing Then
            armyUnit.Size += recruitAmount
        Else
            Dim newUnit As New Unit With {
            .Name = templateUnit.Name,
            .HP = templateUnit.HP,
            .Melee = templateUnit.Melee,
            .Ranged = templateUnit.Ranged,
            .Special = templateUnit.Special,
            .Size = recruitAmount,
            .Armour = templateUnit.Armour,
            .Shield = templateUnit.Shield
        }
            army.Units.Add(newUnit)
        End If

        ' --- Deduct population ---
        player.Population -= recruitAmount

        ' --- Log recruitment ---
        Console.WriteLine(player.Race & "'s army at (" & army.X & "," & army.Y & ") recruits " & recruitAmount & " " & templateUnit.Name)
    End Sub

    Private Function GenerateAIMoves(army As Army, player As Player, maxSteps As Integer) As List(Of ArmyCommand)
        Dim moves As New List(Of ArmyCommand)()
        Dim mapSize As Integer = Map.GetLength(0)

        ' Hypothetical position for planning moves
        Dim currentX As Integer = army.X
        Dim currentY As Integer = army.Y

        ' Determine army index for role
        Dim armyIndex As Integer = player.Armies.IndexOf(army)

        ' Loop to generate moves
        For stepIndex As Integer = 0 To maxSteps - 1
            Dim cmd As New ArmyCommand()
            cmd.Parameter = ""

            ' Step 5 is special / recruitment
            If stepIndex = maxSteps - 1 Then
                If player.Population > 0 AndAlso army.TotalSoldiers > 0 Then
                    ' Add AI recruit command using cleaned-up method
                    AIRecruitArmy(army, player, armyIndex)
                    ' Skip adding a placeholder movement
                    Continue For
                End If
            End If

            ' --- Regular movement logic ---
            Dim directions As String() = {"N", "NE", "E", "SE", "S", "SW", "W", "NW"}
            Dim candidateDirs As New List(Of String)
            Dim dirScores As New Dictionary(Of String, Integer)

            ' Evaluate each direction for capture
            For Each direction In directions
                Dim offset As Point = DirectionToOffset(direction)
                Dim newX As Integer = Math.Max(0, Math.Min(mapSize - 1, currentX + offset.X))
                Dim newY As Integer = Math.Max(0, Math.Min(mapSize - 1, currentY + offset.Y))

                If IsCaptureValid(newX, newY, player.PlayerNumber, Map) Then
                    candidateDirs.Add(direction)
                    dirScores(direction) = 1 ' Score for prioritizing capture
                End If
            Next

            ' Pick move
            Dim chosenMove As String
            If candidateDirs.Count > 0 Then
                ' Randomly pick among best-scoring directions
                Dim bestScore As Integer = dirScores.Values.Max()
                Dim bestDirs = dirScores.Where(Function(kvp) kvp.Value = bestScore).Select(Function(kvp) kvp.Key).ToList()
                chosenMove = bestDirs(rnd.Next(bestDirs.Count))
            Else
                ' No capture moves available: pick any random direction
                chosenMove = directions(rnd.Next(directions.Length))
            End If

            ' Store move as ArmyCommand
            cmd.Command = chosenMove
            moves.Add(cmd)

            ' Update hypothetical position for next step
            Dim offsetPt As Point = DirectionToOffset(chosenMove)
            currentX = Math.Max(0, Math.Min(mapSize - 1, currentX + offsetPt.X))
            currentY = Math.Max(0, Math.Min(mapSize - 1, currentY + offsetPt.Y))
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

    Private Sub ResolveCombat()
        Dim armiesAlreadyInBattle As New HashSet(Of Army)() ' Track armies that have already participated
        Dim processedLocations As New HashSet(Of Point)() ' Avoid duplicate combat per tile

        ' Step 1: Check all army pairs for overlapping 5x5 grids
        For i As Integer = 0 To Players.Count - 1
            Dim p1 As Player = Players(i)
            For Each a1 In p1.Armies
                If armiesAlreadyInBattle.Contains(a1) Then Continue For

                Dim grid1 = GetCombatGrid(a1)

                For j As Integer = i To Players.Count - 1
                    Dim p2 As Player = Players(j)
                    For Each a2 In p2.Armies
                        If a1 Is a2 OrElse p1.PlayerNumber = p2.PlayerNumber Then Continue For
                        If armiesAlreadyInBattle.Contains(a2) Then Continue For

                        Dim grid2 = GetCombatGrid(a2)
                        Dim overlap = grid1.Intersect(grid2).ToList()
                        If overlap.Count = 0 Then Continue For

                        ' Determine clash tile
                        Dim clashTile As New Point(CInt(overlap.Average(Function(pt) pt.X)), CInt(overlap.Average(Function(pt) pt.Y)))
                        If processedLocations.Contains(clashTile) Then Continue For
                        processedLocations.Add(clashTile)

                        ' Find all armies involved at clashTile
                        Dim involvedArmies = Players.SelectMany(Function(p) p.Armies).Where(Function(a) GetCombatGrid(a).Contains(clashTile) AndAlso Not armiesAlreadyInBattle.Contains(a)).ToList()

                        ' Only engage if 2+ armies from different players
                        If involvedArmies.Select(Function(a) Players.First(Function(p) p.Armies.Contains(a)).PlayerNumber).Distinct().Count() < 2 Then Continue For

                        ' Step 2: Execute battle
                        Dim battleLog As BattleLog = Battle(involvedArmies) ' Returns full log without displaying
                        armiesAlreadyInBattle.UnionWith(involvedArmies)

                        ' Step 3: Show report in scrollable form
                        If battleLog IsNot Nothing Then
                            Dim reportWindow As New frmBattleReport()
                            reportWindow.ShowReport(GenerateBattleReport(battleLog))
                        End If
                    Next
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
            ' Reset per-turn collection
            p.FoodCollectedThisTurn = 0
            p.IronCollectedThisTurn = 0
            p.WoodCollectedThisTurn = 0

            ' Count owned squares
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

            ' Determine preferred terrain
            Dim preferredTerrain As Integer
            Select Case p.Race.ToLower()
                Case "elf"
                    preferredTerrain = 1
                Case "dwarf"
                    preferredTerrain = 3
                Case "orc"
                    preferredTerrain = 2
                Case "human"
                    preferredTerrain = 0
                Case Else
                    preferredTerrain = -1
            End Select

            ' Loop through owned squares only
            For x As Integer = 0 To rows - 1
                For y As Integer = 0 To cols - 1
                    If Map(x, y, 1) = p.PlayerNumber Then
                        Dim terrain As Integer = Map(x, y, 0)
                        Dim foodPerSquare As Integer = 1
                        Dim ironPerSquare As Integer = 1
                        Dim woodPerSquare As Integer = 1

                        ' Apply racial bonus
                        If terrain = preferredTerrain Then
                            foodPerSquare += 1
                        End If

                        ' Add resources based on population per square
                        p.FoodCollectedThisTurn += foodPerSquare * popPerSquare
                        p.IronCollectedThisTurn += ironPerSquare * popPerSquare
                        p.WoodCollectedThisTurn += woodPerSquare * popPerSquare
                    End If
                Next
            Next

            ' Apply territory bonus (2% per owned tile)
            Dim territoryBonus As Double = ownedSquares * 0.005
            p.FoodCollectedThisTurn = CInt(p.FoodCollectedThisTurn * (1 + territoryBonus))
            p.IronCollectedThisTurn = CInt(p.IronCollectedThisTurn * (1 + territoryBonus))
            p.WoodCollectedThisTurn = CInt(p.WoodCollectedThisTurn * (1 + territoryBonus))

            ' Add to cumulative totals
            p.Iron += p.IronCollectedThisTurn
            p.Wood += p.WoodCollectedThisTurn

            ' Track food for feeding / growth
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
        ''' <summary>
        ''' Executes a battle between multiple armies in four phases: Ranged, Charge, Melee, Chase.
        ''' Returns the BattleLog containing full technical details.
        ''' Does NOT display the report; the caller handles output.
        ''' </summary>

        ' --- Filter active armies ---
        Dim activeArmies As List(Of Army) = battleArmies.Where(Function(a) a.TotalSoldiers > 0).ToList()
        If activeArmies.Count = 0 Then Return Nothing

        ' --- Initialize BattleLog ---
        Dim battleLog As New BattleLog(activeArmies)

        ' --- PHASE 1: RANGED ---
        Dim unitSnapshot As New Dictionary(Of Unit, Integer)
        For Each army In activeArmies
            For Each u In army.Units
                unitSnapshot(u) = u.Size
            Next
        Next
        For Each defArmy In activeArmies
            Dim attackers = activeArmies.Where(Function(a) a IsNot defArmy).SelectMany(Function(a) a.Units).ToList()
            ApplyProportionalDamage(defArmy, attackers, "Ranged", unitSnapshot, battleLog)
        Next

        ' --- PHASE 2: CHARGE ---
        unitSnapshot = New Dictionary(Of Unit, Integer)
        For Each army In activeArmies
            For Each u In army.Units
                unitSnapshot(u) = u.Size
            Next
        Next
        For Each defArmy In activeArmies
            Dim attackers = activeArmies.Where(Function(a) a IsNot defArmy).SelectMany(Function(a) a.Units).ToList()
            ApplyProportionalDamage(defArmy, attackers, "Charge", unitSnapshot, battleLog)
        Next

        ' --- PHASE 3: MELEE ---
        unitSnapshot = New Dictionary(Of Unit, Integer)
        For Each army In activeArmies
            For Each u In army.Units
                unitSnapshot(u) = u.Size
            Next
        Next
        For Each defArmy In activeArmies
            Dim attackers = activeArmies.Where(Function(a) a IsNot defArmy).SelectMany(Function(a) a.Units).ToList()
            ApplyProportionalDamage(defArmy, attackers, "Melee", unitSnapshot, battleLog)
        Next

        ' --- PHASE 4: CHASE ---
        Dim strongestArmy = activeArmies.OrderByDescending(Function(a) a.TotalSoldiers).FirstOrDefault()
        If strongestArmy IsNot Nothing AndAlso strongestArmy.TotalSoldiers > 0 Then
            unitSnapshot = New Dictionary(Of Unit, Integer)
            For Each army In activeArmies
                For Each u In army.Units
                    unitSnapshot(u) = u.Size
                Next
            Next

            For Each army In activeArmies
                If army IsNot strongestArmy AndAlso army.TotalSoldiers < 0.5 * strongestArmy.TotalSoldiers Then
                    Dim chasingUnits = strongestArmy.Units.Where(Function(u) u.Type = UnitType.LightCavalry AndAlso u.Size > 0).ToList()
                    ApplyProportionalDamage(army, chasingUnits, "Chase", unitSnapshot, battleLog)
                End If
            Next
        End If

        ' --- Post-Battle: Retreat / Spawn Handling ---
        Dim maxStrength As Integer = If(strongestArmy IsNot Nothing, strongestArmy.TotalSoldiers, 0)
        If maxStrength > 0 Then
            For Each army In activeArmies
                If army.TotalSoldiers < 0.5 * maxStrength AndAlso army.TotalSoldiers > 0 Then
                    SendArmyBackToSpawn(army)
                    ClearArmyMoveQueue(army)
                    battleLog.RecordRetreat(army)
                End If
            Next
        End If

        ' --- Return the BattleLog for caller to handle display or further processing ---
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
        ''' <summary>
        ''' Applies proportional damage from multiple attackers to a defending army.
        ''' Damage is calculated using snapshot sizes to ensure simultaneity.
        ''' Each attack is logged individually, with SizeBefore and SizeAfter.
        ''' </summary>

        ' --- Initialize dictionary to accumulate casualties per defender unit ---
        Dim calculatedCasualties As New Dictionary(Of Unit, Long)
        For Each defUnit In defArmy.Units
            calculatedCasualties(defUnit) = 0
        Next

        ' --- Loop over each attacker and apply proportional damage to all defender units ---
        For Each atkUnit In attackingUnits
            Dim atkSize As Integer = unitSnapshot(atkUnit)
            Dim atkValue As Integer = 0

            Select Case phase.ToLower()
                Case "ranged"
                    If atkUnit.Ranged > 0 Then
                        atkValue = atkUnit.Ranged
                        ' Elf archer bonus
                        If atkUnit.Name.ToLower().Contains("archer") AndAlso
                       atkUnit.Special IsNot Nothing AndAlso
                       atkUnit.Special.ToLower().Contains("elf") Then
                            atkValue += 1
                        End If
                    Else
                        Continue For
                    End If

                Case "charge"
                    atkValue = atkUnit.Melee + GetChargeBonus(atkUnit)
                Case "melee"
                    atkValue = atkUnit.Melee
                Case "chase"
                    If atkUnit.Type = UnitType.LightCavalry Then
                        atkValue = atkUnit.Melee * 2
                    Else
                        Continue For
                    End If
            End Select

            Dim totalDefHP As Double = defArmy.Units.Sum(Function(u) u.HP * unitSnapshot(u))
            If totalDefHP = 0 Then Continue For

            ' --- Apply proportional damage to each defender ---
            For Each defUnit In defArmy.Units
                Dim sizeBefore As Integer = unitSnapshot(defUnit)
                Dim prop As Double = (defUnit.HP * sizeBefore) / totalDefHP
                Dim damage As Double = atkValue * atkSize * prop
                damage *= (1 - GetUnitMitigation(defUnit))

                ' Calculate casualties but only accumulate here
                Dim rawCasualties As Double = Math.Floor(damage / defUnit.HP)
                Dim casualties As Long = Math.Min(sizeBefore, CLng(rawCasualties))
                calculatedCasualties(defUnit) += casualties

                ' --- Record each attacker-defender interaction ---
                Dim entry As New BattleEntry With {
                .Attacker = atkUnit,
                .Defender = defUnit,
                .Damage = damage,
                .Phase = phase,
                .SizeBefore = sizeBefore,
                .SizeAfter = CInt(Math.Max(0, sizeBefore - calculatedCasualties(defUnit)))
            }
                battleLog.PhaseEntries(phase).Add(entry)
            Next
        Next

        ' --- Apply all casualties simultaneously ---
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

    Public Function GetUnitMitigation(unit As Unit) As Double
        ''' <summary>
        ''' Returns the mitigation factor for a unit.
        ''' 
        ''' - Armour mitigation:
        '''     None: 0
        '''     Chainmail: 25%
        '''     Plate: 50%
        ''' - Shield mitigation:
        '''     Wooden: 10%
        '''     Iron: 20%
        ''' - Race-specific bonuses:
        '''     Dwarf HI (Stormforged Hammerguards): extra mitigation vs melee and ranged (+additional 0.1)
        '''     Orc LI (War Grunts): extra mitigation even without armour/shield (+0.1)
        ''' 
        ''' Returns a value between 0 and 1 representing the percentage of damage mitigated.
        ''' </summary>

        Dim mitigation As Double = 0

        ' Armour mitigation
        Select Case unit.Armour?.ToLower()
            Case "chainmail"
                mitigation += 0.25
            Case "plate"
                mitigation += 0.5
            Case Else
                mitigation += 0
        End Select

        ' Shield mitigation
        Select Case unit.Shield?.ToLower()
            Case "wooden"
                mitigation += 0.1
            Case "iron"
                mitigation += 0.2
            Case Else
                mitigation += 0
        End Select

        ' Race-specific bonuses
        If unit.Name.ToLower() = "stormforged hammerguards" Then
            mitigation += 0.1 ' extra mitigation for Dwarf HI
        ElseIf unit.Name.ToLower() = "war grunts" Then
            mitigation += 0.2 ' extra mitigation for Orc LI
        End If

        ' Ensure mitigation does not exceed 1
        If mitigation > 1 Then mitigation = 1

        Return mitigation
    End Function

    Public Function GenerateBattleReport(battleLog As BattleLog) As String
        ''' <summary>
        ''' Generates a detailed, technical, phase-by-phase battle report.
        ''' Includes for each phase:
        ''' - Attacking unit
        ''' - Target unit
        ''' - Damage dealt
        ''' - Defender size BEFORE damage
        ''' - Defender size AFTER damage
        ''' - Full list of units in each army
        ''' Also lists armies sent back to spawn at the end.
        ''' </summary>

        Dim report As New Text.StringBuilder()

        report.AppendLine("=== Detailed Battle Report ===")
        report.AppendLine()

        ' Phase-by-phase details
        For Each phase In New String() {"Ranged", "Charge", "Melee", "Chase"}
            report.AppendLine($"--- Phase: {phase} ---")

            If battleLog.PhaseEntries.ContainsKey(phase) Then
                ' Group entries by defending unit
                Dim groupedByDefender = battleLog.PhaseEntries(phase).GroupBy(Function(e) e.Defender)

                For Each group In groupedByDefender
                    Dim defUnit = group.Key
                    report.AppendLine($"Defender: {defUnit.Name}")

                    For Each entry In group
                        Dim attackerName = entry.Attacker.Name
                        Dim damage = Math.Round(entry.Damage, 2)
                        Dim sizeBefore = entry.SizeBefore
                        Dim sizeAfter = entry.SizeAfter

                        report.AppendLine($"   {attackerName} deals {damage} damage to {defUnit.Name} " &
                                      $"(Units: {sizeBefore} -> {sizeAfter})")
                    Next

                    ' Optional: show full army composition after phase
                    Dim army = battleLog.PhaseEntries(phase).Select(Function(e) e.Defender).Distinct()
                    report.AppendLine()
                Next
            Else
                report.AppendLine("No actions in this phase.")
            End If

            report.AppendLine()
        Next

        ' Retreats
        If battleLog.Retreats.Count > 0 Then
            report.AppendLine("--- Retreats ---")
            For Each army In battleLog.Retreats
                report.AppendLine($"Army of {army.Race} sent back to spawn at ({army.X},{army.Y}). " &
                              $"Remaining soldiers: {army.TotalSoldiers}")
            Next
            report.AppendLine()
        End If

        report.AppendLine("=== End of Detailed Battle Report ===")

        Return report.ToString()
    End Function


    Public Sub AIRecruitArmy(army As Army, player As Player, armyIndex As Integer)
        ' Determine army role based on index
        Dim role As String
        Select Case armyIndex
            Case 0
                role = "Offensive"
            Case 1
                role = "Flexible"
            Case 2
                role = "Defensive"
            Case Else
                role = "Flexible"
        End Select

        ' Get race units
        Dim raceUnits As RaceUnits = AllRaces.FirstOrDefault(Function(r) r.RaceName = player.Race)
        If raceUnits Is Nothing Then Exit Sub

        Dim chosenUnit As UnitStats = Nothing

        ' Step 0: Determine favored unit based on role
        Select Case role
            Case "Offensive"
                ' Prefer Heavy Infantry
                chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.HeavyInfantry AndAlso CanAffordUnit(player, u))
                If chosenUnit Is Nothing Then
                    ' Fallback Heavy Cavalry
                    chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.HeavyCavalry AndAlso CanAffordUnit(player, u))
                End If
                If chosenUnit Is Nothing Then
                    ' Fallback Light Cavalry
                    chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.LightCavalry AndAlso CanAffordUnit(player, u))
                End If

            Case "Flexible"
                ' Random affordable unit
                Dim affordableUnits = raceUnits.Units.Where(Function(u) CanAffordUnit(player, u)).ToList()
                If affordableUnits.Count > 0 Then
                    chosenUnit = affordableUnits(rnd.Next(affordableUnits.Count))
                End If

            Case "Defensive"
                ' Prefer Light Infantry
                chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.LightInfantry AndAlso CanAffordUnit(player, u))
                If chosenUnit Is Nothing Then
                    ' Fallback Archer
                    chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) u.Type = UnitType.Archer AndAlso CanAffordUnit(player, u))
                End If
        End Select

        ' Step 1: Last-resort – first affordable unit
        If chosenUnit Is Nothing Then
            chosenUnit = raceUnits.Units.FirstOrDefault(Function(u) CanAffordUnit(player, u))
        End If

        ' Step 2: Add RECRUIT command to army MoveQueue if unit found
        If chosenUnit IsNot Nothing Then
            Dim cmd As New ArmyCommand With {
            .Command = "RECRUIT",
            .Parameter = chosenUnit.ShortName
        }
            army.MoveQueue.Add(cmd)
        End If
    End Sub


    Private Function CanAffordUnit(player As Player, unit As UnitStats) As Boolean
        ' Parse unit.Cost string and compare to player resources
        ' Example cost format: "1 iron(axe),1 iron(chainmail),1 wood(shield)"
        ' Simplified: just check if player has at least 1 iron and 1 wood per unit
        ' You can expand this to your full resource parsing logic

        ' For now, assume cost string always contains "iron" and/or "wood"
        Dim ironRequired As Integer = 0
        Dim woodRequired As Integer = 0

        Dim parts() As String = unit.Cost.Split(","c)
        For Each part In parts
            Dim trimmed As String = part.Trim()
            If trimmed.StartsWith("1 iron") Then ironRequired += 1
            If trimmed.StartsWith("1 wood") Then woodRequired += 1
        Next

        Return (player.Iron >= ironRequired AndAlso player.Wood >= woodRequired)
    End Function


End Class
