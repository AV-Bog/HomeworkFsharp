module TestSimulation

open NUnit.Framework

open HW5.Simulation

[<TestFixture>]
type NetworkSimulationTests () =
    
    [<Test>]
    member this.``Test_With_Probability_1_Spreads_Like_BFS`` () =
        let comp0 = Comp(0, Windows)
        let comp1 = Comp(1, Windows)
        let comp2 = Comp(2, Windows)
        let comp3 = Comp(3, Windows)
        let computers = [| comp0; comp1; comp2; comp3 |]
        
        let matrix = array2D [
            [true; true; false; false]
            [true; true; true; false]
            [false; true; true; true]
            [false; false; true; true]
        ]

        let virus = Virus("TestVirus", Map.ofList [(Windows, 1.0)])
        
        let mutable logs = []
        let testLogger (msg: string) = logs <- msg :: logs
        
        let deterministicRandom = fun () -> 0.5
        
        let network = Network(computers, matrix, virus, testLogger, deterministicRandom)
        network.InitializePatientZero(0)
        
        let result = network.RunSimulation()
        
        Assert.That(result.TotalInfected = 4, Is.True, "Все 4 компьютера должны быть заражены")
        
        Assert.That(comp0.IsInfected, Is.True, "!?")
        Assert.That(comp1.IsInfected, Is.True, "!?")
        Assert.That(comp2.IsInfected, Is.True, "!?")
        Assert.That(comp3.IsInfected, Is.True, "!?")
        
        Assert.That(result.TotalTurns = 3, Is.True, "Должно быть 3 хода для линии из 4 узлов")
    
    [<Test>]
    member this.``Test_With_Probability_0_NoOne_Gets_Infected`` () =
        let comp0 = Comp(0, Linux)
        let comp1 = Comp(1, Linux)
        let comp2 = Comp(2, Linux)
        let computers = [| comp0; comp1; comp2 |]
        
        let matrix = array2D [
            [true; true; true]
            [true; true; true]
            [true; true; true]
        ]
        
        let virus = Virus("HarmlessVirus", Map.ofList [(Linux, 0.0)])
        
        let mutable logs = []
        let testLogger (msg: string) = logs <- msg :: logs
        
        let deterministicRandom = fun () -> 0.1
        
        let network = Network(computers, matrix, virus, testLogger, deterministicRandom)
        network.InitializePatientZero(0)
        
        let result = network.RunSimulation()
        
        Assert.That(result.TotalInfected = 1, Is.True, "Только 1 компьютер должен быть заражен")
        
        Assert.That(comp0.IsInfected, Is.True, "PC-0 (patient zero) должен быть заражен")
        Assert.That(comp1.IsInfected, Is.False, "PC-1 НЕ должен быть заражен")
        Assert.That(comp2.IsInfected, Is.False, "PC-2 НЕ должен быть заражен")
        
        Assert.That(result.TotalTurns = 1, Is.True, "Должен быть 1 ход без новых заражений")
    
    [<Test>]
    member this.``Test_With_Specific_Probability_50_percent`` () =
        let comp0 = Comp(0, MacOS)
        let comp1 = Comp(1, MacOS)
        let computers = [| comp0; comp1 |]
        
        let matrix = array2D [
            [true; true]
            [true; true]
        ]
        
        let virus = Virus("MediumVirus", Map.ofList [(MacOS, 0.5)])
        
        let mutable logs = []
        let testLogger (msg: string) = logs <- msg :: logs
        
        // Тест 1: случайное число 0.3 < 0.5 => заражение происходит
        let randomWillInfect = fun () -> 0.3
        
        let network1 = Network(computers, matrix, virus, testLogger, randomWillInfect)
        network1.InitializePatientZero(0)
        let result1 = network1.RunSimulation()
        
        Assert.That(result1.TotalInfected = 2, Is.True, "При random=0.3 (< 0.5) оба компьютера должны заразиться")
        Assert.That(comp0.IsInfected, Is.True)
        Assert.That(comp1.IsInfected, Is.True)
        
        // Тест 2: случайное число 0.7 > 0.5 => заражение НЕ происходит
        let comp0_2 = Comp(0, MacOS)
        let comp1_2 = Comp(1, MacOS)
        let computers2 = [| comp0_2; comp1_2 |]
        
        let randomWontInfect = fun () -> 0.7
        
        let network2 = Network(computers2, matrix, virus, testLogger, randomWontInfect)
        network2.InitializePatientZero(0)
        let result2 = network2.RunSimulation()
        
        Assert.That(result2.TotalInfected = 1, Is.True, "При random=0.7 (> 0.5) только patient zero должен заразиться")
        Assert.That(comp0_2.IsInfected, Is.True)
        Assert.That(comp1_2.IsInfected, Is.False)