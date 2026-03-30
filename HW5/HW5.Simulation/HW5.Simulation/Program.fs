open System
open computerSimulation

[<EntryPoint>]
let main argv =
    let computers = [|
        Comp(0, Windows)
        Comp(1, Linux)
        Comp(2, Windows)
        Comp(3, MacOS)
        Comp(4, Linux)
        Comp(5, MacOS)
    |]
    
    let adjacencyMatrix = array2D [
        [false; true; true; false; false; false]
        [true; false; true; false; false; false]
        [true; true; false; true; false; false]
        [false; false; true; false; true; true]
        [false; false; false; true; false; false]
        [false; false; false; true; false; false]
    ]
    
    let virusProbabilities = Map [
        (OS.Windows, 0.8)
        (OS.Linux, 0.3)
        (OS.MacOS, 0.5)
    ]
    let virus = Virus("WannaCry", virusProbabilities)
    let rgn = Random()
    let random = fun () -> rgn.NextDouble()
    let log = fun (msg: string) -> printfn "%s" msg
        
    let network = Network(computers, adjacencyMatrix, virus, log, random)
    
    network.InitializePatintZero(2)
    
    network.RunSimulation()
    
    printfn "\nНажмите любую клавишу для выхода..."
    Console.ReadKey() |> ignore
    0