module computerSimulation

open System

type OS = Windows | Linux | MacOS

type Comp (id: int, os: OS) =
    let mutable isInfected = false
    
    member val Id = id with get
    member val Os = os with get
    member c.IsInfected = isInfected
    
    member c.Infect() =
        if not isInfected then
            isInfected <- true
            true
        else
            false
    
    override c.ToString() =
        sprintf "PC-%d (%s) [%s]" 
            c.Id 
            (string c.Os) 
            (if c.IsInfected then "ЗАРАЖЕН" else "Здоров")
    
type Virus (name : string, probabilities: Map<OS,float>) =
    member val Name = name with get
    
    member this.GetInfectionProbability(os: OS) : float =
        match probabilities.TryFind os with
        | Some prob -> prob
        | None -> 0.0
        
    override this.ToString() =
        sprintf "Virus '%s'" this.Name

type Network (computers: Comp[], matrix: bool[,], virus: Virus) =
    let mutable currentInfect: int list = []
    let mutable newInfect: int list = []
    let mutable countTurn = 0
    let rgn = Random()
    
    member private n.GetCompById(id: int) : Comp =
        computers |> Array.find (fun c -> c.Id = id)
    
    member private n.GetNeighbors(id: int) : int list =
        [0 .. computers.Length - 1] |> List.filter( fun i -> matrix[id, i])
        
    member private n.TryInfectComp(id: int) bool =
        let comp = n.GetCompById(id)
        if comp.IsInfected then
            false
        else
            let probability = virus.GetInfectionProbability(comp.Os)
            let roll = rgn.NextDouble()
            if roll < probability then
                comp.Infect() |> ignore
                true
            else
                false
    
    member n.InitializePatintZero(pationtZeroId : int) =
        let compPationt = n.GetCompById(pationtZeroId)
        compPationt.Infect() |> ignore
        countTurn <- 0
        currentInfect <- [pationtZeroId]
    
    member private n.PrintNetwork()=
        for computer in computers do
            printfn "  %s" (computer.ToString())
            
    member this.GetInfectedCount() =
        currentInfect.Length
        
    member n.RunSimulation() =
        printfn "=== ЗАПУСК СИМУЛЯЦИИ ВИРУСА: %s ===" virus.Name
        printfn "Начальное состояние сети:"
        n.PrintNetwork()
        printfn ""
        
        let mutable flag = false
        
        while not flag do
            countTurn <- countTurn + 1
            newInfect <- []
            
            let infectedToProcess = List.rev currentInfect
            
            for infComp in infectedToProcess do
                let neighbors = n.GetNeighbors(infComp)
                for neighbore in neighbors do
                    let compNeighbore = n.GetCompById(neighbore)
                    if not compNeighbore.IsInfected then
                        let probubility = virus.GetInfectionProbability(compNeighbore.Os)
                        let rool = rgn.NextDouble()
                        if rool < probubility then
                            newInfect <- neighbore :: newInfect
            
            let actualNewnInfect =
                newInfect |> List.filter(fun id ->
                    let computer = n.GetCompById(id)
                    computer.Infect())
            
            printfn "=== ХОД %d ===" countTurn
            
            if actualNewnInfect.IsEmpty then
                printfn "Новых заражений нет."
                printfn "Симуляция завершена."
                printfn ""
                printfn "Итоговое состояние сети:"
                n.PrintNetwork()
                printfn ""
                printfn "Всего ходов: %d" countTurn
                printfn "Всего заражено компьютеров: %d" currentInfect.Length
                flag <- true
            else
                printfn "Заразились в этом ходу: %s" 
                    (actualNewnInfect |> List.map (fun id -> sprintf "PC-%d" id) |> String.concat ", ")
                
                currentInfect <- List.append currentInfect actualNewnInfect
                
                printfn "Текущее состояние сети:"
                n.PrintNetwork()
                printfn ""
                
                