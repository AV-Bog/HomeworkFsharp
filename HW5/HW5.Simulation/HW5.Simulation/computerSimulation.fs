module computerSimulation

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

type SimulationResult = {
    TotalTurns: int
    TotalInfected: int
    Logs: string list 
}
    
type Network (computers: Comp[], matrix: bool[,], virus: Virus, log: string -> unit, random: unit -> float) =
    let mutable currentInfect: int list = []
    let mutable newInfect: int list = []
    let mutable countTurn = 0
    
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
            let roll = random()
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
    
    member private n.SprintNetwork()=
        for computer in computers do
            log (sprintf "  %s" (computer.ToString()))
            
    member this.GetInfectedCount() =
        currentInfect.Length
        
    member n.RunSimulation() =
        log (sprintf "=== ЗАПУСК СИМУЛЯЦИИ ВИРУСА: %s ===" virus.Name)
        log "Начальное состояние сети:"
        n.SprintNetwork()
        
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
                        let rool = random()
                        if rool < probubility then
                            newInfect <- neighbore :: newInfect
            
            let actualNewnInfect =
                newInfect |> List.filter(fun id ->
                    let computer = n.GetCompById(id)
                    computer.Infect())
            
            log (sprintf "=== ХОД %d ===" countTurn)
            
            if actualNewnInfect.IsEmpty then
                log "Новых заражений нет."
                log "Симуляция завершена."
                log "Итоговое состояние сети:"
                n.SprintNetwork()
                log (sprintf "Всего ходов: %d" countTurn)
                log (sprintf "Всего заражено компьютеров: %d" currentInfect.Length)
                flag <- true
            else
                log (sprintf "Заразились в этом ходу: %s" 
                    (actualNewnInfect |> List.map (fun id -> sprintf "PC-%d" id) |> String.concat ", "))
                
                currentInfect <- List.append currentInfect actualNewnInfect
                
                log "Текущее состояние сети:"
                n.SprintNetwork()
        {
            TotalTurns = countTurn
            TotalInfected = currentInfect.Length
            Logs = []
        }        