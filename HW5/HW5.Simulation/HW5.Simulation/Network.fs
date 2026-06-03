// <copyright file="Network" author="bogdanovaarina">
// under MIT License
// </copyright>

module HW5.Simulation.Network

open HW5.Simulation.Virus
open HW5.Simulation.SimulationResult

type Network (computers: Comp[], matrix: bool[,], virus: Virus, log: string -> unit, random: unit -> float) =
    let mutable currentInfected: int list = []
    let mutable countTurn = 0
    
    member private n.GetCompById(id: int) : Comp =
        computers |> Array.find (fun c -> c.Id = id)
    
    member private n.GetNeighbors(id: int) : int list =
        [0 .. computers.Length - 1] |> List.filter( fun i -> matrix[id, i])
        
    member private n.TryInjectComp(id: int) : bool =
        let comp = n.GetCompById(id)
        let probability = virus.GetInfectionProbability(comp.Os)
        let roll = random()
        if roll < probability then
            comp.Infect()
        else
            false
    
    member n.InitializePatientZero(patientZeroId : int) =
        let compPatient = n.GetCompById(patientZeroId)
        compPatient.Infect() |> ignore
        countTurn <- 0
        currentInfected <- [patientZeroId]
    
    member private n.PrintNetwork()=
        for computer in computers do
            log $"  %s{computer.ToString()}"

    member this.GetInfectedCount() =
        currentInfected.Length
        
    member n.RunSimulation() =
        log $"=== ЗАПУСК СИМУЛЯЦИИ ВИРУСА: %s{virus.Name} ==="
        log "Начальное состояние сети:"
        n.PrintNetwork()
        
        let mutable hasCandidates = true
        
        while hasCandidates do
            countTurn <- countTurn + 1
            let mutable candidatesForInfection = []
            
            let infectedToProcess = List.rev currentInfected
            
            for infComp in infectedToProcess do
                let neighbors = n.GetNeighbors(infComp)
                for neighbor in neighbors do
                    let compNeighbor = n.GetCompById(neighbor)
                    if not compNeighbor.IsInfected then
                        candidatesForInfection <- neighbor :: candidatesForInfection
            
            let uniqueCandidates = candidatesForInfection |> List.distinct
            
            let actualNewInfected =
                uniqueCandidates |> List.filter (fun id -> n.TryInjectComp(id))
            
            log $"=== ХОД %d{countTurn} ==="

            if actualNewInfected.IsEmpty then
                log "Новых заражений нет."
                log "Симуляция завершена."
                log "Итоговое состояние сети:"
                n.PrintNetwork()
                log $"Всего ходов: %d{countTurn}"
                log $"Всего заражено компьютеров: %d{currentInfected.Length}"
                hasCandidates <- false
            else
                log (sprintf "Заразились в этом ходу: %s" 
                    (actualNewInfected |> List.map (fun id -> $"PC-%d{id}") |> String.concat ", "))
                
                currentInfected <- List.append currentInfected actualNewInfected
                
                log "Текущее состояние сети:"
                n.PrintNetwork()
        
        {
            TotalTurns = countTurn
            TotalInfected = currentInfected.Length
            Logs = []
        }