module HW5.Simulation.entities

open System

type OS = Windows | Linux | MacOS

type Comp (id: int, os: OS) =
    let mutable isInfected = false
    
    member val Id = id with get
    member val Os = os with get
    member val IsInfected = isInfected with get
    
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
    
    member private n.GetNeighbors(id: int) int list =
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
    
    member private n.InitializePatintZero(pationtZeroId : int) =
        let compPationt = n.GetCompById(pationtZeroId)
        compPationt.Infect() |> ignore
        countTurn <- 0
        currentInfect <- [pationtZeroId]
        