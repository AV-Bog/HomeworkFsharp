// <copyright file="Virus" author="bogdanovaarina">
// under MIT License
// </copyright>

module HW5.Simulation.Virus

type Virus (name : string, probabilities: Map<OS,float>) =
    member val Name = name with get
    
    member this.GetInfectionProbability(os: OS) : float =
        match probabilities.TryFind os with
        | Some prob -> prob
        | None -> 0.0
        
    override this.ToString() =
        sprintf "Virus '%s'" this.Name
