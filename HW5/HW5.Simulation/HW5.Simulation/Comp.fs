// <copyright file="Comp" author="bogdanovaarina">
// under MIT License
// </copyright>

namespace HW5.Simulation 

type OS = Windows | Linux | MacOS

type Comp (id: int, os: OS) =
    let mutable isInfected = false
    
    member val Id = id with get
    member val Os = os with get
    member c.IsInfected = isInfected
    
    member c.Infect() =
        let wasInfected = isInfected
        isInfected <- true
        not wasInfected
    
    override c.ToString() =
        sprintf "PC-%d (%s) [%s]" 
            c.Id 
            (string c.Os) 
            (if c.IsInfected then "ЗАРАЖЕН" else "Здоров")