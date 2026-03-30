module Workflow.Calculats

open System

type theNumberCalculate() =
    member _.Return a = Some(a)
    member _.Bind (str: string, func) =
        match Int32.TryParse(str) with
        | false, _ -> None
        | true, x -> func x

let calculate = theNumberCalculate()

let result1 = calculate {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    }

let result2 = calculate {
        let! x = "1"
        let! y = "Ъ"
        let z = x + y
        return z
    }

type RoundingBuilder(precision: int) =
    member _.Return (b: float) = Some(Math.Round(b, precision))
    member _.Bind (a: float, func: float -> float option) = func (Math.Round(a, precision))
        
let rounding precision = RoundingBuilder(precision)

let rez = rounding 3 {
    let! a = 2.0 / 12.0
    let! b = 3.5
    return a / b
}