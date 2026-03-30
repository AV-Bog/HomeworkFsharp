module Workflow.Calculats

open System

type theNumberCalculate() =
    member _.Return a = Some(sprintf "%A" a)
    member _.Bind (str: string, func) =
        match Int32.TryParse(str) with
        | false, _ -> None
        | true, x -> func x

type RoundingBuilder(precision: int) =
    member _.Return (b: float) = Some(Math.Round(b, precision))
    member _.Bind (a: float, func: float -> float option) = func (Math.Round(a, precision))
