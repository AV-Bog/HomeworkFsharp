module Workflow.Calculator

open System

type StringCalculationBuilder() = 
    member _.Return (a: int) : string option = Some $"%d{a}"

    member _.Bind (str: string, func: int -> 'a option) : 'a option =
        match Int32.TryParse(str) with
        | false, _ -> None
        | true, x -> func x
