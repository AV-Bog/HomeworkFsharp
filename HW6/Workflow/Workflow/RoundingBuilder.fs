// <copyright file="${FileName}" author="bogdanovaarina">
// under MIT License
// </copyright>

module Workflow.RoundingBuilder

open System

type RoundingBuilder(precision: int) =
    member _.Return (b: float) = 
        try
            Some(Math.Round(b, precision))
        with
        | :? ArgumentOutOfRangeException -> None
    
    member _.Bind (a: float, func: float -> float option) = 
        try
            func (Math.Round(a, precision))
        with
        | :? ArgumentOutOfRangeException -> None