// <copyright file="${FileName}" author="bogdanovaarina">
// under MIT License
// </copyright>

module Parser.Parser
open System
open FParsec

type Term =
    | Var of string
    | Abs of string list * Term
    | App of Term * Term

type Definition = {
    Name: string
    Body: Term
}

let ws = spaces
let str s = pstring s .>> ws

let identifier : Parser<string, unit> =
    let isFirstChar c = isLetter c && Char.IsLower(c)
    let isOtherChar c = isLetter c || isDigit c
    (many1Satisfy2L isFirstChar isOtherChar "identifier") .>> ws

let var = identifier |>> Var

let parametrs : Parser<string List, unit> =
    many1 identifier

let term, termRef = createParserForwardedToRef<Term, unit>()

let termPrime : Parser<Term, unit> =
    choice [
        str "\\" >>. parametrs .>> str "." .>>. term |>> Abs;
        var;
        between (str "(") (str ")") term
    ]
    
let app : Parser<Term, unit> =
    let apply head args = List.fold (fun acc arg -> App(acc, arg)) head args
    
    many1 termPrime 
    |>> (function 
        | head :: tail -> apply head tail
        | [] -> failwith "Невозможно")

do termRef.Value <- app

let letDefinition : Parser<Definition, unit> =
    str "let" >>. identifier .>> str "=" .>>. term
    |>> (fun (name, value) -> { Name = name; Body = value })

type ParseResult = {
    Definitions: Map<string, Term>
    MainExpression: Term
}

let parseInput (input: string) : ParseResult option =
    let lines = input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    let definitions = ref Map.empty
    let mutable mainExpr = None
    let mutable errorMsg = None
    
    for line in lines do
        if errorMsg.IsNone then
            let trimmed = line.Trim()
            if trimmed <> "" then
                match run (letDefinition .>> ws .>> eof) trimmed with
                | Success(def, _, _) ->
                    definitions.Value <- Map.add def.Name def.Body definitions.Value
                | _ ->
                    match run (term .>> ws .>> eof) trimmed with
                    | Success(expr, _, _) ->
                        if mainExpr.IsNone then
                            mainExpr <- Some expr
                        else
                            errorMsg <- Some "Несколько основных выражений"
                    | Failure(err, _, _) ->
                        errorMsg <- Some $"Ошибка парсинга: {err}"
    
    match errorMsg with
    | Some msg -> 
        printfn "%s" msg
        None
    | None ->
        match mainExpr with
        | Some expr -> 
            Some { Definitions = definitions.Value; MainExpression = expr }
        | None ->
            printfn "Не найдено основное выражение"
            None
