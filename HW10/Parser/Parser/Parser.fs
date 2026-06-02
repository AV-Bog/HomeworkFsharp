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

type ExpressionResult = {
    Expression: Term
    Result: obj option
}

type ParseResult = {
    Definitions: Map<string, Term>
    Expressions: ExpressionResult list
}

let ws = spaces
let str s = pstring s .>> ws

let identifier : Parser<string, unit> =
    let isFirstChar c = isLetter c
    let isOtherChar c = isLetter c || isDigit c
    (many1Satisfy2L isFirstChar isOtherChar "identifier") .>> ws

let var = identifier |>> Var

let parameters : Parser<string list, unit> =
    many1 identifier

let term, termRef = createParserForwardedToRef<Term, unit>()

let termPrime : Parser<Term, unit> =
    choice [
        str "\\" >>. parameters .>> str "." .>>. term |>> Abs;
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

let parseInput (input: string) : ParseResult option =
    let lines = input.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    let definitions = ref Map.empty
    let expressions = ref []
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
                        expressions.Value <- expressions.Value @ [{ Expression = expr; Result = None }]
                    | Failure(err, _, _) ->
                        errorMsg <- Some $"Ошибка парсинга: {err}"
    
    match errorMsg with
    | Some msg -> 
        printfn "%s" msg
        None
    | None ->
        match expressions.Value with
        | [] -> 
            printfn "Не найдено ни одного выражения"
            None
        | exprs -> 
            Some { Definitions = definitions.Value; Expressions = exprs }