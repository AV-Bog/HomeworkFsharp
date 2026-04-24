// <copyright file="${FileName}" author="bogdanovaarina">
// under MIT License
// </copyright>

module Parser.Parser
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
    let isFirstChar c = isLetter c && System.Char.IsLower(c)
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

let letDefinition : Parser<string * Term, unit> =
    str "let" >>. identifier .>> str "=" .>>. term .>> restOfLine true |>> (fun (name, value) -> (name, value))

let parseInput (input: string) : ReplyStatus =
    let lines = input.Split([|'\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
    let definitions = ref Map.empty
    let mutable mainExpr = None
    let mutable error = None
    
    for line in lines do
        if error.IsNone then
            let trimmed = line.Trim()
            if trimmed <> "" then
                match run (letDefinition .>> ws .>> eof) trimmed with
                | Success((name, value), _, _) ->
                    definitions.Value <- Map.add name value definitions.Value
                | _ ->
                    match run (term .>> ws .>> eof) trimmed with
                    | Success(expr, _, _) ->
                        if mainExpr.IsNone then
                            mainExpr <- Some expr
                        else
                            error <- Some "Несколько основных выражений"
                    | Failure(err, _, _) ->
                        error <- Some $"Ошибка парсинга: {err}"
    
    match error with
    | Some msg -> Error
    | None ->
        match mainExpr with
        | Some expr -> Ok
        | None -> Error
        
let rec substituteDefinitions term definitions =
    match term with
    | Var name ->
        match Map.tryFind name definitions with
        | Some defTerm -> defTerm
        | None -> Var name
    | Abs (params, body) -> Abs (params, substituteDefinitions body definitions)
    | App (t1, t2) -> App (substituteDefinitions t1 definitions, substituteDefinitions t2 definitions)   