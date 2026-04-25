// <copyright file="${FileName}" author="bogdanovaarina">
// under MIT License
// </copyright>

module Parser.Interpreter
open Parser

// Генерация свежего имени для альфа-конверсии
let rec freshName hint used =
    if not (Set.contains hint used) then hint
    else freshName (hint + "'") used

// Сбор свободных переменных в терме
let rec freeVars = function
    | Var x -> Set.singleton x
    | Abs (params', body) -> 
        let paramsSet = Set.ofList params'
        Set.difference (freeVars body) paramsSet
    | App (t1, t2) -> Set.union (freeVars t1) (freeVars t2)

// Подстановка replacement вместо переменной x в терме term
let rec subst term x replacement =
    match term with
    | Var y when x = y -> replacement
    | Var _ -> term
    | App (t1, t2) -> App (subst t1 x replacement, subst t2 x replacement)
    
    | Abs (params', body) ->
        if List.contains x params' then
            term
        else
            let fvReplacement = freeVars replacement
            let conflictParams = List.filter (fun p -> Set.contains p fvReplacement) params'
            
            if List.isEmpty conflictParams then
                Abs (params', subst body x replacement)
            else
                let used = Set.union (freeVars body) fvReplacement
                let renameMap = 
                    conflictParams 
                    |> List.map (fun p -> p, freshName p used)
                    |> Map.ofList
                
                let renameParam p = Map.tryFind p renameMap |> Option.defaultValue p
                let renamedParams = List.map renameParam params'
                
                let rec renameInTerm term =
                    match term with
                    | Var y -> 
                        let newY = renameParam y
                        if newY <> y then Var newY else term
                    | Abs (ps, b) -> 
                        let renamedPs = List.map renameParam ps
                        Abs (renamedPs, renameInTerm b)
                    | App (t1, t2) -> App (renameInTerm t1, renameInTerm t2)
                
                let renamedBody = renameInTerm body
                Abs (renamedParams, subst renamedBody x replacement)

// Один шаг бета-редукции
let rec reduce = function
    | App (Abs (x::params', body), arg) ->
        let newBody = subst body x arg
        if List.isEmpty params' then
            newBody
        else
            Abs (params', newBody)
    
    | App (f, n) ->
        let f' = reduce f
        if f' <> f then 
            App (f', n)
        else
            let n' = reduce n
            App (f, n')
    
    | Abs (params', body) ->
        let body' = reduce body
        Abs (params', body')
    
    | v -> v

// Нормализация
let normalize maxSteps term =
    let rec loop current step =
        if step >= maxSteps then None
        else
            let next = reduce current
            if next = current then Some current
            else loop next (step + 1)    
    loop term 0

// Подстановка именованных определений в выражение
let rec substituteDefinitions term definitions =
    match term with
    | Var name ->
        match Map.tryFind name definitions with
        | Some defTerm -> defTerm
        | None -> Var name
    | Abs (params', body) -> 
        Abs (params', substituteDefinitions body definitions)
    | App (t1, t2) -> 
        App (substituteDefinitions t1 definitions, substituteDefinitions t2 definitions)

// В строку
let rec toString = function
    | Var x -> x
    | Abs (params', body) ->
        let paramsStr = String.concat " " params'
        $"(\\{paramsStr}.{toString body})"
    | App (t1, t2) ->
        let t1Str = 
            match t1 with
            | Abs _ -> $"({toString t1})"
            | _ -> toString t1
        let t2Str = 
            match t2 with
            | Var _ -> toString t2
            | _ -> $"({toString t2})"
        $"{t1Str} {t2Str}"