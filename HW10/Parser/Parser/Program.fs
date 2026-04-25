module Program

// <copyright file="${FileName}" author="bogdanovaarina">
// under MIT License
// </copyright>

open Parser
open Interpreter
open System.IO

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "Использование: program.exe <файл|выражение>"
        printfn ""
        printfn "Примеры:"
        printfn "  program.exe input.txt"
        printfn "  program.exe \"let S = \\\\x y z.x z (y z)\""
        printfn "  program.exe \"let S = \\\\x y z.x z (y z)\nlet K = \\\\x y.x\nS K K\""
        1
    else
        let input =
            let arg = argv[0]
            if File.Exists(arg) then
                File.ReadAllText(arg)
            else
                arg
        
        printfn "Входные данные:"
        printfn "%s" input
        printfn ""
        printfn "---"
        printfn ""
        
        match Parser.parseInput input with
        | None ->
            printfn "ОШИБКА: Не удалось разобрать входные данные"
            1
        | Some parseResult ->
            printfn "Найдено определений: %d" (Map.count parseResult.Definitions)
            if Map.count parseResult.Definitions > 0 then
                printfn "Определения:"
                for kv in parseResult.Definitions do
                    printfn "  %s = %s" kv.Key (toString kv.Value)
                printfn ""
            
            printfn "Основное выражение: %s" (toString parseResult.MainExpression)
            printfn ""
            printfn "---"
            printfn ""
            
            let substituted = substituteDefinitions parseResult.MainExpression parseResult.Definitions
            
            match normalize 1000 substituted with
            | Some normalized ->
                printfn "Результат: %s" (toString normalized)
                0
            | None ->
                printfn "ПРЕДУПРЕЖДЕНИЕ: Достигнут лимит шагов редукции"
                printfn "Результат (не до конца нормализован): %s" (toString substituted)
                0