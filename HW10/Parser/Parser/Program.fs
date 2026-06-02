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
            
            printfn "Найдено выражений: %d" parseResult.Expressions.Length
            printfn ""
            printfn "---"
            printfn ""
            
            // Обрабатываем каждое выражение
            let mutable hasError = false
            for i, exprResult in Seq.indexed parseResult.Expressions do
                printfn "Выражение %d: %s" (i + 1) (toString exprResult.Expression)
                printfn ""
                
                let substituted = substituteDefinitions exprResult.Expression parseResult.Definitions
                
                match normalize 1000 substituted with
                | Some normalized ->
                    printfn "Результат %d: %s" (i + 1) (toString normalized)
                | None ->
                    printfn "ПРЕДУПРЕЖДЕНИЕ: Достигнут лимит шагов редукции для выражения %d" (i + 1)
                    printfn "Результат %d (не до конца нормализован): %s" (i + 1) (toString substituted)
                    hasError <- true
                
                printfn ""
                if i < parseResult.Expressions.Length - 1 then
                    printfn "---"
                    printfn ""
            
            if hasError then 1 else 0