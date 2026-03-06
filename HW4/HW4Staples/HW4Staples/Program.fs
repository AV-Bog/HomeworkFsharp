open HW4Staples.Staples

[<EntryPoint>]
let main argv =
    let test1 = "(sl[ekek{}wpffri]kd)"
    let test2 = "([)]"  // Неверно
    let test3 = "{[()]}" // Верно
    
    printfn "Тест 1: %b" (okStaples test1)
    printfn "Тест 2: %b" (okStaples test2)
    printfn "Тест 3: %b" (okStaples test3)
    0