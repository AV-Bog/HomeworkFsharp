open HW4PhoneDirectory.ConsoleUI
open HW4PhoneDirectory.BusinessLogic

[<EntryPoint>]
let main argv =
    printfn "Запуск телефонного справочника..."
    
    let initialBook = 
        match loadFromFile "phonebook.dat" with
        | Success book -> 
            printfn "Загружено %d записей из phonebook.dat" (recordCount book)
            book
        | Error msg -> 
            printfn "%s (начинаем с пустого справочника)" msg
            []
    
    mainLoop initialBook
    0