open HW4PhoneDirectory.ConsoleUI
open HW4PhoneDirectory.BusinessLogic

[<EntryPoint>]
let main argv =
    printfn "Запуск телефонного справочника..."
    
    let initialBook = 
        match loadFromFile "phonebook.dat" with
        | Ok book ->
            printfn $"Загружено %d{recordCount book} записей из phonebook.dat"
            book
        | Error msg ->
            printfn $"%s{msg} (начинаем с пустого справочника)"
            PhoneBook []
    
    mainLoop initialBook
    0