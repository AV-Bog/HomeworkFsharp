module HW4PhoneDirectory.ConsoleUI

open System
open HW4PhoneDirectory.BusinessLogic

let private readLine (prompt: string) : string =
    Console.Write(prompt)
    Console.ReadLine()

let private printResult = function
    | Success value -> 
        match (value :> obj) with
        | :? string as msg -> printfn "✓ %s" msg
        | :? unit -> printfn "✓ Операция выполнена успешно"
        | _ -> printfn "✓ %A" value
    | Error msg -> printfn "✗ Ошибка: %s" msg

let private printRecords (records: Record list) =
    if List.isEmpty records then
        printfn "Справочник пуст"
    else
        printfn "\n Записи в справочнике (%d):" (List.length records)
        records |> List.iteri (fun i { Name = Name n; Phone = Phone p } -> 
            printfn "  %d. %s: %s" (i + 1) n p)
        printfn ""

let private handleAdd (book: PhoneBook) : PhoneBook =
    let name = readLine "Введите имя: "
    let phone = readLine "Введите телефон: "
    match addRecord name phone book with
    | Success newBook -> 
        printfn "Контакт '%s' добавлен" name
        newBook
    | Error msg -> 
        printfn "Не удалось добавить: %s" msg
        book

let private handleFindPhone (book: PhoneBook) : PhoneBook =
    let name = readLine "Введите имя для поиска: "
    match findPhoneByName name book with
    | Success phone -> printfn "Телефон: %s" phone
    | Error msg -> printfn "%s" msg
    book

let private handleFindName (book: PhoneBook) : PhoneBook =
    let phone = readLine "Введите телефон для поиска: "
    match findNameByPhone phone book with
    | Success name -> printfn "Имя: %s" name
    | Error msg -> printfn "%s" msg
    book

let private handleShowAll (book: PhoneBook) : PhoneBook =
    printRecords (getAllRecords book)
    book

let private handleSave (book: PhoneBook) : PhoneBook =
    let path = readLine "Путь к файлу для сохранения: "
    match saveToFile path book with
    | Success () -> printfn "Данные сохранены в '%s'" path
    | Error msg -> printfn "Ошибка сохранения: %s" msg
    book

let private handleLoad (book: PhoneBook) : PhoneBook =
    let path = readLine "Путь к файлу для загрузки: "
    match loadFromFile path with
    | Success loadedBook -> 
        printfn "Загружено %d записей" (recordCount loadedBook)
        loadedBook
    | Error msg -> 
        printfn "Ошибка загрузки: %s" msg
        book

let private showMenu () : int option =
    printfn "\n===  Телефонный справочник ==="
    printfn "1. Добавить запись"
    printfn "2. Найти телефон по имени"
    printfn "3. Найти имя по телефону"
    printfn "4. Показать все записи"
    printfn "5. Сохранить в файл"
    printfn "6. Загрузить из файла"
    printfn "0. Выйти"
    printf "Ваш выбор: "
    
    match Console.ReadLine() with
    | null -> None
    | input -> 
        match Int32.TryParse(input) with
        | (true, value) when value >= 0 && value <= 6 -> Some value
        | _ -> 
            printfn "Введите число от 0 до 6"
            None

let rec mainLoop (book: PhoneBook) : unit =
    match showMenu() with
    | None | Some 0 -> 
        printfn "До свидания!"
    | Some 1 -> mainLoop (handleAdd book)
    | Some 2 -> mainLoop (handleFindPhone book)
    | Some 3 -> mainLoop (handleFindName book)
    | Some 4 -> mainLoop (handleShowAll book)
    | Some 5 -> mainLoop (handleSave book)
    | Some 6 -> mainLoop (handleLoad book)
    | Some _ -> 
        printfn "Неверная команда"
        mainLoop book
