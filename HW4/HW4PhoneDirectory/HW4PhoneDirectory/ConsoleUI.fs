module HW4PhoneDirectory.ConsoleUI

open System
open HW4PhoneDirectory.BusinessLogic

let private readLine (prompt: string) : string =
    Console.Write(prompt)
    Console.ReadLine()

let private printResult = function
    | Ok value -> 
        match box value with
        | :? string as msg -> printfn $"✓ %s{msg}"
        | :? unit -> printfn "✓ Операция выполнена успешно"
        | _ -> printfn $"✓ %A{value}"
    | Error msg -> printfn $"✗ Ошибка: %s{msg}"

let private printRecords (records: Record list) =
    if List.isEmpty records then
        printfn "Справочник пуст"
    else
        printfn $"\nЗаписи в справочнике (%d{List.length records}):"
        records |> List.iteri (fun i { Name = Name n; Phone = Phone p } ->
            printfn $"  %d{i + 1}. %s{n}: %s{p}")
        printfn ""

let private handleAdd (book: PhoneBook) : PhoneBook =
    let name = readLine "Введите имя: "
    let phone = readLine "Введите телефон: "
    match addRecord name phone book with
    | Ok newBook ->
        printfn $"Контакт '%s{name}' добавлен"
        newBook
    | Error msg ->
        printfn $"Не удалось добавить: %s{msg}"
        book

let private handleFindPhone (book: PhoneBook) : PhoneBook =
    let name = readLine "Введите имя для поиска: "
    match findPhoneByName name book with
    | Some phone -> printfn $"Телефон: %s{phone}"
    | None -> printfn $"Контакт '%s{name}' не найден"
    book

let private handleFindName (book: PhoneBook) : PhoneBook =
    let phone = readLine "Введите телефон для поиска: "
    match findNameByPhone phone book with
    | Some name -> printfn $"Имя: %s{name}"
    | None -> printfn $"Телефон '%s{phone}' не найден"
    book

let private handleShowAll (book: PhoneBook) : PhoneBook =
    printRecords (getAllRecords book)
    book

let private handleSave (book: PhoneBook) : PhoneBook =
    let path = readLine "Путь к файлу для сохранения: "
    match saveToFile path book with
    | Ok () -> printfn $"Данные сохранены в '%s{path}'"
    | Error msg -> printfn $"Ошибка сохранения: %s{msg}"
    book

let private handleLoad (book: PhoneBook) : PhoneBook =
    let path = readLine "Путь к файлу для загрузки: "
    match loadFromFile path with
    | Ok loadedBook ->
        printfn $"Загружено %d{recordCount loadedBook} записей"
        loadedBook
    | Error msg ->
        printfn $"Ошибка загрузки: %s{msg}"
        book

let private showShortMenu () =
    printfn "\nДоступные команды:"
    printfn "  add    - добавить запись"
    printfn "  find   - найти телефон по имени"
    printfn "  findp  - найти имя по телефону"
    printfn "  all    - показать все записи"
    printfn "  save   - сохранить в файл"
    printfn "  load   - загрузить из файла"
    printfn "  menu   - показать это меню"
    printfn "  exit   - выход"

let rec mainLoop (book: PhoneBook) : unit =
    printf "> "
    match Console.ReadLine() with
    | null | "exit" -> 
        printfn "До свидания!"
    | "menu" ->
        showShortMenu()
        mainLoop book
    | "add" ->
        mainLoop (handleAdd book)
    | "find" ->
        mainLoop (handleFindPhone book)
    | "findp" ->
        mainLoop (handleFindName book)
    | "all" ->
        mainLoop (handleShowAll book)
    | "save" ->
        mainLoop (handleSave book)
    | "load" ->
        mainLoop (handleLoad book)
    | "" ->
        mainLoop book
    | _ ->
        printfn "Неизвестная команда. Введите 'menu' для списка команд"
        mainLoop book