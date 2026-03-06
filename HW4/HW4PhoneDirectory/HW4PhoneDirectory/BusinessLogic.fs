module HW4PhoneDirectory.BusinessLogic

open System.IO

type Name = Name of string
type Phone = Phone of string

type Record = {
    Name: Name
    Phone: Phone
}

type PhoneBook = Record list

type OperationResult<'T> = 
    | Success of 'T
    | Error of string

let addRecord (name: string) (phone: string) (book: PhoneBook) : OperationResult<PhoneBook> =
    if name.Trim() = "" then
        Error "Имя не может быть пустым"
    elif phone.Trim() = "" then
        Error "Телефон не может быть пустым"
    elif book |> List.exists (fun r -> r.Name = Name name) then
        Error $"Контакт '{name}' уже существует"
    else
        let newRecord = { Name = Name name; Phone = Phone phone }
        Success (newRecord :: book)

let findPhoneByName (name: string) (book: PhoneBook) : OperationResult<string> =
    match book |> List.tryFind (fun r -> r.Name = Name name) with
    | Some record -> 
        let (Phone phone) = record.Phone
        Success phone
    | None -> 
    Error $"Контакт '{name}' не найден"

let findNameByPhone (phone: string) (book: PhoneBook) : OperationResult<string> =
    match book |> List.tryFind (fun r -> r.Phone = Phone phone) with
    | Some record -> 
        let (Name name) = record.Name
        Success name
    | None -> 
        Error $"Телефон '{phone}' не найден"

let getAllRecords (book: PhoneBook) : Record list = book

let recordCount (book: PhoneBook) : int = List.length book

let private serializeRecord ({ Name = Name name; Phone = Phone phone }: Record) : string = $"{name}|{phone}"

let private deserializeRecord (line: string) : OperationResult<Record> =
    let parts = line.Split('|')
    if parts.Length <> 2 then
        Error $"Неверный формат строки: {line}"
    else
        Success { Name = Name parts[0]; Phone = Phone parts[1] }

let saveToFile (filePath: string) (book: PhoneBook) : OperationResult<unit> =
    try
        let content = book |> List.map serializeRecord |> String.concat "\n"
        File.WriteAllText(filePath, content)
        Success ()
    with
    | ex -> Error $"Ошибка записи файла: {ex.Message}"

let loadFromFile (filePath: string) : OperationResult<PhoneBook> =
    if not (File.Exists(filePath)) then
        Success []
    else
        try
            let lines = File.ReadAllLines(filePath)
            let results = lines |> Array.map deserializeRecord
            
            if results |> Array.exists (function Error _ -> true | _ -> false) then
                let errors = results |> Array.choose (function Error e -> Some e | _ -> None)
                Error (sprintf "Ошибки парсинга: %s" (String.concat "; " errors))
            else
                let records = results |> Array.choose (function Success r -> Some r | _ -> None)
                Success (List.ofArray records)
        with
            | ex -> Error $"Ошибка чтения файла: {ex.Message}"