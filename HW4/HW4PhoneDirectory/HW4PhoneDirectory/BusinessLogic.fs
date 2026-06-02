module HW4PhoneDirectory.BusinessLogic

open System.IO

type Name = Name of string
type Phone = Phone of string

type Record = {
    Name: Name
    Phone: Phone
}

type PhoneBook = PhoneBook of Record list

let private getRecords (PhoneBook records) = records
let private createPhoneBook records = PhoneBook records

let addRecord (name: string) (phone: string) (book: PhoneBook) : Result<PhoneBook, string> =
    let records = getRecords book
    
    if name.Trim() = "" then
        Error "Имя не может быть пустым"
    elif phone.Trim() = "" then
        Error "Телефон не может быть пустым"
    elif records |> List.exists (fun r -> r.Name = Name name) then
        Error $"Контакт '{name}' уже существует"
    else
        let newRecord = { Name = Name name; Phone = Phone phone }
        createPhoneBook (newRecord :: records) |> Ok

let findPhoneByName (name: string) (book: PhoneBook) : string option =
    let records = getRecords book
    records |> List.tryFind (fun r -> r.Name = Name name)
    |> Option.map (fun record -> 
        let (Phone phone) = record.Phone
        phone
    )

let findNameByPhone (phone: string) (book: PhoneBook) : string option =
    let records = getRecords book
    records |> List.tryFind (fun r -> r.Phone = Phone phone)
    |> Option.map (fun record -> 
        let (Name name) = record.Name
        name
    )

let getAllRecords (PhoneBook records) : Record list = records

let recordCount (PhoneBook records) : int = List.length records

let private serializeRecord ({ Name = Name name; Phone = Phone phone }: Record) : string = $"{name}|{phone}"

let private deserializeRecord (line: string) : Result<Record, string> =
    match line.Split('|') with
    | [| name; phone |] ->
        Ok { Name = Name name; Phone = Phone phone }
    | _ ->
        Error $"Неверный формат строки: {line}"

let saveToFile (filePath: string) (PhoneBook records) : Result<unit, string> =
    try
        let content = records |> List.map serializeRecord |> String.concat "\n"
        File.WriteAllText(filePath, content)
        Ok ()
    with
    | ex -> Error $"Ошибка записи файла: {ex.Message}"

let loadFromFile (filePath: string) : Result<PhoneBook, string> =
    if not (File.Exists(filePath)) then
        Ok (PhoneBook [])
    else
        try
            let lines = File.ReadAllLines(filePath)
            let results = lines |> Array.map deserializeRecord |> Array.toList
            
            let errors = results |> List.choose (function Error e -> Some e | _ -> None)
            let records = results |> List.choose (function Ok r -> Some r | _ -> None)
            
            if List.length errors > 0 then
                Error (sprintf "Ошибки парсинга: %s" (String.concat "; " errors))
            else
                Ok (PhoneBook records)
        with
        | ex -> Error $"Ошибка чтения файла: {ex.Message}"