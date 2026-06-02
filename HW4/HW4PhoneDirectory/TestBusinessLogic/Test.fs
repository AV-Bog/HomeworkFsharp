module Test.TestsTest

open HW4PhoneDirectory.BusinessLogic
open Microsoft.FSharp.Core
open NUnit.Framework

[<TestFixture>]
type PhoneBookTests () =
    let emptyBook = PhoneBook []
    
    [<Test>]
    member this.addRecord_WithValidData_AddsRecordSuccessfully () =
        let result = addRecord "John" "123456" emptyBook
        
        match result with
        | Ok book ->
            Assert.That(recordCount book, Is.EqualTo(1))
            let records = getAllRecords book
            Assert.That(records.Length, Is.EqualTo(1))
            let (Name name) = records[0].Name
            let (Phone phone) = records[0].Phone
            Assert.That(name, Is.EqualTo("John"))
            Assert.That(phone, Is.EqualTo("123456"))
        | Error _ -> Assert.Fail("Should return Ok")
    
    [<Test>]
    member this.addRecord_WithEmptyName_ReturnsError () =
        let result = addRecord "" "123456" emptyBook
        
        match result with
        | Error msg -> Assert.That(msg, Is.EqualTo("Имя не может быть пустым"))
        | Ok _ -> Assert.Fail("Should return Error")
    
    [<Test>]
    member this.addRecord_WithWhitespaceName_ReturnsError () =
        let result = addRecord "   " "123456" emptyBook
        
        match result with
        | Error msg -> Assert.That(msg, Is.EqualTo("Имя не может быть пустым"))
        | Ok _ -> Assert.Fail("Should return Error")
    
    [<Test>]
    member this.addRecord_WithEmptyPhone_ReturnsError () =
        let result = addRecord "John" "" emptyBook
        
        match result with
        | Error msg -> Assert.That(msg, Is.EqualTo("Телефон не может быть пустым"))
        | Ok _ -> Assert.Fail("Should return Error")
    
    [<Test>]
    member this.addRecord_WithDuplicateName_ReturnsError () =
        let book = 
            match addRecord "John" "123456" emptyBook with
            | Ok b -> b
            | Error _ -> emptyBook
        
        let result = addRecord "John" "789012" book
        
        match result with
        | Error msg -> Assert.That(msg, Is.EqualTo("Контакт 'John' уже существует"))
        | Ok _ -> Assert.Fail("Should return Error")
    
    [<Test>]
    member this.addRecord_AddsMultipleRecords_Correctly () =
        let book1 = 
            match addRecord "John" "123456" emptyBook with
            | Ok b -> b
            | Error _ -> emptyBook
        
        let book2 = 
            match addRecord "Jane" "789012" book1 with
            | Ok b -> b
            | Error _ -> book1
        
        let book3 = 
            match addRecord "Bob" "345678" book2 with
            | Ok b -> b
            | Error _ -> book2
        
        Assert.That(recordCount book3, Is.EqualTo(3))
        
        let phoneJohn = findPhoneByName "John" book3
        let phoneJane = findPhoneByName "Jane" book3
        let phoneBob = findPhoneByName "Bob" book3
        
        Assert.That(phoneJohn, Is.EqualTo(Some "123456"))
        Assert.That(phoneJane, Is.EqualTo(Some "789012"))
        Assert.That(phoneBob, Is.EqualTo(Some "345678"))
    
    [<Test>]
    member this.findPhoneByName_WithExistingName_ReturnsPhone () =
        let book = 
            match addRecord "John" "123456" emptyBook with
            | Ok b -> b
            | Error _ -> emptyBook
        
        let result = findPhoneByName "John" book
        Assert.That(result, Is.EqualTo(Some "123456"))
    
    [<Test>]
    member this.findPhoneByName_WithNonExistingName_ReturnsNone () =
        let book = 
            match addRecord "John" "123456" emptyBook with
            | Ok b -> b
            | Error _ -> emptyBook
        
        let result = findPhoneByName "Jane" book
        Assert.That(result, Is.EqualTo(None))
    
    [<Test>]
    member this.findNameByPhone_WithExistingPhone_ReturnsName () =
        let book = 
            match addRecord "John" "123456" emptyBook with
            | Ok b -> b
            | Error _ -> emptyBook
        
        let result = findNameByPhone "123456" book
        Assert.That(result, Is.EqualTo(Some "John"))
    
    [<Test>]
    member this.findNameByPhone_WithNonExistingPhone_ReturnsNone () =
        let book = 
            match addRecord "John" "123456" emptyBook with
            | Ok b -> b
            | Error _ -> emptyBook
        
        let result = findNameByPhone "999999" book
        Assert.That(result, Is.EqualTo(None))
    
    [<Test>]
    member this.getAllRecords_ReturnsAllRecords () =
        let book = 
            match addRecord "John" "123456" emptyBook with
            | Ok b -> 
                match addRecord "Jane" "789012" b with
                | Ok b2 -> b2
                | Error _ -> b
            | Error _ -> emptyBook
        
        let records = getAllRecords book
        Assert.That(records.Length, Is.EqualTo(2))
    
    [<Test>]
    member this.recordCount_ReturnsCorrectCount () =
        Assert.That(recordCount emptyBook, Is.EqualTo(0))
        
        let book1 = 
            match addRecord "John" "123456" emptyBook with
            | Ok b -> b
            | Error _ -> emptyBook
        Assert.That(recordCount book1, Is.EqualTo(1))
        
        let book2 = 
            match addRecord "Jane" "789012" book1 with
            | Ok b -> b
            | Error _ -> book1
        Assert.That(recordCount book2, Is.EqualTo(2))
    