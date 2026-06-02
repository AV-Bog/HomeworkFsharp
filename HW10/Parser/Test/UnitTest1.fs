module Test

open NUnit.Framework
open Parser.Parser
open FParsec

[<SetUp>]
let Setup () =
    ()

let parseTerm (input: string) =
    match run term input with
    | Success(result, _, _) -> Some result
    | Failure(_, _, _) -> None

let parseLetDefinition (input: string) =
    match run letDefinition input with
    | Success(result, _, _) -> Some result
    | Failure(_, _, _) -> None

// Тесты для term
[<Test>]
let ``term должен парсить лямбда-абстракцию`` () =
    let result = parseTerm "\\x.x"
    match result with
    | Some(Abs(["x"], Var("x"))) -> Assert.Pass()
    | _ -> Assert.Fail("Не удалось распарсить абстракцию")

[<Test>]
let ``term должен парсить лямбда-абстракцию с несколькими параметрами`` () =
    let result = parseTerm "\\x y.x y"
    match result with
    | Some(Abs(["x"; "y"], App(Var("x"), Var("y")))) -> Assert.Pass()
    | _ -> Assert.Fail("Не удалось распарсить абстракцию с несколькими параметрами")

// Тесты для аппликации
[<Test>]
let ``term должен парсить аппликацию`` () =
    let result = parseTerm "x y"
    match result with
    | Some(App(Var("x"), Var("y"))) -> Assert.Pass()
    | _ -> Assert.Fail("Не удалось распарсить аппликацию")

[<Test>]
let ``term должен парсить вложенные аппликации`` () =
    let result = parseTerm "x y z"
    match result with
    | Some(App(App(Var("x"), Var("y")), Var("z"))) -> Assert.Pass()
    | _ -> Assert.Fail("Не удалось распарсить вложенную аппликацию")

// Тесты для letDefinition
[<Test>]
let ``letDefinition должен парсить определение`` () =
    let result = parseLetDefinition "let x = y"
    match result with
    | Some(def) ->
        Assert.That(def.Name, Is.EqualTo("x"))
        match def.Body with
        | Var("y") -> Assert.Pass()
        | _ -> Assert.Fail("Тело не является переменной y")
    | None -> Assert.Fail("Не удалось распарсить определение")

[<Test>]
let ``letDefinition должен парсить сложное определение`` () =
    let result = parseLetDefinition "let f = \\x.x"
    match result with
    | Some(def) ->
        Assert.That(def.Name, Is.EqualTo("f"))
        match def.Body with
        | Abs(["x"], Var("x")) -> Assert.Pass()
        | _ -> Assert.Fail("Тело не является абстракцией")
    | None -> Assert.Fail("Не удалось распарсить определение")

// Тесты для parseInput
[<Test>]
let ``parseInput должен парсить определение и выражение`` () =
    let input = "let x = y\nx y"
    match parseInput input with
    | Some(result) ->
        Assert.That(result.Definitions.Count, Is.EqualTo(1))
        Assert.That(result.Definitions.ContainsKey("x"), Is.True)
        Assert.That(result.Expressions.Length, Is.EqualTo(1))
        match result.Expressions.Head.Expression with
        | App(Var("x"), Var("y")) -> Assert.Pass()
        | _ -> Assert.Fail("Неверное выражение")
    | None -> Assert.Fail("Не удалось распарсить входные данные")

[<Test>]
let ``parseInput должен парсить несколько выражений`` () =
    let input = "x\ny\nz"
    match parseInput input with
    | Some(result) ->
        Assert.That(result.Definitions.Count, Is.EqualTo(0))
        Assert.That(result.Expressions.Length, Is.EqualTo(3))
    | None -> Assert.Fail("Не удалось распарсить несколько выражений")

[<Test>]
let ``parseInput должен парсить определения и несколько выражений`` () =
    let input = "let id = \\x.x\nlet const = \\x y.x\nid const\nconst id"
    match parseInput input with
    | Some(result) ->
        Assert.That(result.Definitions.Count, Is.EqualTo(2))
        Assert.That(result.Definitions.ContainsKey("id"), Is.True)
        Assert.That(result.Definitions.ContainsKey("const"), Is.True)
        Assert.That(result.Expressions.Length, Is.EqualTo(2))
    | None -> Assert.Fail("Не удалось распарсить входные данные")

[<Test>]
let ``parseInput должен возвращать None при отсутствии выражений`` () =
    let input = "let x = y\nlet z = w"
    match parseInput input with
    | Some(_) -> Assert.Fail("Не должен был вернуть Some при отсутствии выражений")
    | None -> Assert.Pass()

[<Test>]
let ``parseInput должен обрабатывать пустые строки`` () =
    let input = "\n\n\n"
    match parseInput input with
    | Some(_) -> Assert.Fail("Не должен был вернуть Some для пустого ввода")
    | None -> Assert.Pass()

[<Test>]
let ``parseInput должен игнорировать пробелы`` () =
    let input = "  let x = y  \n  x y  "
    match parseInput input with
    | Some(result) ->
        Assert.That(result.Definitions.Count, Is.EqualTo(1))
        Assert.That(result.Expressions.Length, Is.EqualTo(1))
    | None -> Assert.Fail("Не удалось распарсить с пробелами")

[<Test>]
let ``parseInput должен обрабатывать синтаксические ошибки`` () =
    let input = "let = x\nx y"
    match parseInput input with
    | Some(_) -> Assert.Fail("Не должен был вернуть Some при синтаксической ошибке")
    | None -> Assert.Pass()

[<Test>]
let ``parseInput должен парсить сложные вложенные выражения`` () =
    let input = "let apply = \\f x.f x\napply (\\x.x) y"
    match parseInput input with
    | Some(result) ->
        match result.Expressions.Head.Expression with
        | App(App(Var("apply"), Abs(["x"], Var("x"))), Var("y")) -> Assert.Pass()
        | _ -> Assert.Fail("Неверная структура вложенного выражения")
    | None -> Assert.Fail("Не удалось распарсить сложное выражение")

[<Test>]
let ``parseInput должен парсить комбинацию определений и выражений в любом порядке`` () =
    let input = "x\nlet id = \\x.x\ny\nlet const = \\x y.x\nid y"
    match parseInput input with
    | Some(result) ->
        Assert.That(result.Definitions.Count, Is.EqualTo(2))
        Assert.That(result.Expressions.Length, Is.EqualTo(3))
    | None -> Assert.Fail("Не удалось распарсить смешанный ввод")

[<Test>]
let ``parseInput должен корректно работать с выражениями в скобках`` () =
    let input = "let f = \\x.x\n(f y)"
    match parseInput input with
    | Some(result) ->
        match result.Expressions.Head.Expression with
        | App(Var("f"), Var("y")) -> Assert.Pass()
        | _ -> Assert.Fail("Неверная структура выражения со скобками")
    | None -> Assert.Fail("Не удалось распарсить выражение со скобками")