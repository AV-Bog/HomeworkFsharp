module Tests

open NUnit.Framework
open FsUnit
open HW1

[<Test>]
let ``Factorial of 0 should be 1`` () =
    factorial 0 |> should equal 1I

[<Test>]
let ``Factorial of 20 should be large number`` () =
    factorial 20 |> should equal 2432902008176640000I


[<Test>]
let ``generatePowersOfTwo 0 0 should return single element`` () =
    match generatePowersOfTwo 0 0 with
    | Ok result -> result |> should equal [1.0]
    | Error _ -> Assert.Fail "Expected Ok, got Error"

[<Test>]
let ``generatePowersOfTwo 0 5 should start from 1`` () =
    match generatePowersOfTwo 0 5 with
    | Ok result -> result |> should equal [1.0; 2.0; 4.0; 8.0; 16.0; 32.0]
    | Error _ -> Assert.Fail "Expected Ok, got Error"

[<Test>]
let ``generatePowersOfTwo 3 4 should return powers from 8 to 128`` () =
    match generatePowersOfTwo 3 4 with
    | Ok result -> result |> should equal [8.0; 16.0; 32.0; 64.0; 128.0]
    | Error _ -> Assert.Fail "Expected Ok, got Error"

[<Test>]
let ``generatePowersOfTwo 1 0 should return single element 2`` () =
    match generatePowersOfTwo 1 0 with
    | Ok result -> result |> should equal [2.0]
    | Error _ -> Assert.Fail "Expected Ok, got Error"

[<Test>]
let ``generatePowersOfTwo -1 3 should handle negative n`` () =
    match generatePowersOfTwo -1 3 with
    | Ok result -> result |> should equal [0.5; 1.0; 2.0; 4.0]
    | Error _ -> Assert.Fail "Expected Ok, got Error"
    
[<Test>]
let ``generatePowersOfTwo with negative m should return Error`` () =
    match generatePowersOfTwo 2 -3 with
    | Ok _ -> Assert.Fail "Expected Error, got Ok"
    | Error msg -> msg |> should equal "m не может быть отрицательным"


[<Test>]
let ``firstOccurrence in empty list should be None`` () =
    firstOccurrence 5 [] |> should equal None

[<Test>]
let ``firstOccurrence at second position should be Some 1`` () =
    firstOccurrence 2 [1; 2; 3] |> should equal (Some 1)


[<Test>]
let ``Reverse of multiple elements`` () =
    reverse [1; 2; 3; 4] |> should equal [4; 3; 2; 1]

[<Test>]
let ``Fibonacci of negative should be None`` () =
    fibonacci -1 |> should equal None

[<Test>]
let ``Fibonacci of 10 should be 55`` () =
    fibonacci 10 |> should equal (Some 55I)