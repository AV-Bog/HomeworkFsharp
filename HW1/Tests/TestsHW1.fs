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
    generatePowersOfTwo 0 0 |> should equal [1.0]

[<Test>]
let ``generatePowersOfTwo 0 5 should start from 1`` () =
    generatePowersOfTwo 0 5 |> should equal [1.0; 2.0; 4.0; 8.0; 16.0; 32.0]

[<Test>]
let ``generatePowersOfTwo 3 4 should return powers from 8 to 128`` () =
    generatePowersOfTwo 3 4 |> should equal [8.0; 16.0; 32.0; 64.0; 128.0]

[<Test>]
let ``generatePowersOfTwo 1 0 should return single element 2`` () =
    generatePowersOfTwo 1 0 |> should equal [2.0]

[<Test>]
let ``generatePowersOfTwo -1 3 should handle negative n`` () =
    generatePowersOfTwo -1 3 |> should equal [0.5; 1.0; 2.0; 4.0]
    
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