module TestsHW2

open NUnit.Framework
open FsUnit
open HW2.Functions

[<TestFixture>]
module PrimeTests =

    [<Test>]
    let ``IsPrime 2 should be true`` () =
        isPrime 2 |> should equal true

    [<Test>]
    let ``IsPrime 4 should be false`` () =
        isPrime 4 |> should equal false

    [<Test>]
    let ``IsPrime 1 should be false`` () =
        isPrime 1 |> should equal false

    [<Test>]
    let ``PrimeNumbers first 5 should be correct`` () =
        primeNumbers () |> Seq.take 5 |> Seq.toList |> should equal [2; 3; 5; 7; 11]


[<TestFixture>]
module ExpressionTests =

    [<Test>]
    let ``Evaluate Number 42 should return 42`` () =
        Number 42.0 |> evaluate |> should equal 42.0

    [<Test>]
    let ``Evaluate (2 + 3) * 4 should return 20`` () =
        BinarOp("*", BinarOp("+", Number 2.0, Number 3.0), Number 4.0) 
        |> evaluate |> should equal 20.0

    [<Test>]
    let ``Evaluate sqrt(16) should return 4`` () =
        UnoOp("sqrt", Number 16.0) |> evaluate |> should equal 4.0

    [<Test>]
    let ``Evaluate division by zero should throw`` () =
        (fun () -> BinarOp("/", Number 10.0, Number 0.0) |> evaluate |> ignore) 
        |> should throw typeof<exn>


[<TestFixture>]
module BinTreeTests =

    [<Test>]
    let ``MapForFree should double all values`` () =
        let tree = Node(5, Node(3, Empty, Empty), Node(7, Empty, Empty))
        let result = mapForFree tree (fun x -> x * 2)
        
        match result with
        | Node(v, Node(l, _, _), Node(r, _, _)) ->
            v |> should equal 10
            l |> should equal 6
            r |> should equal 14
        | _ -> failwith "Wrong structure"

    [<Test>]
    let ``MapForFree should not mutate original tree`` () =
        let tree = Node(5, Empty, Empty)
        let _ = mapForFree tree (fun x -> x * 100)
        tree |> should equal (Node(5, Empty, Empty))


[<TestFixture>]
module CountEvensTests =

    [<Test>]
    let ``countEvensFilter [1..10] should return 5`` () =
        countEvensFilter [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] |> should equal 5

    [<Test>]
    let ``countEvensMap empty list should return 0`` () =
        countEvensMap [] |> should equal 0

    [<Test>]
    let ``countEvensFold all evens should count all`` () =
        countEvensFold [2; 4; 6; 8; 10] |> should equal 5

    [<Test>]
    let ``All three countEvens should return same result`` () =
        let list = [1; 2; 3; 4; 5; 6]
        let filter = countEvensFilter list
        let map = countEvensMap list
        let fold = countEvensFold list
        filter |> should equal map
        map |> should equal fold
    
    [<FsCheck.NUnit.Property>]
    let ``All countEvens functions should be equivalent`` (numbers: int list) =
        let filter = countEvensFilter numbers
        let map = countEvensMap numbers
        let fold = countEvensFold numbers
        filter = map && map = fold

    [<FsCheck.NUnit.Property>]
    let ``countEvensFilter result should be between 0 and list length`` (numbers: int list) =
        let result = countEvensFilter numbers
        result >= 0 && result <= List.length numbers

    [<FsCheck.NUnit.Property>]
    let ``countEvens on empty list should be 0`` () =
        let empty : int list = []
        countEvensFilter empty = 0 
        && countEvensMap empty = 0 
        && countEvensFold empty = 0
        
    [<FsCheck.NUnit.Property>]
    let ``countEvens on single even should be 1`` (even: int) =
        let evenNum = even * 2
        countEvensFilter [evenNum] = 1
        && countEvensMap [evenNum] = 1
        && countEvensFold [evenNum] = 1

    [<FsCheck.NUnit.Property>]
    let ``countEvens on single odd should be 0`` (odd: int) =
        let oddNum = odd * 2 + 1
        countEvensFilter [oddNum] = 0
        && countEvensMap [oddNum] = 0
        && countEvensFold [oddNum] = 0

    [<FsCheck.NUnit.Property>]
    let ``countEvens should be additive for concatenated lists`` (list1: int list) (list2: int list) =
        let count1 = countEvensFilter list1
        let count2 = countEvensFilter list2
        let countCombined = countEvensFilter (list1 @ list2)
        countCombined = count1 + count2

    [<FsCheck.NUnit.Property>]
    let ``countEvens should handle negative numbers correctly`` (numbers: int list) =
        countEvensFilter numbers = countEvensFilter (List.map abs numbers)