module Tests

open HW4Point_free.Transformations
open NUnit.Framework

[<TestFixture>]
module FunctionTests =
    
    [<FsCheck.NUnit.Property>]
    let ``func1 equals func'1`` (x: int, l: int list) =
        func x l = func'1 x l
    
    [<FsCheck.NUnit.Property>]
    let ``func'1 equals func'2`` (x: int, l: int list) =
        func'1 x l = func'2 x l
    
    [<FsCheck.NUnit.Property>]
    let ``func'2 equals func'3`` (x: int, l: int list) =
        func'2 x l = func'3 x l
    