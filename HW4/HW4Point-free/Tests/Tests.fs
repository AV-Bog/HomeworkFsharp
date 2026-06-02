module Tests

open HW4PointFree.Transformations
open NUnit.Framework

[<TestFixture>]
module FunctionTests =

    let private isEqual f g (x: int) (lst: int list) =
        f x lst = g x lst

    [<FsCheck.NUnit.Property>]
    let ``scaleList equals scaleList1`` (x: int, l: int list) =
        scaleList x l = scaleList1 x l

    [<FsCheck.NUnit.Property>]
    let ``scaleList1 equals scaleList2`` (x: int, l: int list) =
        scaleList1 x l = scaleList2 x l

    [<FsCheck.NUnit.Property>]
    let ``scaleList2 equals scaleList3`` (x: int, l: int list) =
        scaleList2 x l = scaleList3 x l

    [<FsCheck.NUnit.Property>]
    let ``scaleList3 equals scaleListPF`` (x: int, l: int list) =
        scaleList3 x l = scaleListPF x l

    [<FsCheck.NUnit.Property>]
    let ``scaleList equals scaleListPF (transitive)`` (x: int, l: int list) =
        scaleList x l = scaleListPF x l