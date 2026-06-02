namespace HW4PointFree

module Transformations =
    let scaleList x l = List.map (fun y -> y * x) l
    let scaleList1 x = List.map (fun y -> y * x)
    let scaleList2 x = List.map (fun y -> ((*) x) y)
    let scaleList3 x = List.map ((*) x)
    let scaleListPF = List.map << (*)