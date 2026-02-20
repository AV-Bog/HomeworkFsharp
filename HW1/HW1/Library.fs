module HW1

let factorial x : bigint=
    if x <= 0 then 1I
    else
        let rec recFactorial x acc =
            if x = 1 then acc
            else recFactorial (x-1) (bigint x * acc)
        recFactorial x 1I
        
let nmFunction n m=
    let acc = pown 2I n
    List.scan (fun acc _ -> acc * 2I) acc [0 .. m-1]

let firstOccurrence x list =
    let rec recFirst acc list =
        match list with
        | [] -> None
        | head :: tail ->
            if head = x then Some acc
            else recFirst (acc+1) tail
    recFirst 0 list

let reverse list =
    let rec recRev acc list =
        match list with
        | [] -> acc
        | head :: tail -> recRev (head :: acc) tail
    recRev [] list
    
let fibonacci n : option<bigint> =
    if n < 0 then None
    else
        let rec fibLoop a b count =
            if count = 0 then a
            else fibLoop b (a + b) (count - 1)
        Some (fibLoop 0I 1I n)