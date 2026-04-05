module HW1

let factorial x : bigint =
    if x <= 0 then 1I
    else
        let rec recFactorial x acc =
            if x = 1 then acc
            else recFactorial (x-1) (bigint x * acc)
        recFactorial x 1I
        
let generatePowersOfTwo n m =
    let rec loop acc count result =
        if count > m then 
            List.rev result
        else
            let newAcc = if count = 0 then acc else acc * 2.0
            loop newAcc (count + 1) (newAcc :: result)
    
    let startValue = 2.0 * n
    loop startValue 0 []

let firstOccurrence x list =
    let rec recFirst acc list =
        match list with
        | [] -> None
        | head :: tail ->
            | head :: _ when head = x -> Some acc
            | _ :: tail -> recFirst (acc+1) tail
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
        let rec fibLoop = function
            | 0, prev, _ -> prev
            | count, prev, cur -> fibLoop (count - 1, cur, prev + cur)
        Some (fibLoop (n, 0I, 1I))