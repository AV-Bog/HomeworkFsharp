module task1

let findMin list=
    match list with
    | [] -> None
    | _ -> Some (List.reduce min list)
    
let buildKvadrat n =
    let isGraniza row col = row = 0 || row = n - 1 || col = 0 || col = n - 1
    let charForCletca row col = if isGraniza row col then '*' else ' '
    
    if n <= 0 then Seq.empty
    else
        seq { for row in 0 .. n - 1 do 
              let line = seq { for col in 0 .. n - 1 do charForCletca row col }
                          |> System.String.Concat
              yield line }

let printKvadrat n =
    buildKvadrat n |> Seq.iter (printfn "%s")
        
type HashTable<'k, 'v when 'k : equality>(hashFunc: 'k -> int, capacity: int) =
    let mutable size = 0
    let keys: option<'k> array = Array.init capacity (fun _ -> None)
    let values: 'v array = Array.zeroCreate capacity
    
    let findIndex key =
        let startIdx = abs (hashFunc key) % capacity
        let mutable i = startIdx
        
        while keys.[i].IsSome && keys.[i].Value <> key do
            i <- (i + 1) % capacity
            if i = startIdx then failwith "Hash table is full"
        
        i
        
    member _.Add(key, value) =
        let i = findIndex key
        if keys.[i].IsNone then size <- size + 1
        keys.[i] <- Some key
        values.[i] <- value
    
    member _.Contains(key) =
        let i = findIndex key
        keys.[i].IsSome && keys.[i].Value = key
    
    member _.Remove(key) =
        let i = findIndex key
        if keys.[i].IsSome && keys.[i].Value = key then
            keys.[i] <- None
            size <- size - 1
            true
        else false
    
    member _.Count = size