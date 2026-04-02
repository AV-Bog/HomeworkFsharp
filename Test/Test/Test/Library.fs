// <copyright file="Library.fs" author="bogdanovaarina">
// under MIT License
// </copyright>

/// КР
module task1

/// Finds the minimum element in a list
let findMin list=
    match list with
    | [] -> None
    | _ -> Some (List.reduce min list)
    
/// Builds a square frame of the specified size
let buildKvadrat n =
    let isGraniza row col = row = 0 || row = n - 1 || col = 0 || col = n - 1
    let charForCletca row col = if isGraniza row col then '*' else ' '
    
    if n <= 0 then Seq.empty
    else
        seq { for row in 0 .. n - 1 do 
              let line = seq { for col in 0 .. n - 1 do charForCletca row col }
                          |> System.String.Concat
              yield line }

/// Prints the square frame to the screen
let printKvadrat n =
    buildKvadrat n |> Seq.iter (printfn "%s")

/// Hash table class
type HashTable<'k, 'v when 'k : equality>(hashFunc: 'k -> int, capacity: int) =
    /// Current number of elements in the table
    let mutable size = 0
    /// Array of keys
    let keys: option<'k> array = Array.init capacity (fun _ -> None)
    /// Array of values corresponding to keys by index
    let values: 'v array = Array.zeroCreate capacity
    
    /// Searches for an index for a key
    let findIndex key =
        let startIdx = abs (hashFunc key) % capacity
        let mutable i = startIdx
        
        while keys[i].IsSome && keys[i].Value <> key do
            i <- (i + 1) % capacity
            if i = startIdx then failwith "Hash table is full"
        
        i
        
    /// Adds or updates a key-value pair in the hash table
    member _.Add(key, value) =
        let i = findIndex key
        if keys[i].IsNone then size <- size + 1
        keys[i] <- Some key
        values[i] <- value
    
    /// Checks whether the key exists in the hash table
    member _.Contains(key) =
        let i = findIndex key
        keys[i].IsSome && keys[i].Value = key
    
    /// Removes an element by key from the hash table
    member _.Remove(key) =
        let i = findIndex key
        if keys[i].IsSome && keys[i].Value = key then
            keys[i] <- None
            size <- size - 1
            true
        else false
    
    /// Returns the number of elements in the hash table
    member _.Count = size