namespace HW4Staples

module Brackets =
    let isMatchingPair openBr closeBr =
        match openBr, closeBr with
        | '(', ')' | '[', ']' | '{', '}' -> true
        | _ -> false
        
    let rec checkStack (stack: char list) (remaining: char list) =
        match remaining with
        | [] -> stack.IsEmpty
        | current :: rest ->
            match current with
            | '(' | '[' | '{' -> 
                checkStack (current :: stack) rest
            | ')' | ']' | '}' ->
                match stack with
                | top :: restStack when isMatchingPair top current ->
                    checkStack restStack rest
                | _ -> false
            | _ -> checkStack stack rest
    
    let public okStaples (str : string) =
        let charList = str |> List.ofSeq
        checkStack [] charList