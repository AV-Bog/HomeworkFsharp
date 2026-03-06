namespace HW4Staples

module Staples =
    let onlyStaples (str : string)=
        str
        |> Seq.filter (fun c -> "()[]{}".Contains(c))
        |> System.String.Concat
    
    let isMatchingPair openBr closeBr =
        match openBr, closeBr with
        | '(', ')' | '[', ']' | '{', '}' -> true
        | _ -> false
        
    let rec checkStack (stack: char list) (remaining: string) =
        if System.String.IsNullOrEmpty(remaining) then
            stack.IsEmpty
        else
            let current = remaining.[0]
            let rest = remaining.Substring(1)
            
            match current with
            | '(' | '[' | '{' -> checkStack (current :: stack) rest
            | ')' | ']' | '}' ->
                match stack with
                | [] -> false 
                | top :: restStack ->
                    if isMatchingPair top current then
                        checkStack restStack rest
                    else
                        false
            | _ -> checkStack stack rest
    
    let okStaples (str : string) =
        let bracketsOnly = onlyStaples str
        checkStack [] bracketsOnly