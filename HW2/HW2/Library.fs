namespace HW2

module Functions =
    let isPrime n =
        let rec check divisor =
            if divisor * divisor > n then true
            elif n % divisor = 0 then false
            else check (divisor + 1)
        if n < 2 then false
        else check 2
    
    let primeNumbers ()=
        let rec loop a =
            if isPrime a then
                seq {
                    yield a
                    yield! loop (a + 1)
                }
            else
                loop (a + 1)
        loop 2

    type Expression =
        | Number of float
        | BinarOp of op: string * left: Expression * right: Expression
        | UnoOp of op: string * operand: Expression
        
    let rec evaluate (exp : Expression) =
        match exp with
        | Number a -> a
        | BinarOp (op, left, right) ->
            let leftVal = evaluate left
            let rightVal = evaluate right
            
            match op with
            |"+" -> leftVal + rightVal
            | "-" -> leftVal - rightVal
            | "*" -> leftVal * rightVal
            | "/" -> 
                if rightVal = 0.0 then
                    failwith "Деление на ноль!"
                else
                    leftVal / rightVal
            | "^" -> pown leftVal (int rightVal)
            | "%" -> leftVal % rightVal
            | _ -> failwith $"Неизвестная операция: {op}"
        | UnoOp (op, operand) ->
            let operandVal = evaluate operand
            
            match op with
            | "sqrt" -> sqrt operandVal
            | "abs" -> abs operandVal
            | "sin" -> sin operandVal
            | "cos" -> cos operandVal
            | _ -> failwith $"Неизвестная унарная операция: {op}"

    
    type BinTree<'T> =
       | Empty
       | Node of value: 'T * leftSon: BinTree<'T> * rightSon: BinTree<'T>
    
    let rec mapForFree tree func =
            match tree with
            | Empty -> Empty
            | Node(value, leftSon, rightSon) ->
                Node(func value, mapForFree leftSon func, mapForFree rightSon func)
            
            
    let countEvensFilter number =
        number |> List.filter (fun x -> x % 2 = 0) |> List.length
    
    let countEvensMap number =
        number |> List.map (fun x -> if x%2 = 0 then 1 else 0) |> List.sum
        
    let countEvensFold number =
        number |> List.fold (fun acc x -> if x % 2 = 0 then (acc + 1) else acc) 0
    
    