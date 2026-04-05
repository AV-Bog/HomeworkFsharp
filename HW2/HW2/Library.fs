namespace HW2

module Functions =
    let isPrime n =
        let rec check divisor =
            if divisor * divisor > n then true
            elif n % divisor = 0 then false
            else check (divisor + 1)
        if n < 2 then false
        else check 2
    
    let primeNumbers () =
        Seq.initInfinite (fun i -> i + 2)
        |> Seq.filter isPrime

    type BinaryOp =
        | Add
        | Subtract
        | Multiply
        | Divide
        | Power
        | Modulo
    
    type UnaryOp =
        | Sqrt
        | Abs
        | Sin
        | Cos
    
    type Expression =
        | Number of float
        | Binary of BinaryOp * Expression * Expression
        | Unary of UnaryOp * Expression
    
    let evaluate (exp : Expression) =
        let rec eval exp =
            match exp with
            | Number a -> a
            | Binary (op, left, right) ->
                let leftVal = eval left
                let rightVal = eval right
                
                match op with
                | Add -> leftVal + rightVal
                | Subtract -> leftVal - rightVal
                | Multiply -> leftVal * rightVal
                | Divide when rightVal = 0.0 -> failwith "Деление на ноль!"
                | Divide -> leftVal / rightVal
                | Power -> pown leftVal (int rightVal)
                | Modulo -> leftVal % rightVal
            | Unary (op, operand) ->
                let operandVal = eval operand
                
                match op with
                | Sqrt -> sqrt operandVal
                | Abs -> abs operandVal
                | Sin -> sin operandVal
                | Cos -> cos operandVal
        eval exp
    
    type BinTree<'T> =
        | Empty
        | Node of value: 'T * leftSon: BinTree<'T> * rightSon: BinTree<'T>
    
    let rec treeMap tree func =
        match tree with
        | Empty -> Empty
        | Node(value, leftSon, rightSon) ->
            Node(func value, treeMap leftSon func, treeMap rightSon func)
        
    let rec areTreesEqual tree1 tree2 =
        match tree1, tree2 with
        | Empty, Empty -> true
        | Node(v1, l1, r1), Node(v2, l2, r2) ->
            v1 = v2 && areTreesEqual l1 l2 && areTreesEqual r1 r2
        | _ -> false
    
    let countEvensFilter numbers =
        numbers |> List.filter (fun x -> x % 2 = 0) |> List.length
    
    let countEvensMap number =
        number |> List.map (fun x -> if x % 2 = 0 then 1 else 0) |> List.sum
        
    let countEvensFold number =
        number |> List.fold (fun acc x -> if x % 2 = 0 then (acc + 1) else acc) 0
    
    