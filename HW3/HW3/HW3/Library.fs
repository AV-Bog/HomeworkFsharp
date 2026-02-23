namespace HW3

type Term =
    | Var of string
    | Abs of string * Term
    | App of Term * Term
    
module Interpreter =
    let rec freshName hint used =
        if not (Set.contains hint used) then hint
        else freshName (hint + "'") used
    
    let rec freeVars = function
        | Var x -> Set.singleton x
        | Abs (x, t) -> freeVars t |> Set.remove x
        | App (t1, t2) -> Set.union (freeVars t1) (freeVars t2)
    
    let rec subst t x replacement =
        match t with
        | Var y -> if x = y then replacement else t
        | App (t1, t2) -> App (subst t1 x replacement, subst t2 x replacement)
        | Abs (y, body) ->
            if y = x then t
            else
                if Set.contains y (freeVars replacement) then
                    let used = Set.union (freeVars body) (freeVars replacement)
                    let z = freshName y used
                    let renamedBody = subst body y (Var z)
                    Abs (z, subst renamedBody x replacement)
                else
                    Abs (y, subst body x replacement)
    
    let rec reduce = function
        | App (Abs (x, body), arg) -> subst body x arg
        | App (f, n) ->
            let f' = reduce f
            if f' <> f then 
                App (f', n)
            else
                let n' = reduce n
                if n' <> n then App (f, n')
                else App (f, n)
        | Abs (x, body) ->
            let body' = reduce body
            if body' <> body then Abs (x, body')
            else Abs (x, body)
        | v -> v
    
    let normalize term=
        let maxSteps = 1000
        let rec loop current step =
            if step >= maxSteps then None
            else
                let next = reduce current
                if next = current then Some current
                else loop next (step + 1)    
        loop term 0
    