let rec interp (e: Ast.expr) (s: Store.t) : Store.value =
        let num (n: Store.value) : int =
                match n with
                | NumV n -> n
                | _ -> failwith "Not a number."
        in
        match e with
        | Num n -> NumV n
        | Add (e1, e2) -> NumV ((num (interp e1 s))+(num (interp e2 s)))
        | Sub (e1, e2) -> NumV ((num (interp e1 s))-(num (interp e2 s)))
        | Id s1 ->
                begin
                        match Store.find s1 s with
                        | FreezedV (e1, t) -> interp e1 t 
                        | _ -> Store.find s1 s
                end
        
        | LetIn (s1, e1, e2) -> interp e2 (Store.add s1 (interp e1 s) s)
        | App (e1, e2) ->
                begin
                        match interp e1 s with
                        | ClosureV (s1, e3, t) -> interp e3 (Store.add s1 (FreezedV (e2, s)) t)
                        | _ -> failwith "Not a function."
                end
        | Lambda (s1, e1) -> ClosureV (s1, e1, s)
        | LessThan (e1, e2) -> if num (interp e1 s) < num (interp e2 s) then ClosureV ("x", Lambda ("y", Id "x"), [])
                               else ClosureV ("x", Lambda ("y", Id "y"), [])
        | RLetIn (s1, e1, e2) ->
               begin
                      match interp e1 s with
                      | ClosureV (s2, e3, t) -> let rec s'' = (s1, Store.ClosureV (s2, e3, s'')) :: t in interp e2 s''
                      | _ -> failwith "Not a function."
               end

