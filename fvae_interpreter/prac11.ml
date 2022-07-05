let rec interp (p: Ast.expr) (s: Store.t) : Store.value =
        let num (n: Store.value) : int =
                match n with
                | NumV n -> n
                | _ -> failwith "Not a number."
        in
        match p with
        | Num n -> NumV n
        | Add (e1, e2) -> NumV ((num (interp e1 s))+(num (interp e2 s)))
        | Sub (e1, e2) -> NumV ((num (interp e1 s))-(num (interp e2 s)))
        | Id s1 -> Store.find s1 s
        | LetIn (s1, e1, e2) -> interp e2 (Store.add s1 (interp e1 s) s)
        | App (e1, e2) ->
                begin
                        match interp e1 s with
                        | ClosureV (s1, e3, t) -> interp e3 (Store.add s1 (interp e2 s) t)
                        | _ -> failwith "Not a function."
                end
        | Lambda (s1, e1) -> ClosureV (s1, e1, s)

