let rec interp (e: Ast.expr) (s:Store.t) : Value.t =
        let num (n: Value.t) : int =
                match n with
                | NumV n -> n
        in
        match e with
        | Num n -> NumV n
        | Add (e1, e2) -> NumV ((num (interp e1 s))+(num (interp e2 s)))
        | Sub (e1, e2) -> NumV ((num (interp e1 s))-(num (interp e2 s)))
        | Id s1 -> Store.find s1 s
        | LetIn (s1, e1, e2) -> interp e2 (Store.add s1 (interp e1 s) s)
