let rec interp (e: Ast.expr) : Value.t =
        let num (n: Value.t) : int =
                match n with
                | NumV n -> n
        in 
        match e with
        | Num n -> NumV n
        | Add (a,b) -> NumV (num (interp a)+num (interp b))
        | Sub (a,b) -> NumV (num (interp a)-num (interp b))

