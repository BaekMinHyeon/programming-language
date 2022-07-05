let rec interp (e: Ast.expr) (s: Store.t) : Store.value =
        let num (n: Store.value) : int =
                match n with
                | NumV n -> n
                | _ -> failwith "Not a number."
        in
        let boolean (b: Store.value) : bool =
                match b with
                | BoolV b -> b
                | _ -> failwith "Not a bool."
        in
        match e with
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
        | Bool b -> BoolV b
        | Cond (e1, e2, e3) -> if boolean (interp e1 s) == true then interp e2 s
                               else interp e3 s
        | LessThan (e1, e2) -> if num (interp e1 s) < num (interp e2 s) then BoolV true
                               else BoolV false

let rec interp_c (e: AstC.expr) (s: StoreC.t) : StoreC.value =
        let num (n: StoreC.value) : int =
                match n with
                | NumV n -> n
                | _ -> failwith "Not a number."
        in
        match e with
        | Num n -> NumV n
        | Add (e1, e2) -> NumV ((num (interp_c e1 s))+(num (interp_c e2 s)))
        | Sub (e1, e2) -> NumV ((num (interp_c e1 s))-(num (interp_c e2 s)))
        | Id s1 -> StoreC.find s1 s
        | LetIn (s1, e1, e2) -> interp_c e2 (StoreC.add s1 (interp_c e1 s) s)
        | App (e1, e2) ->
                begin
                        match interp_c e1 s with
                        | ClosureV (s1, e3, t) -> interp_c e3 (StoreC.add s1 (interp_c e2 s) t)
                        | _ -> failwith "Not a function."
                end
        | Lambda (s1, e1) -> ClosureV (s1, e1, s)
        | Cond (e1, e2, e3) ->
                begin
                        match num (interp_c e1 s) with
                        | 0 -> interp_c e3 s
                        | _ -> interp_c e2 s
                end
        | LessThan (e1, e2) -> if num (interp_c e1 s) < num (interp_c e2 s) then NumV 1
                               else NumV 0

let rec interp_fp (e: AstFP.expr) (s: StoreFP.t) : StoreFP.value =
        let num (n: StoreFP.value) : int =
                match n with
                | NumV n -> n
                | _ -> failwith "Not a number."
        in
        match e with
        | Num n -> NumV n
        | Add (e1, e2) -> NumV ((num (interp_fp e1 s))+(num (interp_fp e2 s)))
        | Sub (e1, e2) -> NumV ((num (interp_fp e1 s))-(num (interp_fp e2 s)))
        | Id s1 -> StoreFP.find s1 s
        | LetIn (s1, e1, e2) -> interp_fp e2 (StoreFP.add s1 (interp_fp e1 s) s)
        | App (e1, e2) ->
                begin
                        match interp_fp e1 s with
                        | ClosureV (s1, e3, t) -> interp_fp e3 (StoreFP.add s1 (interp_fp e2 s) t)
                        | _ -> failwith "Not a function."
                end
        | Lambda (s1, e1) -> ClosureV (s1, e1, s)
        | LessThan (e1, e2) -> if num (interp_fp e1 s) < num (interp_fp e2 s) then ClosureV ("x", Lambda ("y", Id "x"), [])
                               else ClosureV ("x", Lambda ("y", Id "y"), [])



