let rec interp_expr (e: Ast.expr) (s: Store.t) : Value.t =
        let num (n: Value.t) : int =
                match n with
                | NumV n -> n
                | _ -> failwith "Not a number."
        in
        let boolean (b: Value.t) : bool =
                match b with
                | BoolV b -> b
                | _ -> failwith "Not a bool."
        in
        match e with
        | Num n -> NumV n
        | Bool b -> BoolV b
        | Add (e1, e2) -> NumV ((num (interp_expr e1 s))+(num (interp_expr e2 s)))
        | Sub (e1, e2) -> NumV ((num (interp_expr e1 s))-(num (interp_expr e2 s)))
        | Name s1 -> Store.find s1 s
        | Lt (e1, e2) -> if num (interp_expr e1 s) < num (interp_expr e2 s) then BoolV true 
                         else BoolV false
        | Gt (e1, e2) -> if num (interp_expr e1 s) > num (interp_expr e2 s) then BoolV true
                         else BoolV false
        | Eq (e1, e2) -> if interp_expr e1 s == interp_expr e2 s then BoolV true
                         else BoolV false
        | And (e1, e2) -> BoolV (boolean (interp_expr e1 s) && boolean (interp_expr e2 s))
        | Or (e1, e2) -> BoolV (boolean (interp_expr e1 s) || boolean (interp_expr e2 s))

let rec interp_stmt (e: Ast.stmt) (s: Store.t) : Store.t =
        let rec interp_list (sl: Ast.stmt list) (s1: Store.t) : Store.t =
                match sl with
                | h::t -> interp_list t (interp_stmt h s1)
                | [] -> s1
        in
        match e with
        | AssignStmt (str, e1) -> Store.add str (interp_expr e1 s) s
        | IfStmt (e1, sl1, sl2) -> 
                        begin
                                match interp_expr e1 s with
                                | BoolV false -> interp_list sl2 s
                                | BoolV true -> interp_list sl1 s 
                                | _ -> failwith "Not a bool."
                        end

let interp_prog (e: Ast.prog) : Store.t =
         let rec interp_list (sl: Ast.stmt list) (s: Store.t) : Store.t =
                match sl with
                | h::t -> interp_list t (interp_stmt h s)
                | [] -> s
        in
        match e with
        | Program sl -> interp_list sl []

let ast = ParserMain.parse "x = 1; y = 2; if (x > y){ y = 1;} else{ y = 1; if (x == y) { y = y + 3;}}"
let _ = Format.printf "%a\n" Ast.pp_ast ast 
let _ = Format.printf "%a\n" Store.pp (interp_prog ast)
