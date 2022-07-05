let interp_fundef (fd: Ast.fundef) (fs: FStore.t) : FStore.t =
        match fd with
        | FunDef (h, sl, ae) -> FStore.add h sl ae fs

let rec interp_expr (e: Ast.expr) (fs: FStore.t) (s: Store.t) : Value.t =
        let num (n: Value.t) : int =
                match n with
                | NumV n -> n
        in
        let rec makelist (e1: Ast.expr list) : Value.t list =
                match e1 with
                | [] -> []
                | h::a -> (interp_expr h fs s)::makelist a
        in
        let rec storeadd (s1: string list) (v1: Value.t list) (s: Store.t) : Store.t =
                match (s1, v1) with
                | ([], []) -> []
                | ([], _::_) -> failwith ("Unmatched numbers of arguments: "^(string_of_int (List.length v1))^" <> "^(string_of_int (List.length s1)))
                | (_::_, []) -> failwith ("Unmatched numbers of arguments: "^(string_of_int (List.length v1))^" <> "^(string_of_int (List.length s1)))
                | (hs::ts, hv::tv) -> (Store.add hs hv s)@(storeadd ts tv s)
        in
        match e with
        | Num n -> NumV n
        | Add (e1, e2) -> NumV ((num (interp_expr e1 fs s))+(num (interp_expr e2 fs s)))
        | Sub (e1, e2) -> NumV ((num (interp_expr e1 fs s))-(num (interp_expr e2 fs s)))
        | Id s1 -> Store.find s1 s
        | LetIn (s1, e1, e2) -> interp_expr e2 fs (Store.add s1 (interp_expr e1 fs s) s)
        | Call (s1, e1) ->
                begin
                        match FStore.find s1 fs with
                        | (sl, ae) -> interp_expr ae fs (storeadd sl (makelist e1) [])
                end

let interp (p: Ast.prog) : Value.t =
        let rec interp1 (p1: Ast.fundef list) (fs: FStore.t) : FStore.t =
               match p1 with
               | [] -> fs
               | (h::a) -> (interp1 a (interp_fundef h fs))
        in
        match p with
        | Prog (l, e) -> interp_expr e (interp1 l []) []

let ast = ParserMain.parse "def f x y z p q = x + y endef f(1, 2, 3)"
let _ = Format.printf "%a\n" Ast.pp ast
let _ = Format.printf "%a\n" Ast.pp_ast ast
let _ = Format.printf "%a\n" Value.pp (interp ast)
