let rec interp_expr (e: Ast.expr) (env: Env.t) (mem: Memory.t) : Value.t =
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
        | Add (e1, e2) -> NumV ((num (interp_expr e1 env mem))+(num (interp_expr e2 env mem)))
        | Sub (e1, e2) -> NumV ((num (interp_expr e1 env mem))-(num (interp_expr e2 env mem)))
        | Name s1 -> Memory.find (Env.find s1 env) mem
        | Lt (e1, e2) -> if num (interp_expr e1 env mem) < num (interp_expr e2 env mem) then BoolV true
                         else BoolV false
        | Gt (e1, e2) -> if num (interp_expr e1 env mem) > num (interp_expr e2 env mem) then BoolV true
                         else BoolV false
        | Eq (e1, e2) -> if interp_expr e1 env mem == interp_expr e2 env mem then BoolV true
                         else BoolV false
        | And (e1, e2) -> BoolV (boolean (interp_expr e1 env mem) && boolean (interp_expr e2 env mem))
        | Or (e1, e2) -> BoolV (boolean (interp_expr e1 env mem) || boolean (interp_expr e2 env mem))
        | Ref s1 -> AddrV (Env.find s1 env)

let rec interp_stmt (e: Ast.stmt) (env: Env.t) (mem: Memory.t) : Env.t * Memory.t =
        let address (a: Value.t) : Env.addr =
                match a with
                | AddrV a -> a
                | _ -> failwith "Not an address."
        in
        let rec interp_list (sl: Ast.stmt list) (env1: Env.t) (mem1: Memory.t) : Env.t * Memory.t =
                match sl with
                | h::t -> 
                        begin 
                                match interp_stmt h env1 mem1 with
                                | (env2, mem2) -> interp_list t env2 mem2
                        end
                | [] -> (env, mem1)
        in
        match e with
        | VarDeclStmt s -> (Env.add s (Env.new_addr ()) env, mem)
        | IfStmt (e1, sl1, sl2) ->
                        begin
                                match interp_expr e1 env mem with
                                | BoolV false -> interp_list sl2 env mem
                                | BoolV true -> interp_list sl1 env mem
                                | _ -> failwith "Not a bool."
                        end
        | StoreStmt (e1, e2) -> (env, Memory.add (address (interp_expr e1 env mem)) (interp_expr e2 env mem) mem)
        | LoadStmt (s, e1) -> (env, Memory.add (Env.find s env) (Memory.find (address (interp_expr e1 env mem)) mem) mem)
        | WhileStmt (e1, sl) -> 
                        begin
                                match interp_expr e1 env mem with
                                | BoolV false -> (env, mem)
                                | BoolV true ->
                                        begin
                                                match interp_list sl env mem with
                                                | (env1, mem1) -> interp_stmt e env1 mem1
                                        end
                                | _ -> failwith "Not a bool."
                        end

let interp_prog (e: Ast.prog) : Env.t * Memory.t =
        let rec interp_list (sl: Ast.stmt list) (env1: Env.t) (mem1: Memory.t) : Env.t * Memory.t =
                match sl with
                | h::t -> 
                        begin
                                match interp_stmt h env1 mem1 with
                                | (env2, mem2) -> interp_list t env2 mem2
                        end
                | [] -> (env1, mem1)
        in
        match e with
        | Program sl -> interp_list sl Env.empty Memory.empty

