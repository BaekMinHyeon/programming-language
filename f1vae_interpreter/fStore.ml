type t = (string * (string list * Ast.expr)) list
let empty : t = []
let mem (x: string) (fs: t) : bool =
        let rec contain (fs1: t) : bool =
                match fs1 with
                | [] -> false
                | (h, (_, _))::a -> if h = x then true
                                    else contain a
        in
        contain fs
let add (x: string) (p1: string list) (e: Ast.expr) (fs: t) : t =
        let rec remove (map: t) : t =
                match map with
                | [] -> []
                | (h, (sl, i))::a -> if h = x then a
                                     else (h, (sl, i))::(remove a)
        in
        (x, (p1, e))::(remove fs)
let find (x: string) (fs: t) : (string list * Ast.expr) =
        let rec contain (fs1: t) : (string list * Ast.expr) =
                match fs1 with
                | [] -> failwith ("Undefined function: "^x)
                | (h, (sl, i))::a -> if h = x then (sl, i)
                                     else contain a
        in
        contain fs
