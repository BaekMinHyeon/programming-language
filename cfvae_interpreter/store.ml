type t = (string * value) list
and value =
        | NumV of int
        | BoolV of bool
        | ClosureV of string * Ast.expr * t

let empty = []
let mem (x:string) (s:t) : bool =
        let rec contain (s1:t) : bool =
                match s1 with
                | [] -> false
                | (h, _)::a -> if h = x then true
                          else contain a
        in
        contain s
let add (x:string) (v: value) (s:t) : t =
        let rec remove (map: t) : t =
                match map with
                | [] -> []
                | (h, i)::a -> if h = x then a
                               else (h, i)::(remove a)
        in
        (x, v)::(remove s)
let find (x:string) (s:t) : value =
        let rec contain (s1:t) : value =
                match s1 with
                | [] -> failwith ("Free identifier: "^x)
                | (h, v)::a -> if h = x then v
                               else contain a
        in
        contain s

