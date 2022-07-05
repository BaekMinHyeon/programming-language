type t = (string * Value.t) list

let empty = []
let mem (x:string) (s:t) : bool =
        let rec contain (s1:t) : bool =
                match s1 with
                | [] -> false
                | (h, _)::a -> if h = x then true
                          else contain a
        in
        contain s
let add (x:string) (v: Value.t) (s:t) : t =
        let rec remove (map: t) : t =
                match map with
                | [] -> []
                | (h, i)::a -> if h = x then a
                               else (h, i)::(remove a)
        in
        (x, v)::(remove s)
let find (x:string) (s:t) : Value.t =
        let rec contain (s1:t) : Value.t =
                match s1 with
                | [] -> failwith ("Free identifier: "^x)
                | (h, v)::a -> if h = x then v
                               else contain a
        in
        contain s
