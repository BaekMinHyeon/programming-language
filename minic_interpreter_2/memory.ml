type t = (Env.addr * Value.t) list

let empty = []
let mem (x:Env.addr) (s:t) : bool =
        let rec contain (s1:t) : bool =
                match s1 with
                | [] -> false
                | (h, _)::a -> if h = x then true
                          else contain a
        in
        contain s
let add (x:Env.addr) (v: Value.t) (s:t) : t =
        let rec remove (map: t) : t =
                match map with
                | [] -> []
                | (h, i)::a -> if h = x then a
                               else (h, i)::(remove a)
        in
        (x, v)::(remove s)
let find (x:Env.addr) (s:t) : Value.t =
        let rec contain (s1:t) : Value.t =
                match s1 with
                | [] -> failwith ("Uninitialized address.")
                | (h, v)::a -> if h = x then v
                               else contain a
        in
        contain s


let pp fmt (s: t) : unit =
        Format.fprintf fmt "[%a]"
        (Format.pp_print_list ~pp_sep: (fun fmt () -> Format.fprintf fmt "; ")
        (fun fmt (a, v) -> Format.fprintf fmt "a%d %a" a Value.pp v)) s
