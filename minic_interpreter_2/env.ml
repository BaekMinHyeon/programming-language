type addr = int
type t = (string * addr) list

let empty = []
let mem (x:string) (s:t) : bool =
        let rec contain (s1:t) : bool =
                match s1 with
                | [] -> false
                | (h, _)::a -> if h = x then true
                          else contain a
        in
        contain s
let add (x:string) (v: addr) (s:t) : t =
        let rec remove (map: t) : t =
                match map with
                | [] -> []
                | (h, i)::a -> if h = x then a
                               else (h, i)::(remove a)
        in
        (x, v)::(remove s)
let find (x:string) (s:t) : addr =
        let rec contain (s1:t) : addr =
                match s1 with
                | [] -> failwith ("Free identifier: "^x)
                | (h, v)::a -> if h = x then v
                               else contain a
        in
        contain s

let index = ref 0

let new_addr () =
        let nindex = !index in
        let _ = index := nindex + 1 in
        nindex

let pp fmt (s: t) : unit = 
        Format.fprintf fmt "[%a]"
        (Format.pp_print_list ~pp_sep: (fun fmt () -> Format.fprintf fmt "; ")
        (fun fmt (x, a) -> Format.fprintf fmt "%s a%d" x a)) s
