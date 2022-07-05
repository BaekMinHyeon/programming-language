module ListSet = struct
        type t = int list

        let empty : t = []
        let add (elem: int) (set: t) : t =
                let rec remove (set1: t) : t =
                        match set1 with
                        | [] -> empty
                        | h::a -> if h = elem then a
                                  else h::remove a
                in
                if List.mem elem set = false then elem::set
                else elem::(remove set)
        let union (set1: t) (set2: t) : t =
                let rec contain (set: t) : t =
                        match set with
                        | [] -> []
                        | h::a -> if List.mem h set1 = true then contain a
                                  else h::contain a
                in
                set1@(contain set2)
        let intersection (set1: t) (set2: t) : t =
                let rec contain (set: t) : t =
                        match set with
                        | [] -> empty
                        | h::a -> if List.mem h set2 = true then h::contain a
                                  else contain a
                in
                contain set1
        let relative_complement (set1: t) (set2: t) : t =
                let rec contain (set: t) : t =
                        match set with
                        | [] -> empty
                        | h::a -> if List.mem h set2 = true then contain a
                                  else h::contain a
                in
                contain set1
end

module ListMap = struct
        type t = (string * int) list

        let empty : t = []
        let add (key: string) (value : int) (map : t) : t =
                let rec remove2 (map1: t) : t =
                        match map1 with
                        | [] -> []
                        | (s, i)::a -> if s = key then a
                                       else (s, i)::(remove2 a)
                in
                (key, value)::(remove2 map)
        let get (key: string) (map: t) : int =
                let rec get2 (map1: t) : int =
                        match map1 with
                        | [] -> failwith "No such key here."
                        | (s, i)::a -> if s = key then i
                                       else get2 a
                in
                get2 map
        let remove (key : string) (map: t) : t =
                let rec remove2 (map1: t) : t =
                        match map1 with
                        | [] -> failwith "No such key here."
                        | (s, i)::a -> if s = key then a
                                       else (s, i)::(remove2 a)
                in
                remove2 map
        let values (map: t) : ListSet.t =
                let rec r_value (value: int) (map1: t) : t =
                        match map1 with
                        | [] -> []
                        | (s, i)::a -> if value = i then r_value value a
                                       else (s, i)::(r_value value a)
                in 
                let rec values2 (map1: t) : ListSet.t =
                        match map1 with
                        | [] -> []
                        | (_, i)::a -> i::(values2 (r_value i a))
                in
                values2 map
end

type value = Int of int | String of string
type node = N of value * node list
let preorder (n: node) : value list =
        let rec print (n1:node list) : value list =
                match n1 with
                | [] -> []
                | (N(v, h))::t -> [v]@(print h)@(print t)
        in
        match n with
        | N(v, []) -> [v]
        | N(v, h) -> v::print h
let postorder (n: node) : value list =
        let rec print (n1:node list) : value list =
                match n1 with
                | [] -> []
                | (N(v, h))::t -> (print h)@[v]@(print t)
        in
        match n with
        | N(v, []) -> [v]
        | N(v, h) -> print h@[v]
let levelorder (n: node) : value list =
        let rec level (n1:node list) (n2: node list): value list =
                match n1 with
                | [] -> begin
                                match n2 with
                                | [] -> []
                                | (N(v, h1))::t1 -> [v]@(level h1 t1)
                        end
                | h::t -> level t (n2@[h])
        in
        match n with
        | N(v, []) -> [v]
        | N(v, h) -> v::level h []

