type encoded_elem = int * string
let rec decode (lst: encoded_elem list) : string list =
        let rec mk_list (acc: string list) (n: int) (s: string) : string list =
                if n = 0 then acc
                else mk_list (s::acc) (n-1) s
        in
        match lst with
        | [] -> []
        | (n,h)::t -> (mk_list [] n h)@(decode t)

let rec compress (lst: string list) : string list =
        let head (lst2: string list) : string =
                match lst2 with
                | [] -> ""
                | h::_ -> h
        in
        match lst with
        | [] -> []
        | h::t -> if h = head t then compress t
                  else h::compress t

let pack (lst: string list) : string list list =
        let head (lst2: string list) : string =
                match lst2 with
                | [] -> ""
                | h::_ -> h
        in
        let rec nested (n: int) (lst2: string list) : string list list =
                let rec make (lst3: string list) : string list =
                        match lst3 with
                        | [] -> []
                        | h::t -> if h = head t then h::make t
                                  else [h]
                in
                match lst2 with
                | [] -> []
                | h::[] -> begin
                                match n with
                                | 0 -> [[h]]
                                | _ -> []
                           end
                | h::t -> begin
                                match n with
                                | 0 -> make lst2::nested (n+1) t
                                | _ -> if h <> head t then make t::nested (n+1) t
                                       else nested (n+1) t
                          end 
        in
        nested 0 lst

let encode (lst: string list) : encoded_elem list =
        let head (lst2: string list) : string =
                match lst2 with
                | [] -> ""
                | h::_ -> h
        in
        let rec encoding (n: int) (lst2: string list) : encoded_elem list =
                match lst2 with
                | [] -> []
                | h::t -> if h = head t then encoding (n+1) t
                          else (n, h)::encoding 1 t
        in
        encoding 1 lst

let slice (lst: string list) (s: int) (e: int) : string list =
        let rec find (lst2: string list) (number: int) : string list =
                match lst2 with
                | [] -> []
                | h::t -> if (number < s || number > e) then find t (number+1)
                          else h::find t (number+1)
        in
        let rec length (lst2: string list) : int =
                match lst2 with
                |[] -> 0
                |_::t -> 1+(length t)
        in
        if (s < 0 || e >= length lst || s > e) then failwith "index is out of range" 
        else find lst 0

let rec rotate (lst: string list) (index: int) : string list =
        let rec length (lst2: string list) : int =
                match lst2 with
                |[] -> 0
                |_::t -> 1+(length t)
        in
        let rec back (lst2: string list) (number: int) : string list =
                match lst2 with
                | [] -> []
                | h::t -> if (number < index) then back t (number+1)
                          else h::back t (number+1)
        in
        let rec front (lst2: string list) (number: int) : string list =
               match lst2 with
               | [] -> []
               | h::t -> if (number < index) then h::front t (number+1)
                         else front t (number+1)
        in
        if (length lst <= index || (length lst)+index < 0) then failwith "index is out of range"
        else if index < 0 then rotate lst ((length lst)+index)
        else (back lst 0)@(front lst 0) 
