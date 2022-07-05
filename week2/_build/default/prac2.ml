let rec last (lst: string list) : string option =
        match lst with
        |[] -> None
        |h::[] -> Some h
        |_::t -> last t

let rec last_two (lst: string list) : (string*string) option =
        match lst with
        |[] -> None
        |_::[] -> None
        |x::y::[] -> Some (x,y)
        |_::t -> last_two t 

let rec at (k : int) (lst: string list) : string option =
        match lst with
        |[] -> None
        |h::t -> match k with
                |0 -> None
                |1 -> Some h
                |k -> at (k-1) t 

let rec length (lst: string list) : int =
     match lst with
     |[] -> 0
     |_::t -> 1+(length t)

let rec rev (lst: string list) : string list =
        match lst with
        |[] -> []
        |h::t -> (rev t)@[h]

let is_palin (lst: string list) : bool = 
        if (lst = (rev lst)) || (lst = []) then true
        else false

let rec sort (lst: int list) : int list =
        let rec insert (element: int) (lst2: int list) : int list =
                match lst2 with
                |[] -> [element]
                |h::t -> if element < h then element::h::t
                         else h::insert element t
        in
        match lst with
        |[] -> []
        |h::t -> insert h (sort t)
