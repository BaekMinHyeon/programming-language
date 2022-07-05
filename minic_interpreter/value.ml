type t =
  | NumV of int
  | BoolV of bool

let pp fmt (v: t) : unit =
        match v with
        | NumV n -> Format.fprintf fmt "%d" n
        | BoolV b -> Format.fprintf fmt "%b" b
