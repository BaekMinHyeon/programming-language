type t =
  | NumV of int
  | BoolV of bool
  | AddrV of Env.addr

let pp fmt (v: t) : unit =
        match v with
        | NumV n -> Format.fprintf fmt "%d" n
        | BoolV b -> Format.fprintf fmt "%b" b
        | AddrV a -> Format.fprintf fmt "a%d" a
