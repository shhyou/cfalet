signature TYPE =
sig
  datatype tyscheme =
      Mono of t
    | Poly of string list * t
  and t =
      Unit
    | Int
    | Bool
    | Var of tyvar ref
    | Arrow of t * t
    | Ref of t
  and tyvar =
      Unlink of string
    | Link of t

  val toString : t -> string
  val toStringPoly : tyscheme -> string
end
