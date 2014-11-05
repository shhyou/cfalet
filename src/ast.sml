structure AST =
struct
  (* syntax tree, `v` is for (syntactic) value and `t` the expression *)
  datatype v =
      Unit
    | Int of int
    | Bool of bool
    | Var of string
    | Lam of string * t
  and t =
      Value of v
    | Ap of t * t
    | Let of string * t * t
    | If of t * t * t
    | Ref of t
    | Deref of t
    | Set of t * t
end
