(* the interpreter of the language *)

structure AST =
struct
  (* syntax tree, `v` is for (syntactic) value and `t` the expression *)
  datatype v =
      Unit
    | Int of int
    | Bool of bool
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

(* run-time value representation; hence `value -> value` for functions *)
datatype value =
    Unit
  | Int of int
  | Bool of bool
  | Fun of value -> value

(* XXX TODO eval : (string * value) list -> AST.t -> value *)
fun eval cxt expr = Fun (fn x => x)
