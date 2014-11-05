(* For compilation instructions/usages, see wrappers/typeinfer-main.sml *)

structure Type =
struct
  datatype tyscheme =
      Mono of t
    | Poly of string list * t
  and t =
      Var of tyvar ref
    | Int
    | Bool
    | Arrow of t * t
  and tyvar =
      Unlink of string
    | Link of t
end

structure Typeinfer =
struct

exception Undefined

fun infer cxt (AST.Value v) = raise Undefined
  | infer cxt (AST.Ap (v1, v2)) = raise Undefined
  | infer cxt (AST.Let (x, e1, e2)) = raise Undefined
  | infer cxt (AST.If (e1, e2, e3)) = raise Undefined
  | infer cxt (AST.Ref e) = raise Undefined
  | infer cxt (AST.Deref e) = raise Undefined
  | infer cxt (AST.Set (e1, e2)) = raise Undefined

end
