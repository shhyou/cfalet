structure Type : TYPE =
struct
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

  fun addParen b s = if b then "(" ^ s ^ ")" else s

  fun mkStringAux pred Unit = "Unit"
    | mkStringAux pred Int = "Int"
    | mkStringAux pred Bool = "Bool"
    | mkStringAux pred (Var (ref (Unlink v))) = v
    | mkStringAux pred (Var (ref (Link t))) = mkStringAux pred t
    | mkStringAux pred (Arrow (t1, t2)) =
        addParen (pred > 0) (mkStringAux 1 t1 ^ " -> " ^ mkStringAux 0 t2)
    | mkStringAux pred (Ref t) =
        addParen (pred >= 10) (mkStringAux 9 t ^ " ref")

  val toString = mkStringAux 0

  fun toStringPoly (Mono t) = toString t
    | toStringPoly (Poly (vs, t)) =
        "forall " ^ String.concatWith " " vs ^ ". " ^ toString t
end
