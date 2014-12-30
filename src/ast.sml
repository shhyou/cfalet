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

  (*  alphaConv : AST.t -> AST.t *)
  fun alphaConv e =
    let
      val cnt = ref 0
      fun rename s =
        let
          val name = s ^ Int.toString (!cnt)
        in
          cnt := !cnt + 1;
          name
        end
      fun doIt (cxt, Value v) = Value (doItV (cxt, v))
        | doIt (cxt, Ap (e1, e2)) = Ap (doIt (cxt, e1), doIt (cxt, e2))
        | doIt (cxt, Let (x, e1, e2)) =
            let
              val x' = rename x
            in
              Let (x', doIt (cxt, e1), doIt (EnvStr.extend (x, x') cxt, e2))
            end
        | doIt (cxt, If (e1, e2, e3)) = If (doIt (cxt, e1), doIt (cxt, e2), doIt (cxt, e3))
        | doIt (cxt, Ref e) = Ref (doIt (cxt, e))
        | doIt (cxt, Deref e) = Deref (doIt (cxt, e))
        | doIt (cxt, Set (e1, e2)) = Set (doIt (cxt, e1), doIt (cxt, e2))
      and doItV (cxt, Var x) = Var (EnvStr.lookup x cxt)
        | doItV (cxt, Lam (x, e)) =
            let
              val x' = rename x
            in
              Lam (x', doIt (EnvStr.extend (x, x') cxt, e))
            end
        | doItV (cxt, v) = v
    in
      doIt (EnvStr.empty, e)
    end

  val tab = "    "

  fun addParen b s = if b then "(" ^ s ^ ")" else s

  fun block indent sep k e =
    let
      val newblock = case e of Let _ => true | If _ => true | _ => false
      val indent' = if newblock then tab ^ indent else indent
    in
      if newblock
      then sep ^ "\n" ^ indent' ^ k indent' e
      else sep ^ " " ^ k indent' e
    end

  fun mkStringAuxV pred indent Unit = "()"
    | mkStringAuxV pred indent (Int n) = Int.toString n
    | mkStringAuxV pred indent (Bool b) = Bool.toString b
    | mkStringAuxV pred indent (Var x) = x
    | mkStringAuxV pred indent (Lam (x, e)) =
        addParen (pred > 0) ("fn " ^ x ^ block indent " =>" (mkStringAux 0) e)
  and mkStringAux pred indent (Value v) = mkStringAuxV pred indent v
    | mkStringAux pred indent (Ap (e1, e2)) =
        addParen (pred >= 10) (mkStringAux 9 indent e1 ^ " " ^ mkStringAux 10 indent e2)
    | mkStringAux pred indent (Let (x, e1, e2)) =
        addParen (pred > 0) ("let " ^ x ^ block indent " =" (mkStringAux 0) e1 ^ " in\n"
                            ^ indent ^ mkStringAux 0 indent e2)
    | mkStringAux pred indent (If (e1, e2, e3)) =
        addParen (pred > 0) ("if" ^ block indent "" (mkStringAux 0) e1
                            ^ block indent "\nthen" (mkStringAux 0) e2
                            ^ block indent "\nelse" (mkStringAux 0) e3)
    | mkStringAux pred indent (Ref e) =
        addParen (pred >= 10) ("ref " ^ mkStringAux 10 indent e)
    | mkStringAux pred indent (Deref e) =
        addParen (pred > 8) ("!" ^ mkStringAux 10 indent e)
    | mkStringAux pred indent (Set (e1, e2)) =
        addParen (pred > 4) (mkStringAux 10 indent e1 ^ " := " ^ mkStringAux 10 indent e2)

  val toString = mkStringAux 0 ""

  val p0 = alphaConv (Value (Lam ("x", Value (Lam ("f", Ap (Value (Var "f"), Value (Var "x")))))))
  val p1 = alphaConv (Let ("id", Value (Lam ("x", Value (Var "x"))),
           Ap (Ap (Value (Var "id"), Value (Var "id")), Value (Int 5))))
  val p2 = alphaConv (Value (Lam ("x", Let ("y", Value (Var "x"), Value (Var "y")))))

end
