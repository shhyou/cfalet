structure NCL =
struct
  datatype v =
      Unit
    | Int of int
    | Bool of bool
    | Var of string
    | Lam of string * t
  and t =
      Value of v
    | Ap of v * v
    | Let of string * string option * t * t
    | If of v * t * t
    | Ref of v
    | Deref of v
    | Set of v * v

  val fresh =
    let
      val cnt = ref 0
    in
      fn () =>
        let
          val name = "%" ^ Int.toString (!cnt)
        in
          cnt := !cnt + 1;
          name
        end
    end

  (*  normalizeK : AST.t -> (NCL.v -> NCL.t) -> NCL.t *)
  fun normalizeK (AST.Value AST.Unit) k = k Unit
    | normalizeK (AST.Value (AST.Int n)) k = k (Int n)
    | normalizeK (AST.Value (AST.Bool b)) k = k (Bool b)
    | normalizeK (AST.Value (AST.Var x)) k = k (Var x)
    | normalizeK (AST.Value (AST.Lam (x, e))) k = k (Lam (x, normalizeK e Value))
    | normalizeK (AST.Ap (e1, e2)) k =
        normalizeK e1 (fn v1 =>
          normalizeK e2 (fn v2 =>
            let
              val tmp = fresh ()
            in
              Let (tmp, NONE, Ap (v1, v2),
              k (Var tmp))
            end))
    | normalizeK (AST.Let (x, e1, e2)) k =
        normalizeK e1 (fn v1 =>
          Let (fresh (), SOME x, Value v1,
          normalizeK e2 k))
    | normalizeK (AST.If (e1, e2, e3)) k =
        normalizeK e1 (fn v1 =>
          let
            val tmp = fresh ()
            val arg = fresh ()
          in
            Let (tmp, NONE, Value (Lam (arg, k (Var arg))),
            If (v1,
                normalizeK e2 (fn v => Ap (Var tmp, v)),
                normalizeK e3 (fn v => Ap (Var tmp, v))))
          end)
    | normalizeK (AST.Ref e) k =
        normalizeK e (fn v =>
          let
            val tmp = fresh ()
          in
            Let (tmp, NONE, Ref v,
            k (Var tmp))
          end)
    | normalizeK (AST.Deref e) k =
        normalizeK e (fn v =>
          let
            val tmp = fresh ()
          in
            Let (tmp, NONE, Deref v,
            k (Var tmp))
          end)
    | normalizeK (AST.Set (e1, e2)) k =
        normalizeK e1 (fn v1 =>
          normalizeK e2 (fn v2 =>
            let
              val tmp = fresh ()
            in
              Let (tmp, NONE, Set (v1, v2),
              k (Var tmp))
            end))

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
        addParen (pred >= 10) (mkStringAuxV 9 indent e1 ^ " " ^ mkStringAuxV 10 indent e2)
    | mkStringAux pred indent (Let (x, lbl, e1, e2)) =
      let
        val name = case lbl of NONE => "" | SOME y => y ^ "/"
      in
        addParen (pred > 0) ("let " ^ name ^ x ^ block indent " =" (mkStringAux 0) e1 ^ " in\n"
                            ^ indent ^ mkStringAux 0 indent e2)
      end
    | mkStringAux pred indent (If (e1, e2, e3)) =
        addParen (pred > 0) ("if " ^ mkStringAuxV 0 indent e1
                            ^ block indent "\nthen" (mkStringAux 0) e2
                            ^ block indent "\nelse" (mkStringAux 0) e3)
    | mkStringAux pred indent (Ref e) =
        addParen (pred >= 10) ("ref " ^ mkStringAuxV 10 indent e)
    | mkStringAux pred indent (Deref e) =
        addParen (pred > 8) ("!" ^ mkStringAuxV 10 indent e)
    | mkStringAux pred indent (Set (e1, e2)) =
        addParen (pred > 4) (mkStringAuxV 10 indent e1 ^ " := " ^ mkStringAuxV 10 indent e2)

  val toString = mkStringAux 0 ""

end
