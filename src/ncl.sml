structure NCL =
struct
  datatype v =
      Unit
    | Int of int
    | Bool of bool
    | Var of string
  and t =
      Value of v
    | LetVal of string * v * t
    | Let of string * comp * t
    | If of v * t * t
  and comp =
      Lam of string * t
    | Ap of v * v
    | Ref of string * v
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
    | normalizeK (AST.Value (AST.Lam (x, e))) k = mkLet (Lam (x, normalizeK e Value), k)
    | normalizeK (AST.Ap (e1, e2)) k =
        normalizeK e1 (fn v1 =>
          normalizeK e2 (fn v2 =>
            mkLet (Ap (v1, v2), k)))
    | normalizeK (AST.Let (x, e1, e2)) k =
        normalizeK e1 (fn v1 =>
          LetVal (x, v1,
          normalizeK e2 k))
    | normalizeK (AST.If (e1, e2, e3)) k =
        normalizeK e1 (fn v1 =>
          let
            val k_var = fresh ()
            val k_arg = fresh ()
          in
            Let (k_var, Lam (k_arg, k (Var k_arg)),
            If (v1,
                normalizeK e2 (fn v => mkLet (Ap (Var k_var, v), Value)),
                normalizeK e3 (fn v => mkLet (Ap (Var k_var, v), Value))))
          end)
    | normalizeK (AST.Ref e) k = normalizeK e (fn v => mkLet (Ref (fresh (), v), k))
    | normalizeK (AST.Deref e) k = normalizeK e (fn v => mkLet (Deref v, k))
    | normalizeK (AST.Set (e1, e2)) k =
        normalizeK e1 (fn v1 =>
          normalizeK e2 (fn v2 =>
            mkLet (Set (v1, v2), k)))
  and mkLet (comp, k) =
    let
      val tmp = fresh ()
    in
      Let (tmp, comp, k (Var tmp))
    end

  val tab = "    "

  fun addParen b s = if b then "(" ^ s ^ ")" else s

  (*  block : string -> string -> (NCL.t -> string) -> NCL.t -> string *)
  fun block indent sep k e =
    let
      val newblock = case e of Value _ => false | _ => true
      val indent' = if newblock then tab ^ indent else indent
    in
      if newblock
      then sep ^ "\n" ^ indent' ^ k indent' e
      else sep ^ " " ^ k indent' e
    end

  fun mkStringAuxV pred Unit = "()"
    | mkStringAuxV pred (Int n) = Int.toString n
    | mkStringAuxV pred (Bool b) = Bool.toString b
    | mkStringAuxV pred (Var x) = x
  and mkStringAuxC pred indent (Lam (x, e)) =
        addParen (pred > 0) ("fn " ^ x ^ block indent " =>" (mkStringAux 0) e)
    | mkStringAuxC pred indent (Ap (e1, e2)) =
        addParen (pred >= 10) (mkStringAuxV 9 e1 ^ " " ^ mkStringAuxV 10 e2)
    | mkStringAuxC pred indent (Ref (tag, v)) =
        addParen (pred >= 10) ("ref " ^ mkStringAuxV 10 v)
    | mkStringAuxC pred indent (Deref v) =
        addParen (pred > 8) ("!" ^ mkStringAuxV 10 v)
    | mkStringAuxC pred indent (Set (v1, v2)) =
        addParen (pred > 4) (mkStringAuxV 10 v1 ^ " := " ^ mkStringAuxV 10 v2)
  and mkStringAux pred indent (Value v) = mkStringAuxV pred v
    | mkStringAux pred indent (LetVal (x, v, e)) =
        addParen (pred > 0) ("let/val " ^ x ^ " = " ^ mkStringAuxV 0 v ^ " in\n"
                            ^ indent ^ mkStringAux 0 indent e)
    | mkStringAux pred indent (Let (x, e1, e2)) =
        addParen (pred > 0) ("let " ^ x ^ " = " ^ mkStringAuxC 0 indent e1 ^ " in\n"
                            ^ indent ^ mkStringAux 0 indent e2)
    | mkStringAux pred indent (If (e1, e2, e3)) =
        addParen (pred > 0) ("if " ^ mkStringAuxV 0 e1
                            ^ block indent "\nthen" (mkStringAux 0) e2
                            ^ block indent "\nelse" (mkStringAux 0) e3)

  val toString = mkStringAux 0 ""

  val p0 = normalizeK AST.p0 Value
  val p1 = normalizeK AST.p1 Value
  val p2 = normalizeK AST.p2 Value

end
