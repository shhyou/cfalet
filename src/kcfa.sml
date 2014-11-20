structure CFA0 =
struct

type addr = int

(* env := string -> addr *)
type env = addr EnvStr.t

datatype value =
    Unit
  | Int of int
  | Bool of bool
  | Ref of addr
  | Clos of string * string * NCL.t * env (* tag * x * e * environment *)

(* ValSet := P(value) *)
structure ValSet = BinarySetFn(struct
  type ord_key = value

  fun getPriority Unit = 0
    | getPriority (Int _) = 1
    | getPriority (Bool _) = 2
    | getPriority (Ref _) = 3
    | getPriority (Clos _) = 4

  fun compare (Unit, Unit) = EQUAL
    | compare (Int n, Int m) = Int.compare (n, m)
    | compare (Bool false, Bool true) = LESS
    | compare (Bool true, Bool false) = GREATER
    | compare (Bool _, Bool _) = EQUAL
    | compare (Ref a1, Ref a2) = Int.compare (a1, a2)
    | compare (Clos (f, _, _, _), Clos (g, _, _, _)) = String.compare (f, g)
    | compare (v1, v2) = Int.compare (getPriority v1, getPriority v2)
end)

structure Map = struct open IntBinaryMap end

(* store := addr -> P(value) *)
type store = ValSet.set Map.map

exception Undefined

val alloc =
  let
    val cnt = ref (~1)
    val strs = ref EnvStr.empty
  in
    fn x => EnvStr.lookup x (!strs)
      handle (EnvStr.NotFound _) =>
        ( cnt := !cnt + 1;
          strs := EnvStr.extend (x, !cnt) (!strs);
          !cnt )
  end

(*  eval : env * store * NCL.t -> store * ValSet.set *)
fun eval (cxt, sigma, NCL.Value NCL.Unit) = (sigma, ValSet.singleton Unit)
  | eval (cxt, sigma, NCL.Value (NCL.Int n)) = (sigma, ValSet.singleton (Int n))
  | eval (cxt, sigma, NCL.Value (NCL.Bool b)) = (sigma, ValSet.singleton (Bool b))
  | eval (cxt, sigma, NCL.Value (NCL.Var x)) = (sigma, valOf (Map.find (sigma, EnvStr.lookup x cxt)))
  | eval (cxt, sigma, NCL.LetLam (f, x, e1, e2)) = raise Undefined
  | eval (cxt, sigma, NCL.Ap (NCL.Var x, e2)) =
      let
      in
        raise Undefined
      end
  | eval (cxt, sigma, NCL.Ap (e1, e2)) = raise Undefined
end
