structure CFA0 =
struct

structure Set = struct open IntBinarySet end
structure Map = struct open IntBinaryMap end

type addr = int

type env = addr EnvStr.t

datatype storable =
    (* Value *)
    Const
  | Ref of addr
  | Cl of string * env * string * NCL.t
    (* Kont *)
  | Frame of string * env * NCL.t * addr
  | Halt

structure PowVal = BinarySetFn(struct
  type ord_key = storable

  fun getPriority Const = 0
    | getPriority (Ref _) = 1
    | getPriority (Cl _) = 2
    | getPriority (Frame _) = 3
    | getPriority Halt = 4

  fun compare (Const, Const) = EQUAL
    | compare (Ref l1, Ref l2) = Int.compare (l1, l2)
    | compare (Cl (f, _, _, _), Cl (g, _, _, _)) = String.compare (f, g)
    | compare (Frame (x1, _, _, l1), Frame (x2, _, _, l2)) =
        (case String.compare (x1, x2) of
            EQUAL => Int.compare (l1, l2)
          | unequ => unequ)
    | compare (Halt, Halt) = EQUAL
    | compare (v1, v2) = Int.compare (getPriority v1, getPriority v2)
end)

type store = PowVal.set Map.map

local
  val cnt = ref 0
  fun alloc_map map =
    ( fn () => map
    , fn x =>
        EnvStr.lookup x (!map) handle EnvStr.NotFound _ =>
          let
            val addr = !cnt
          in
            cnt := !cnt + 1;
            map := EnvStr.extend (x, addr) (!map);
            addr
          end )
in
  val (getAllocMap, alloc) = alloc_map (ref EnvStr.empty)
  val (getAllocKMap, allocK) = alloc_map (ref EnvStr.empty)
end

exception Undefined
exception NotContinuation

fun storeInsert (item as (store, addr, values)) =
  case Map.find (store, addr) of
      NONE => Map.insert item
    | SOME values' => Map.insert (store, addr, PowVal.union (values, values'))

fun gamma (cxt, store, NCL.Var x) = valOf (Map.find (store, EnvStr.lookup x cxt))
  | gamma _ = PowVal.singleton Const

(*  return : <sigma, vs, l> -> { <Gamma', sigma', e, l'> | <frame Gamma, x, e, l'> in sigma(l) } *)
fun return (store, values, kont) =
  let
    fun collectEval (Frame (x, cxt, e, kont'), states) =
        let
          val l = alloc x
        in
          eval (EnvStr.extend (x, l) cxt, storeInsert (store, l, values), e, kont')@states
        end
      | collectEval (Halt, states) = states
      | collectEval _ = raise NotContinuation

    val konts = valOf (Map.find (store, kont))
  in
    if PowVal.member (konts, Halt)
    then PowVal.foldl collectEval [store] konts
    else PowVal.foldl collectEval [] konts
  end

and comp (cxt, store, expr, y, e', l) =
  let
    val l' = allocK y
    val store' = storeInsert (store, l', PowVal.singleton (Frame (y, cxt, e', l)))

    fun doIt (NCL.Lam (x, e)) = return (store', PowVal.singleton (Cl (y, cxt, x, e)), l')
      | doIt (NCL.Ap (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          fun collectEval (Cl (_, cxt', x', e''), states) =
              let
                val l'' = alloc x'
              in
                eval (EnvStr.extend (x', l'') cxt', storeInsert (store', l'', v2'), e'', l')@states
              end
            | collectEval (_, states) = states
        in
          PowVal.foldl collectEval [] v1'
        end
      | doIt (NCL.Ref v) =
        let
          val l'' = alloc y
        in
          return (storeInsert (store', l'', gamma (cxt, store, v)), PowVal.singleton (Ref l''), l')
        end
      | doIt (NCL.Deref v) =
        let
          val getVals = List.map (fn (Ref l) => valOf (Map.find (store, l))
                                   | _ => PowVal.empty)
          val getVal = List.foldl PowVal.union PowVal.empty o getVals
        in
          return (store', getVal (PowVal.listItems (gamma (cxt, store, v))), l')
        end
      | doIt (NCL.Set (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          val store'' = PowVal.foldl (fn (Ref l, st) => storeInsert (st, l, v2')
                                       | (_, st) => st)
                                     store' v1'
        in
          return (store'', PowVal.singleton Const, l')
        end
  in
    doIt expr
  end

and eval (cxt, store, NCL.Value v, l) = return (store, gamma (cxt, store, v), l)
  | eval (cxt, store, NCL.LetVal (x, v, e), l) =
    let
      val l' = allocK x
    in
      return (storeInsert (store, l', PowVal.singleton (Frame (x, cxt, e, l))), gamma (cxt, store, v), l')
    end
  | eval (cxt, store, NCL.Let (x, f, e), l) = comp (cxt, store, f, x, e, l)
  | eval (cxt, store, NCL.If (_, e1, e2), l) = eval (cxt, store, e1, l) @ eval (cxt, store, e2, l)

val kont0 = allocK ""
val store0 = Map.insert (Map.empty, kont0, PowVal.singleton Halt)

end
