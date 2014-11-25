structure CFA0 =
struct

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

fun toString Const = "<Const>"
  | toString (Ref l) = "<Ref " ^ Int.toString l ^ ">"
  | toString (Cl (tag, env, x, e)) = "<Cl " ^ tag ^ ">"
  | toString (Frame (x, env, e, l)) = "<Frame " ^ x ^ "/" ^ Int.toString l ^ ">"
  | toString Halt = "<Halt>"

structure PowVal = struct
  structure Set = BinarySetFn(struct
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

  open Set
  val toString = fn set => "{" ^ String.concatWith "," (List.map toString (listItems set)) ^ "}"
end

structure ValMap = struct
  open IntBinaryMap
  fun toString store =
    "[" ^ String.concatWith ", "
            (List.map (fn (k,v) => Int.toString k ^ ":" ^ PowVal.toString v) store)
        ^ "]"
end

type store = PowVal.set ValMap.map

structure Stores = BinarySetFn(struct
  type ord_key = string * store
  fun compare ((x1, s1), (x2, s2)) =
    case String.compare (x1, x2) of
        EQUAL => ValMap.collate PowVal.compare (s1, s2)
      | unequ => unequ
end)

local
  val cnt = ref 0
  fun alloc_map map =
    ( fn () => !map
    , fn () => (map := EnvStr.extend ("", 0) EnvStr.empty; cnt := 1)
    , fn x =>
        EnvStr.lookup x (!map) handle _ =>
          let
            val addr = !cnt
          in
            cnt := !cnt + 1;
            map := EnvStr.extend (x, addr) (!map);
            addr
          end )
in
  val (getAllocMap, reset, alloc) = alloc_map (ref EnvStr.empty)
  val (getAllocKMap, resetK, allocK) = alloc_map (ref EnvStr.empty)
end

exception Undefined
exception NotContinuation

fun storeInsert (item as (store, addr, values)) =
  case ValMap.find (store, addr) of
      NONE => ValMap.insert item
    | SOME values' => ValMap.insert (store, addr, PowVal.union (values, values'))

fun gamma (cxt, store, NCL.Var x) = valOf (ValMap.find (store, EnvStr.lookup x cxt))
  | gamma _ = PowVal.singleton Const

(*  return : <sigma, vs, l> -> { <Gamma', sigma', e, l'> | <frame Gamma, x, e, l'> in sigma(l) } *)
fun return (store, values, kont, stores) =
  let
    fun collectEval (Frame (x, cxt, e, kont'), stores') =
        let
          val l = alloc x
          val store' = storeInsert (store, l, values)
          val stores'' = Stores.add (stores', (x, store'))
        in
          eval (EnvStr.extend (x, l) cxt, store', e, kont', stores'')
        end
      | collectEval (Halt, stores') = Stores.add (stores', ("", store))
      | collectEval _ = raise NotContinuation

    val konts = valOf (ValMap.find (store, kont))
  in
    PowVal.foldl collectEval stores konts
  end

and comp (cxt, store, expr, y, e', l, stores) =
  let
    val l' = allocK y
    val store' = storeInsert (store, l', PowVal.singleton (Frame (y, cxt, e', l)))

    fun doIt (NCL.Lam (x, e)) = return (store', PowVal.singleton (Cl (y, cxt, x, e)), l', stores)
      | doIt (NCL.Ap (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          fun collectEval (Cl (_, cxt', x', e''), stores') =
              let
                val l'' = alloc x'
                val store'' = storeInsert (store', l'', v2')
              in
                if Stores.member (stores', (x', store''))
                then stores'
                else eval (EnvStr.extend (x', l'') cxt', store'', e'', l', Stores.add (stores', (x', store'')))
              end
            | collectEval (_, stores') = stores'
        in
          PowVal.foldl collectEval stores v1'
        end
      | doIt (NCL.Ref (tag, v)) =
        let
          val l'' = alloc tag
          val store'' = storeInsert (store', l'', gamma (cxt, store, v))
        in
          return (store'', PowVal.singleton (Ref l''), l', stores)
        end
      | doIt (NCL.Deref v) =
        let
          val getVals = List.map (fn (Ref l) => valOf (ValMap.find (store, l))
                                   | _ => PowVal.empty)
          val getVal = List.foldl PowVal.union PowVal.empty o getVals
        in
          return (store', getVal (PowVal.listItems (gamma (cxt, store, v))), l', stores)
        end
      | doIt (NCL.Set (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          val store'' = PowVal.foldl (fn (Ref l, st) => storeInsert (st, l, v2')
                                       | (_, st) => st)
                                     store' v1'
        in
          return (store'', PowVal.singleton Const, l', stores)
        end
  in
    doIt expr
  end

and eval (cxt, store, NCL.Value v, l, stores) = return (store, gamma (cxt, store, v), l, stores)
  | eval (cxt, store, NCL.LetVal (x, v, e), l, stores) =
    let
      val l' = allocK x
      val store' = storeInsert (store, l', PowVal.singleton (Frame (x, cxt, e, l)))
    in
      return (store', gamma (cxt, store, v), l', stores)
    end
  | eval (cxt, store, NCL.Let (x, f, e), l, stores) = comp (cxt, store, f, x, e, l, stores)
  | eval (cxt, store, NCL.If (_, e1, e2), l, stores) =
    let
      val stores' = eval (cxt, store, e1, l, stores)
    in
      eval (cxt, store, e2, l, stores')
    end

val kont0 = allocK ""
val store0 = ValMap.insert (ValMap.empty, kont0, PowVal.singleton Halt)
val stores0 = Stores.add (Stores.empty, ("", store0))

fun test expr =
  let
    val res' = Stores.listItems (eval (EnvStr.empty, store0, expr, kont0, stores0))
    val res = List.map (fn (x,st) => (x, ValMap.listItemsi st)) res'

    val varEnv = getAllocMap ()
    val kontEnv = getAllocKMap ()
  in
    print ("program\n" ^ NCL.toString expr ^ "\n=====\n");
    print ("env:  [" ^ String.concatWith ", " (List.map (fn (k,v) => k ^ " => " ^ Int.toString v) varEnv) ^ "]\n");
    print ("kenv: [" ^ String.concatWith ", " (List.map (fn (k,v) => k ^ " => " ^ Int.toString v) kontEnv) ^ "]\n");
    print ("[\n" ^ String.concatWith ",\n\n" (List.map (fn (x, st) => x ^ ":\n" ^ ValMap.toString st) res) ^ "\n]\n")
  end

(* To enable stack trace:
   http://blog.clawpaws.net/post/2007/02/16/Getting-Backtraces-with-Standard-ML
    CM.make "$smlnj-tdp/back-trace.cm";
    SMLofNJ.Internals.TDP.mode := true;
 *)

end
