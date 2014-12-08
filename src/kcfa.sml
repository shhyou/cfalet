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
      NONE => (true, ValMap.insert item)
    | SOME values' =>
      let
        val values'' = PowVal.union (values, values')
      in
        case PowVal.compare (values', values'') of
            EQUAL => (false, store)
          | unequ => (true, ValMap.insert (store, addr, values''))
      end

fun gamma (cxt, store, NCL.Var x) = valOf (ValMap.find (store, EnvStr.lookup x cxt))
  | gamma _ = PowVal.singleton Const

(*  return : <sigma, vs, l> -> { <Gamma', sigma', e, l'> | <frame Gamma, x, e, l'> in sigma(l) } *)
fun return (cxt, store, values, y, e', kont) =
  let
    val l = alloc y
    val (dirty, store') = storeInsert (store, l, values)
  in
    eval (EnvStr.extend (y, l) cxt, store', e', kont)
  end

and comp (cxt, store, expr, y, e', l) =
  let
    fun doIt (NCL.Lam (x, e)) = return (cxt, store, PowVal.singleton (Cl (y, cxt, x, e)), y, e', l)
      | doIt (NCL.Ap (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          val l' = allocK y
          val (dirty, store') = storeInsert (store, l', PowVal.singleton (Frame (y, cxt, e', l)))

          fun collectEval (cl as Cl (_, cxt', x', e''), store'') =
              let
                val l'' = alloc x'
                val (dirty', store''') = storeInsert (store'', l'', v2')
              in
                if dirty orelse dirty'
                then eval (EnvStr.extend (x', l'') cxt', store''', e'', l')
                else store''
              end
            | collectEval (_, store'') = store''
        in
          PowVal.foldl collectEval store' v1'
        end
      | doIt (NCL.Ref (tag, v)) =
        let
          val l'' = alloc tag
          val (_, store'') = storeInsert (store, l'', gamma (cxt, store, v))
        in
          return (cxt, store'', PowVal.singleton (Ref l''), y, e', l)
        end
      | doIt (NCL.Deref v) =
        let
          val getVals = List.map (fn (Ref l) => valOf (ValMap.find (store, l))
                                   | _ => PowVal.empty)
          val getVal = List.foldl PowVal.union PowVal.empty o getVals
        in
          return (cxt, store, getVal (PowVal.listItems (gamma (cxt, store, v))), y, e', l)
        end
      | doIt (NCL.Set (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          val store'' = PowVal.foldl (fn (Ref l, st) => #2 (storeInsert (st, l, v2'))
                                       | (_, st) => st)
                                     store v1'
        in
          return (cxt, store'', PowVal.singleton Const, y, e', l)
        end
  in
    doIt expr
  end

and eval (cxt, store, NCL.Value v, l) =
    let
      val values = gamma (cxt, store, v)
      fun collectEval (Frame (x, cxt, e, kont'), store') =
          let
            val l = alloc x
            val (_, store'') = storeInsert (store', l, values)
          in
            eval (EnvStr.extend (x, l) cxt, store'', e, kont')
          end
        | collectEval (Halt, store') = store'
        | collectEval _ = raise NotContinuation

      val konts = valOf (ValMap.find (store, l))
    in
      PowVal.foldl collectEval store konts
    end
  | eval (cxt, store, NCL.LetVal (x, v, e), l) = return (cxt, store, gamma (cxt, store, v), x, e, l)
  | eval (cxt, store, NCL.Let (x, f, e), l) = comp (cxt, store, f, x, e, l)
  | eval (cxt, store, NCL.If (_, e1, e2), l) =
    let
      val store' = eval (cxt, store, e1, l)
    in
      eval (cxt, store', e2, l)
    end

val kont0 = allocK ""
val store0 = ValMap.insert (ValMap.empty, kont0, PowVal.singleton Halt)

fun test expr =
  let
    val res = ValMap.listItemsi (eval (EnvStr.empty, store0, expr, kont0))

    val varEnv = getAllocMap ()
    val kontEnv = getAllocKMap ()

    val strippedRes =
      let
        fun lookup x = [EnvStr.lookup x varEnv] handle EnvStr.NotFound _ => []

        fun vars (NCL.Value _) = []
          | vars (NCL.LetVal (x, _, _)) = lookup x
          | vars (NCL.Let (x, NCL.Lam (y, e1), e2)) =
              List.concat [lookup x, lookup y, vars e1, vars e2]
          | vars (NCL.Let (x, _, e2)) = lookup x@vars e2
          | vars (NCL.If (_, e1, e2)) = vars e1@vars e2

        val excludeAddrs = vars expr
        fun someEq n = List.exists (fn m => m = n) excludeAddrs
      in
        List.filter (not o someEq o #1) res
      end

    fun mark x =
      String.concat ["{{", Int.toString (EnvStr.lookup x varEnv), "|", x, "}}"]
        handle EnvStr.NotFound _ => x

    fun markCode (v as NCL.Value _) = v
      | markCode (NCL.LetVal (x, v, e)) = NCL.LetVal (mark x, v, markCode e)
      | markCode (NCL.Let (x, NCL.Lam (y, e1), e2)) =
          NCL.Let (mark x, NCL.Lam (mark y, markCode e1), markCode e2)
      | markCode (NCL.Let (x, f, e2)) = NCL.Let (mark x, f, markCode e2)
      | markCode (NCL.If (v, e1, e2)) = NCL.If (v, markCode e1, markCode e2)

  in
    print (NCL.toString (markCode expr) ^ "\n=====\n");
    print (ValMap.toString strippedRes ^ "\n=====\n");
    List.app (fn (k, v) =>
      print (PowVal.toString v ^ "\n---\n")) res
  end

(* To enable stack trace:
   http://blog.clawpaws.net/post/2007/02/16/Getting-Backtraces-with-Standard-ML
    CM.make "$smlnj-tdp/back-trace.cm";
    SMLofNJ.Internals.TDP.mode := true;
 *)

end
