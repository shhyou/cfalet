structure CFA1 =
struct

fun pairCollate (cmp1, cmp2) = fn ((a1, b1), (a2, b2)) =>
  case cmp1 (a1, a2) of
      EQUAL => cmp2 (b1, b2)
    | unequ => unequ

type addr = int

type env = addr EnvStr.t

datatype storable =
    (* Value *)
    Unit
  | Int
  | Bool
  | Ref of addr
  | Cl of string * env * string * NCL.t
    (* Kont *)
  | Frame of string * env * NCL.t * addr * string
  | Halt

fun toString Unit = "<Unit>"
  | toString Int = "<Int>"
  | toString Bool = "<Bool>"
  | toString (Ref l) = "<Ref " ^ Int.toString l ^ ">"
  | toString (Cl (tag, env, x, e)) = "<Cl " ^ tag ^ ">"
  | toString (Frame (x, env, e, l, _)) = "<Frame " ^ x ^ "/" ^ Int.toString l ^ ">"
  | toString Halt = "<Halt>"

structure PowVal = struct
  structure Set = BinarySetFn(struct
    type ord_key = storable

    fun getPriority Unit = 0
      | getPriority Int = 1
      | getPriority Bool = 2
      | getPriority (Ref _) = 3
      | getPriority (Cl _) = 4
      | getPriority (Frame _) = 5
      | getPriority Halt = 6

    fun compare (Unit, Unit) = EQUAL
      | compare (Int, Int) = EQUAL
      | compare (Bool, Bool) = EQUAL
      | compare (Ref l1, Ref l2) = Int.compare (l1, l2)
      | compare (Cl (f, _, _, _), Cl (g, _, _, _)) = String.compare (f, g)
      | compare (Frame (x1, _, _, l1, _), Frame (x2, _, _, l2, _)) =
          pairCollate (String.compare, Int.compare) ((x1,l1), (x2,l2))
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
  structure EnvMap = BinaryMapFn(struct
    type ord_key = string * string
    val compare = pairCollate (String.compare, String.compare)
  end)

  val cnt = ref 0
  fun alloc_map map =
    ( fn () => EnvMap.listItemsi (!map)
    , fn () => (map := EnvMap.insert (EnvMap.empty, ("", ""), 0); cnt := 1)
    , fn key =>
        case EnvMap.find (!map, key) of
            SOME addr => addr
          | NONE =>
            let
              val addr = !cnt
            in
              cnt := !cnt + 1;
              map := EnvMap.insert (!map, key, addr);
              addr
            end )
in
  val (getAllocMap, reset, alloc) = alloc_map (ref EnvMap.empty)
  val (getAllocKMap, resetK, allocK) = alloc_map (ref EnvMap.empty)
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

fun gamma (cxt, store, NCL.Unit) = PowVal.singleton Unit
  | gamma (cxt, store, NCL.Int _) = PowVal.singleton Int
  | gamma (cxt, store, NCL.Bool _) = PowVal.singleton Bool
  | gamma (cxt, store, NCL.Var x) = valOf (ValMap.find (store, EnvStr.lookup x cxt))

(*  return : <sigma, vs, l> -> { <Gamma', sigma', e, l'> | <frame Gamma, x, e, l'> in sigma(l) } *)
fun return (cxt, store, values, y, e', kont, y') =
  let
    val l = alloc (y, y')
    val (dirty, store') = storeInsert (store, l, values)
  in
    eval (EnvStr.extend (y, l) cxt, store', e', kont, y')
  end

and comp (cxt, store, expr, y, e', l, y') =
  let
    fun doIt (NCL.Lam (x, e)) = return (cxt, store, PowVal.singleton (Cl (y, cxt, x, e)), y, e', l, y')
      | doIt (NCL.Ap (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          val l' = allocK (y, y')
          val (dirty, store') = storeInsert (store, l', PowVal.singleton (Frame (y, cxt, e', l, y')))

          fun collectEval (cl as Cl (_, cxt', x', e''), store'') =
              let
                val l'' = alloc (x', y)
                val (dirty', store''') = storeInsert (store'', l'', v2')
              in
                if dirty orelse dirty'
                then eval (EnvStr.extend (x', l'') cxt', store''', e'', l', y)
                else store''
              end
            | collectEval (_, store'') = store''
        in
          PowVal.foldl collectEval store' v1'
        end
      | doIt (NCL.Ref (tag, v)) =
        let
          val l'' = alloc (tag, y')
          val (_, store'') = storeInsert (store, l'', gamma (cxt, store, v))
        in
          return (cxt, store'', PowVal.singleton (Ref l''), y, e', l, y')
        end
      | doIt (NCL.Deref v) =
        let
          val getVals = List.map (fn (Ref l) => valOf (ValMap.find (store, l))
                                   | _ => PowVal.empty)
          val getVal = List.foldl PowVal.union PowVal.empty o getVals
        in
          return (cxt, store, getVal (PowVal.listItems (gamma (cxt, store, v))), y, e', l, y')
        end
      | doIt (NCL.Set (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          val store'' = PowVal.foldl (fn (Ref l, st) => #2 (storeInsert (st, l, v2'))
                                       | (_, st) => st)
                                     store v1'
        in
          return (cxt, store'', PowVal.singleton Unit, y, e', l, y')
        end
  in
    doIt expr
  end

and eval (cxt, store, NCL.Value v, l, y') =
    let
      val values = gamma (cxt, store, v)
      fun collectEval (Frame (x, cxt, e, kont', y''), store') =
          let
            val l' = alloc (x, y'')
            val (_, store'') = storeInsert (store', l', values)
          in
            eval (EnvStr.extend (x, l') cxt, store'', e, kont', y'')
          end
        | collectEval (Halt, store') = store'
        | collectEval _ = raise NotContinuation

      val konts = valOf (ValMap.find (store, l))
    in
      PowVal.foldl collectEval store konts
    end
  | eval (cxt, store, NCL.LetVal (x, v, e), l, y') = return (cxt, store, gamma (cxt, store, v), x, e, l, y')
  | eval (cxt, store, NCL.Let (x, f, e), l, y') = comp (cxt, store, f, x, e, l, y')
  | eval (cxt, store, NCL.If (_, e1, e2), l, y') =
    let
      val store' = eval (cxt, store, e1, l, y')
    in
      eval (cxt, store', e2, l, y')
    end

val kont0 = allocK ("", "")
val store0 = ValMap.insert (ValMap.empty, kont0, PowVal.singleton Halt)

fun test expr =
  let
    val store = eval (EnvStr.empty, store0, expr, kont0, "")
    val res = ValMap.listItemsi store

    val strippedRes =
      let
        val pointedAddr =
          List.mapPartial (fn (Ref addr) => SOME addr
                            | _ => NONE)
                          (List.concat (List.map (PowVal.listItems o #2) res))
      in
        List.filter (fn (addr, v) => List.exists (fn m => addr = m) pointedAddr
                              orelse List.exists (fn Halt => true
                                                   | Frame _ => true
                                                   | _ => false)
                                                 (PowVal.listItems v))
                    res
      end

    fun markedVars (v as NCL.Value _) = []
      | markedVars (NCL.LetVal (x, v, e)) = x::markedVars e
      | markedVars (NCL.Let (x, NCL.Lam (y, e1), e2)) =
          x::y::(markedVars e1@markedVars e2)
      | markedVars (NCL.Let (x, f, e2)) = x::markedVars e2
      | markedVars (NCL.If (v, e1, e2)) = markedVars e1@markedVars e2

    val vars = ListMergeSort.uniqueSort String.compare (markedVars expr)

    val varEnv = getAllocMap () (* already sorted in increasing key order *)

    val varVals = EnvStr.fromList (List.map (fn v =>
      (v, List.mapPartial (fn ((v', l), l') =>
            if v = v'
            then SOME (l, valOf (ValMap.find (store, l')))
            else NONE) varEnv)) vars)


    fun mark x =
      let
        fun index (v, []) = raise Undefined
          | index (v, (v', _)::vs) =
              if v = v' then 0 else 1 + index (v, vs)
      in
        String.concat ["{{", Int.toString (index (x, varVals)), "|", x, "}}"]
          handle EnvStr.NotFound _ => x
      end

    fun markCode (v as NCL.Value _) = v
      | markCode (NCL.LetVal (x, v, e)) = NCL.LetVal (mark x, v, markCode e)
      | markCode (NCL.Let (x, NCL.Lam (y, e1), e2)) =
          NCL.Let (mark x, NCL.Lam (mark y, markCode e1), markCode e2)
      | markCode (NCL.Let (x, f, e2)) = NCL.Let (mark x, f, markCode e2)
      | markCode (NCL.If (v, e1, e2)) = NCL.If (v, markCode e1, markCode e2)

  in
    print (NCL.toString (markCode expr) ^ "\n=====\n");
    print (ValMap.toString strippedRes ^ "\n=====\n");
    List.app (fn (k, vs) =>
      (List.app (fn (y', v) =>
        (print (y' ^ ":\n");
         print (PowVal.toString v ^ "\n");
         print "\n")) vs;
       print "---\n")) varVals
  end

(* To enable stack trace:
   http://blog.clawpaws.net/post/2007/02/16/Getting-Backtraces-with-Standard-ML
    CM.make "$smlnj-tdp/back-trace.cm";
    SMLofNJ.Internals.TDP.mode := true;
 *)

end
