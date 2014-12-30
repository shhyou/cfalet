structure CFAK =
struct

(* the limit of length of the contour *)
val kLimit = 1

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
  | Frame of string * env * NCL.t * addr * string list (* contour *)
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
      | compare (Cl (f, env1, _, _), Cl (g, env2, _, _)) =
          pairCollate (String.compare, EnvStr.collate Int.compare) ((f, env1), (f, env2)) 
      | compare (Frame (x1, env1, _, l1, _), Frame (x2, env2, _, l2, _)) =
          pairCollate (pairCollate (String.compare, Int.compare), EnvStr.collate Int.compare)
            (((x1,l1), env1), ((x2,l2), env2))
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

type 'a state = { expr: 'a
                , environment : env
                , store : store
                , cont : addr
                , contour : string list }

local
  structure EnvMap = BinaryMapFn(struct
    type ord_key = string * string list
    fun compareList (0, _, _) = EQUAL
      | compareList (_, [], []) = EQUAL
      | compareList (n, [], _::_) = LESS
      | compareList (n, _::_, []) = GREATER
      | compareList (n, s1::ss1, s2::ss2) =
          case String.compare (s1, s2) of
              EQUAL => compareList (n-1, ss1, ss2)
            | unequ => unequ
    val compare = pairCollate (String.compare, fn (ss1, ss2) => compareList (kLimit, ss1, ss2))
  end)

  val cnt = ref 0
  fun alloc_map map =
    ( fn () => EnvMap.listItemsi (!map)
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
  val (getAllocMap, alloc) = alloc_map (ref EnvMap.empty)
  val (getAllocKMap, allocK) = alloc_map (ref EnvMap.empty)
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
fun return ({ expr = e'
            , environment = cxt
            , store = store
            , cont = l
            , contour = contr }, values, y) =
  let
    val l' = alloc (y, contr)
    val (dirty, store') = storeInsert (store, l', values)
  in
    eval { expr = e'
         , environment = EnvStr.extend (y, l') cxt
         , store = store'
         , cont = l
         , contour = contr }
  end

and comp ({ expr = expr
          , environment = cxt
          , store = store
          , cont = l
          , contour = contr }, y, e') =
  let
    fun doIt (NCL.Lam (x, e)) = return ({ expr = e'
                                        , environment = cxt
                                        , store = store
                                        , cont = l
                                        , contour = contr }, PowVal.singleton (Cl (y, cxt, x, e)), y)
      | doIt (NCL.Ap (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          val l' = allocK (y, contr)
          val (dirty, store') = storeInsert (store, l', PowVal.singleton (Frame (y, cxt, e', l, contr)))
          val contr' = y::contr

          fun collectEval (cl as Cl (_, cxt', x', e''), store'') =
              let
                val l'' = alloc (x', contr')
                val (dirty', store''') = storeInsert (store'', l'', v2')
              in
                if dirty orelse dirty'
                then eval { expr = e''
                          , environment = EnvStr.extend (x', l'') cxt'
                          , store = store'''
                          , cont = l'
                          , contour = contr' }
                else store''
              end
            | collectEval (_, store'') = store''
        in
          PowVal.foldl collectEval store' v1'
        end
      | doIt (NCL.Ref (tag, v)) =
        let
          val l'' = alloc (tag, contr)
          val (_, store'') = storeInsert (store, l'', gamma (cxt, store, v))
        in
          return ({ expr = e'
                  , environment = cxt
                  , store = store''
                  , cont = l
                  , contour = contr }, PowVal.singleton (Ref l''), y)
        end
      | doIt (NCL.Deref v) =
        let
          val getVals = List.map (fn (Ref l) => valOf (ValMap.find (store, l))
                                   | _ => PowVal.empty)
          val getVal = List.foldl PowVal.union PowVal.empty o getVals
        in
          return ({ expr = e'
                  , environment = cxt
                  , store = store
                  , cont = l
                  , contour = contr }, getVal (PowVal.listItems (gamma (cxt, store, v))), y)
        end
      | doIt (NCL.Set (v1, v2)) =
        let
          val (v1', v2') = (gamma (cxt, store, v1), gamma (cxt, store, v2))
          val store'' = PowVal.foldl (fn (Ref l, st) => #2 (storeInsert (st, l, v2'))
                                       | (_, st) => st)
                                     store v1'
        in
          return ({ expr = e'
                  , environment = cxt
                  , store = store''
                  , cont = l
                  , contour = contr }, PowVal.singleton Unit, y)
        end
  in
    doIt expr
  end

and eval { expr = NCL.Value v
         , environment = cxt
         , store = store
         , cont = l
         , contour = contr } =
    let
      val values = gamma (cxt, store, v)
      fun collectEval (Frame (x, cxt, e, kont', contr'), store') =
          let
            val l' = alloc (x, contr')
            val (_, store'') = storeInsert (store', l', values)
          in
            eval { expr = e
                 , environment = EnvStr.extend (x, l') cxt
                 , store = store''
                 , cont = kont'
                 , contour = contr' }
          end
        | collectEval (Halt, store') = store'
        | collectEval _ = raise NotContinuation

      val konts = valOf (ValMap.find (store, l))
    in
      PowVal.foldl collectEval store konts
    end
  | eval { expr = NCL.LetVal (x, v, e)
         , environment = cxt
         , store = store
         , cont = l
         , contour = contr } = return ({ expr = e
                                       , environment = cxt
                                       , store = store
                                       , cont = l
                                       , contour = contr }, gamma (cxt, store, v), x)
  | eval { expr = NCL.Let (x, f, e)
         , environment = cxt
         , store = store
         , cont = l
         , contour = contr } = comp ({ expr = f
                                     , environment = cxt
                                     , store = store
                                     , cont = l
                                     , contour = contr }, x, e)
  | eval { expr = NCL.If (_, e1, e2)
         , environment = cxt
         , store = store
         , cont = l
         , contour = contr } = eval { expr = e1
                                    , environment = cxt
                                    , store = eval { expr = e2
                                                   , environment = cxt
                                                   , store = store
                                                   , cont = l
                                                   , contour = contr }
                                    , cont = l
                                    , contour = contr }

val kont0 = allocK ("", [])
val store0 = ValMap.insert (ValMap.empty, kont0, PowVal.singleton Halt)

fun test expr =
  let
    val store = eval { expr = expr
                     , environment = EnvStr.empty
                     , store = store0
                     , cont = kont0
                     , contour = [] }
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

    val varVals = List.map (fn v =>
      (v, List.mapPartial (fn ((v', contr), l) =>
            if v = v'
            then SOME (contr, valOf (ValMap.find (store, l)))
            else NONE) varEnv)) vars

    fun mark x =
      let
        fun index (v, []) = raise Undefined
          | index (v, (v', _)::vs) =
              if v = v' then 0 else 1 + index (v, vs)
      in
        String.concat ["{{", Int.toString (index (x, varVals)), "|", x, "}}"]
      end

    fun markCode (v as NCL.Value _) = v
      | markCode (NCL.LetVal (x, v, e)) = NCL.LetVal (mark x, v, markCode e)
      | markCode (NCL.Let (x, NCL.Lam (y, e1), e2)) =
          NCL.Let (mark x, NCL.Lam (mark y, markCode e1), markCode e2)
      | markCode (NCL.Let (x, f, e2)) = NCL.Let (mark x, f, markCode e2)
      | markCode (NCL.If (v, e1, e2)) = NCL.If (v, markCode e1, markCode e2)

    fun cutContour (_, []) = []
      | cutContour (0, _::_) = ["..."]
      | cutContour (n, s::ss) = s::cutContour (n-1, ss)
  in
    print (NCL.toString (markCode expr) ^ "\n=====\n");
    print (ValMap.toString strippedRes ^ "\n=====\n");
    List.app (fn (k, vs) =>
      (List.app (fn (contr, v) =>
        (print (String.concatWith "::" (List.rev (cutContour (kLimit, contr))) ^ ":\n");
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
