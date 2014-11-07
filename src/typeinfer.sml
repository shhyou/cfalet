(* For compilation instructions/usages, see wrappers/typeinfer-main.sml *)

signature ENV =
sig
  type 'a t
  exception NotFound of string

  val empty : 'a t
  val extend : string * 'a -> 'a t -> 'a t
  val lookup : string -> 'a t -> 'a
  val member : string -> 'a t -> bool

  val fromList : (string * 'a) list -> 'a t
  val toString : ('a -> string) -> 'a t -> string
end

structure Env : ENV =
struct
  type 'a t = (string * 'a) list
  exception NotFound of string

  val empty = []

  fun extend pr = fn xs => pr :: xs

  fun lookup key [] = raise (NotFound key)
    | lookup key ((k, v)::xs) =
        if key = k
        then v
        else lookup key xs

  fun member key [] = false
    | member key ((k,v)::xs) = (key = k) orelse member key xs

  val fromList = fn xs => xs

  fun toString show xs =
      "[" ^ String.concatWith ", "
              (List.map (fn (x, v) => "(" ^ x ^ "," ^ show v ^ ")") xs)
    ^ "]"
end

structure Typeinfer =
struct

exception Undefined

(*  fresh : unit -> Type.t *)
val fresh =
  let
    val cnt = ref 0
  in
    fn () =>
      let
        val v = "'t" ^ Int.toString (!cnt)
      in
        cnt := !cnt + 1;
        Type.Var (ref (Type.Unlink v))
      end
  end

(*  instantiate : Type.tyscheme -> Type.t *)
fun instantiate (Type.Mono t) = t
  | instantiate (Type.Poly (vs, t)) =
      let
        val vars = Env.fromList (List.map (fn x => (x, fresh ())) vs)

        fun renew (Type.Var (var as ref (Type.Unlink x))) =
              (Env.lookup x vars handle Env.NotFound _ => Type.Var var)
          | renew (Type.Var (ref (Type.Link t))) = renew t
          | renew (Type.Arrow (t1, t2)) = Type.Arrow (renew t1, renew t2)
          | renew t = t (* Unit, Int, Bool *)
      in
        renew t
      end

(*  occurs : string * Type.t -> bool *)
fun occurs (x, Type.Var (ref (Type.Unlink y))) = x = y
  | occurs (x, Type.Var (ref (Type.Link t))) = occurs (x, t)
  | occurs (x, Type.Arrow (t1, t2)) = occurs (x, t1) orelse occurs (x, t2)
  | occurs _ = false

exception OccurCheck of string * Type.t
exception NoUnify of Type.t * Type.t

(*  unify : Type.t * Type.t -> unit *)
fun unify (Type.Unit, Type.Unit) = ()
  | unify (Type.Int, Type.Int) = ()
  | unify (Type.Bool, Type.Bool) = ()
  | unify (Type.Var        (ref (Type.Link t1)), t2) = unify (t1, t2)
  | unify (Type.Var (var as ref (Type.Unlink x1)), t2) =
      if occurs (x1, t2)
      then raise (OccurCheck (x1, t2))
      else var := Type.Link t2
  | unify (t1,       Type.Var (ref (Type.Link t2))) = unify (t1, t2)
  | unify (t1, t2 as Type.Var (ref (Type.Unlink _))) = unify (t2, t1)
  | unify (Type.Arrow (dom1, codom1), Type.Arrow (dom2, codom2)) =
      (unify (dom1, dom2);
       unify (codom1, codom2))
  | unify ts = raise (NoUnify ts)

(*  generalize : Type.tyscheme Env.t * Type.t -> Type.tyscheme *)
fun generalize (cxt, t) =
  let
    val maps = ref Env.empty
    val vars = ref []

    fun genVars (Type.Var (ref (Type.Unlink x))) =
          if Env.member x (!maps) orelse Env.member x cxt
          then ()
          else let
            val v = fresh ()                                        (* impossible *)
            val y = case v of Type.Var (ref (Type.Unlink y)) => y | _ => raise Undefined
          in
            maps := Env.extend (x,v) (!maps);
            vars := y::(!vars)
          end
      | genVars (Type.Var (ref (Type.Link t))) = genVars t
      | genVars (Type.Arrow (t1, t2)) = (genVars t1; genVars t2)
      | genVars _ = () (* Unit, Int, Bool *)

    fun renew (t as Type.Var (ref (Type.Unlink x))) =
          (Env.lookup x (!maps) handle Env.NotFound _ => t)
      | renew (Type.Var (ref (Type.Link t))) = t
      | renew (Type.Arrow (t1, t2)) = Type.Arrow (renew t1, renew t2)
      | renew t = t (* Unit, Int, Bool *)
  in
    genVars t;
    if null (!vars) then Type.Mono t else Type.Poly (!vars, renew t)
  end

(*  infer : Type.tyscheme Env.t * AST.t -> Type.t *)
fun infer (cxt, AST.Value AST.Unit) = Type.Unit
  | infer (cxt, AST.Value (AST.Int n)) = Type.Int
  | infer (cxt, AST.Value (AST.Bool b)) = Type.Bool
  | infer (cxt, AST.Value (AST.Var x)) = instantiate (Env.lookup x cxt)
  | infer (cxt, AST.Value (AST.Lam (x, e))) =
      let
        val a = fresh ()   (* lam : a -> b *)
      in
        Type.Arrow (a, infer (Env.extend (x, Type.Mono a) cxt, e))
      end
  | infer (cxt, AST.Ap (e1, e2)) =
      let
        val b = fresh ()
        val t1 = infer (cxt, e1)
        val a = infer (cxt, e2)
      in
        unify (Type.Arrow (a, b), t1);
        b
      end
  | infer (cxt, AST.Let (x, v as AST.Value _, e)) =
      let
        val t = infer (cxt, v)
      in
        infer (Env.extend (x, generalize (cxt, t)) cxt, e)
      end
  | infer (cxt, AST.Let (x, e1, e2)) =
      let
        val t1 = infer (cxt, e1)
      in
        infer (Env.extend (x, Type.Mono t1) cxt, e2)
      end
  | infer (cxt, AST.If (e1, e2, e3)) =
      let
        val t1 = infer (cxt, e1)
        val t2 = infer (cxt, e2)
        val t3 = infer (cxt, e3)
      in
        unify (Type.Bool, t1);
        unify (t2, t3);
        t2
      end
  | infer (cxt, AST.Ref e) =
      let
        val t = infer (cxt, e)
      in
        Type.Ref t
      end
  | infer (cxt, AST.Deref e) =
      let
        val t = infer (cxt, e)
        val a = fresh ()
      in
        unify (Type.Ref a, t);
        a
      end
  | infer (cxt, AST.Set (e1, e2)) =
      let
        val t1 = infer (cxt, e1)
        val t2 = infer (cxt, e2)
      in
        unify (t1, Type.Ref t2);
        Type.Unit
      end

end
