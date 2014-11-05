(*
    // http://stackoverflow.com/questions/5053149/

    Compilation instruction for SML/NJ:
      > ml-build typeinfer.cm Main.main typeinfer
      > sml @SMLload typeinfer.x86-win32

    Compilation instruction for MLton:
      > mlton typeinfer.mlb

    Usage in SML/NJ REPL:
      - CM.make "typeinfer.cm"
      - Typeinfer.infer;
      val it = fn : 'a -> AST.t -> 'b
      -
*)

structure Main =
struct

(*  main : string * string list -> OS.Process.status *)
fun main (prog_name, args) =
  ( print "hello, world!\n";
    print "name: "; print prog_name;
    print "\nargs: "; List.app (fn s => (print s; print " ")) args;
    print "\n";
    print (Type.toString (Typeinfer.infer (Env.empty, AST.Value (AST.Int 5)))); print "\n";
    print (Type.toString (Typeinfer.infer (Env.empty, AST.Value (AST.Var "x")))); print "\n";
    OS.Process.success)

fun mainWrapped () =
  main (CommandLine.name (), CommandLine.arguments ())

end
