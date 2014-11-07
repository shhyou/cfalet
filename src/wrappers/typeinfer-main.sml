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
  ( print (AST.toString AST.p0 ^ "\n");
    print (Type.toString (Typeinfer.infer (Env.empty, AST.p0)) ^ "\n=====\n");
    print (AST.toString AST.p1 ^ "\n");
    print (Type.toString (Typeinfer.infer (Env.empty, AST.p1)) ^ "\n=====\n");
    print "input any program:\n";
    print (Type.toString (Typeinfer.infer (Env.empty, Parser0.parse TextIO.stdIn)) ^ "\n");
    OS.Process.success)

fun mainWrapped () =
  main (CommandLine.name (), CommandLine.arguments ())

end
