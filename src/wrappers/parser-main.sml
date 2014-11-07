structure Main =
struct

datatype ParseResult = Success of AST.t | Failed of string

fun main (prog_name, args) =
  let
    val input = fn _ => TextIO.input TextIO.stdIn
    val result = Success (Parser0.parse input)
                  handle Parser0.ParseError (msg, pos) =>
                    Failed ("line " ^ (Int.toString pos) ^ ", " ^ msg)
  in
    (case result of
        Success e => print ("parse result\n=====\n" ^ AST.toString e ^ "\n")
      | Failed msg => print ("Error: " ^ msg ^ "\n"));
    OS.Process.success
  end

fun mainWrapped () =
  main (CommandLine.name (), CommandLine.arguments ())

end
