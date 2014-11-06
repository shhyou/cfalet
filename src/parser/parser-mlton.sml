(* Parser0.sml *)

(* This file provides glue code for building the Parser0ulator using the
 * parser and lexer specified in Parser0.lex and Parser0.grm.
*)

structure Parser0 =
struct

(*
 * We apply the functors generated from Parser0.lex and Parser0.grm to produce
 * the Parser0Parser structure.
 *)

  structure Parser0LrVals =
    Parser0LrValsFun(structure Token = LrParser.Token)

  structure Parser0Lex =
    Parser0LexFun(structure Tokens = Parser0LrVals.Tokens)

  structure Parser0Parser =
    Join(structure LrParser = LrParser
         structure ParserData = Parser0LrVals.ParserData
         structure Lex = Parser0Lex)

(*
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
              TextIO.output(TextIO.stdOut,
                            "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in Parser0Parser.parse(0,lexstream,print_error,())
      end

(*
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the Parser0ulator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse () =
    let
      val lexer = Parser0Parser.makeLexer (fn _ => TextIO.input TextIO.stdIn)
      val (result,lexer) = invoke lexer
    in
      TextIO.output(TextIO.stdOut, "parse result\n=====\n" ^ (AST.toString result) ^ "\n")
    end

end (* structure Parser0 *)

val () = Parser0.parse ()
