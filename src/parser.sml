(* This file is modified from MLton/mlyacc/examples/calc.sml *)

structure Parser0 =
struct

structure Parser0LrVals =
  Parser0LrValsFun(structure Token = LrParser.Token)

structure Parser0Lex =
  Parser0LexFun(structure Tokens = Parser0LrVals.Tokens)

structure Parser0Parser =
  Join(structure LrParser = LrParser
       structure ParserData = Parser0LrVals.ParserData
       structure Lex = Parser0Lex)

exception ParseError of string * int

(*  parse : (int -> string) -> AST.t, raises ParseError *)
fun parse ins =
  let
    fun parseError (s, i, _) = raise (ParseError (s, i))
    fun input _ = TextIO.input ins
  in
    #1 (Parser0Parser.parse (0, Parser0Parser.makeLexer input, parseError, ()))
  end

end
