(* This file is modified from MLton/mlyacc/examples/calc.lex *)

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val pos = ref 0

fun eof () = Tokens.EOF (!pos,!pos)

fun error (e,l : int, _) = TextIO.output (TextIO.stdOut, String.concat[
        "line ", (Int.toString l), ": ", e, "\n"
      ])

%%

%header (functor Parser0LexFun(structure Tokens: Parser0_TOKENS));

alund = [A-Za-z_];
digit = [0-9];
ws = [\ \t];

%%
\n       => (pos := (!pos) + 1; lex ());
{ws}+    => (lex ());
{digit}+ => (Tokens.INT (valOf (Int.fromString yytext), !pos, !pos));
"()"     => (Tokens.UNIT (!pos, !pos));
"("      => (Tokens.LPAREN (!pos, !pos));
")"      => (Tokens.RPAREN (!pos, !pos));
"true"   => (Tokens.BOOL (true, !pos, !pos));
"false"  => (Tokens.BOOL (false, !pos, !pos));
"fn"     => (Tokens.FN (!pos, !pos));
"=>"     => (Tokens.MAPSTO (!pos, !pos));
"let"    => (Tokens.LET (!pos, !pos));
"="      => (Tokens.EQUALS (!pos, !pos));
"in"     => (Tokens.IN (!pos, !pos));
"if"     => (Tokens.IF (!pos, !pos));
"then"   => (Tokens.THEN (!pos, !pos));
"else"   => (Tokens.ELSE (!pos, !pos));
{alund}({alund}|{digit})* => (Tokens.VAR (yytext, !pos, !pos));
"!"      => (Tokens.DEREF (!pos, !pos));
"+"      => (Tokens.PLUS (!pos, !pos));
"*"      => (Tokens.TIMES (!pos, !pos));
"-"      => (Tokens.MINUS (!pos, !pos));
"/"      => (Tokens.DIV (!pos, !pos));
":="     => (Tokens.ASSIGN (!pos, !pos));
.        => (error ("ignoring bad character " ^ yytext, !pos, !pos);
             lex ());
