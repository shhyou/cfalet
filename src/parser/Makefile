EXE = .exe
RM = del /Q

.PHONY: all clean

parser: parser.grm lexer.lex
	mllex lexer.lex
	mlyacc parser.grm

all: parser$(EXE)

clean:
	$(RM) parser.exe parser.grm.sig parser.grm.sml parser.grm.desc lexer.lex.sml

parser$(EXE): parser ../wrappers/parser.mlb ../parser.sml ../wrappers/parser-main.sml ../wrappers/parser-mlton.sml
	mlton -verbose 2 ../wrappers/parser.mlb
	move ../wrappers/parser$(EXE) .
