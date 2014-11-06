EXE = .exe
RM = del /Q

.PHONY: all clean

all: parser$(EXE)

clean:
	$(RM) parser.exe parser.grm.sig parser.grm.sml parser.grm.desc lexer.lex.sml

parser$(EXE): parser.grm lexer.lex parser-mlton.sml parser.mlb
	mllex lexer.lex
	mlyacc parser.grm
	mlton -verbose 2 parser.mlb
