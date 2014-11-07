EXE = .exe
RM = del /Q

.PHONY: all clean

parser: parser.grm lexer.lex ../parser.sml parser.mlb

all: parser$(EXE)

clean:
	$(RM) parser.exe parser.grm.sig parser.grm.sml parser.grm.desc lexer.lex.sml

parser$(EXE):
	mllex lexer.lex
	mlyacc parser.grm
	mlton -verbose 2 parser.mlb
