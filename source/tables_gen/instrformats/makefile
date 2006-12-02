all : build

instr_gen : lex.yy.c y.tab.h y.tab.c instrgen-c.o
	gcc -o instr_gen lex.yy.c y.tab.c instrgen-c.o

lex.yy.c : instrformats.l
	flex instrformats.l

y.tab.c y.tab.h : instrformats.y
	yacc -d instrformats.y

instrgen-c.o : instrgen-c.h instrgen-c.c
	gcc -c instrgen-c.c

build : instr_gen
	instr_gen 

clean :
	rm instr_gen y.tab.* lex.yy.c *.o

clean_all :
	rm instr_gen y.tab.* lex.yy.c *.o ../../tables/instructions.* ../../simulator/simdispatch.c  