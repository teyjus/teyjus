all : build

instr_gen : lex.yy.c y.tab.h y.tab.c instrgen-c.o instrgen-ocaml.o util.o
	gcc -o instr_gen lex.yy.c y.tab.c instrgen-c.o instrgen-ocaml.o util.o -lm

lex.yy.c : instrformats.l
	flex instrformats.l

y.tab.c y.tab.h : instrformats.y
	yacc -d instrformats.y

instrgen-c.o : instrgen-c.h instrgen-c.c util.h
	gcc -c instrgen-c.c

instrgen-ocaml.o : instrgen-ocaml.h instrgen-ocaml.c util.h
	gcc -c instrgen-ocaml.c

util.h : 
	cp ../util/util.h .

util.c :
	cp ../util/util.c .

util.o : util.h util.c
	gcc -o util.o -c util.c  

build : instr_gen
	instr_gen 

clean :
	rm instr_gen y.tab.* lex.yy.c *.o util.h util.c

clean_all :
	rm instr_gen y.tab.* lex.yy.c *.o ../../tables/instructions.* ../../simulator/simdispatch.c  util.h util.c