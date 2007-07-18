all : build

perv_gen : lex.yy.c y.tab.h y.tab.c types.o pervgen-c.o op.o util.o pervgen-ocaml.o ocamlcode.o ccode.o
	gcc -o perv_gen lex.yy.c y.tab.c types.o pervgen-c.o op.o util.o pervgen-ocaml.o ocamlcode.o ccode.o

lex.yy.c : pervasives.l
	flex pervasives.l

y.tab.c y.tab.h : pervasives.y
	yacc -d pervasives.y

types.o : types.h types.c util.h
	gcc -c types.c

pervgen-c.o : pervgen-c.h pervgen-c.c util.h
	gcc -c pervgen-c.c

pervgen-ocaml.o : pervgen-ocaml.h pervgen-ocaml.c util.h
	gcc -c pervgen-ocaml.c

ocamlcode.o : ocamlcode.h ocamlcode.c util.h
	gcc -c ocamlcode.c

ccode.o : ccode.h ccode.c util.h
	gcc -c ccode.c

op.o : op.h op.c util.h
	gcc -c op.c

build : perv_gen pervasives.in
	perv_gen pervasives.in

util.h : 
	cp ../util/util.h .

util.c :
	cp ../util/util.c .

util.o : util.h util.c
	gcc -o util.o -c util.c  

clean :
	rm perv_gen y.tab.* lex.yy.c *.o util.h util.c

clean_all :
	rm perv_gen y.tab.* lex.yy.c *.o ../../tables/pervasives.* util.h util.c