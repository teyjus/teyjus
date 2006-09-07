all : build

perv_gen : lex.yy.c y.tab.h y.tab.c types.o pervgen-c.o
	gcc -o perv_gen lex.yy.c y.tab.c types.o pervgen-c.o

lex.yy.c : pervasives.l
	flex pervasives.l

y.tab.c y.tab.h : pervasives.y
	yacc -d pervasives.y

types.o : types.h types.c
	gcc -c types.c

pervgen-c.o : pervgen-c.h pervgen-c.c
	gcc -c pervgen-c.c

build : perv_gen pervasives.in
	perv_gen pervasives.in

clean :
	rm perv_gen y.tab.* lex.yy.c *.o ../tables/pervasives.*