all:	vb

CFLAGS=-Wall

lex.yy.c:	lexer.l
	flex -i $<

parser.tab.c:	parser.y
		bison -v -d $<

vb:	emit.o parser.tab.o lex.yy.o main.o
	gcc lex.yy.o main.o emit.o parser.tab.o -o $@ -lfl

clean:
	rm *.o parser.tab.[hc] parser.output lex.yy.c vb
