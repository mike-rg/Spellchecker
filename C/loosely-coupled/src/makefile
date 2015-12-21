CC = clang

spellchk: spellchecker.c document.c dictionary.c
	${CC} -o$@ spellchecker.c document.c dictionary.c -std=gnu99 -g -O0

clean:	
	rm spellchk