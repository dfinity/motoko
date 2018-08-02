# Makefile for actorscript  demos

# This works with bash

ASC=../main.native
# with the compiler
all:
	rm -f *.txt
	$(ASC) counterex.as >> counterex.txt 2>&1
	$(ASC) hoare.as >> hoare.txt 2>&1
	$(ASC) quicksort.as >> quicksort.txt 2>&1
	$(ASC) bankex.as >> bankex.txt 2>&1


# refresh
current:
	rm -f *.txt
	make all 

clean:
	rm -f *.txt
