# Makefile for actorscript  cases

# This works with bash

ASC=../main.native
# with the compiler
all:
	rm -f result
	$(ASC) bank.as >> result 2>&1
	$(ASC) bank_ordered.as >> result 2>&1
	$(ASC) control.as >> result 2>&1
	$(ASC) literals.as >> result 2>&1
	$(ASC) numericops.as >> result 2>&1
	$(ASC) bitops.as >> result 2>&1
	$(ASC) relationalops.as >> result 2>&1
	$(ASC) concat.as >> result 2>&1	
	$(ASC) duplicatefield.as >> result 2>&1
	$(ASC) nonlinearpat.as >> result 2>&1
	$(ASC) typedef.as >> result 2>&1

	diff result result.ok

# refresh
current:
	rm -f result
	make all 

clean:
	rm -f result
