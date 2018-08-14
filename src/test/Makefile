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
	$(ASC) duplicatefield.as >> result 2>&1 || true
	$(ASC) nonlinearpat.as >> result 2>&1 || true
	$(ASC) typedef.as >> result 2>&1
	$(ASC) type_equiv.as >> result 2>&1
	$(ASC) quicksort.as >> result 2>&1
	$(ASC) hoare.as >> result 2>&1
	$(ASC) bankex.as >> result 2>&1
	$(ASC) counter.as >> result 2>&1
	$(ASC) testwhile.as >> result 2>&1
	$(ASC) overflow.as >> result 2>&1 || true
	$(ASC) menhirbug.as >> result 2>&1 || true

	diff result result.ok

# refresh
current:
	rm -f result
	make all 

clean:
	rm -f result
