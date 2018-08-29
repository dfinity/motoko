# Makefile for actorscript  cases

# This works with bash

ASC=../main.native
ASC_FLAGS=-t -d
# with the compiler
all:
	rm -f result
	$(ASC) $(ASC_FLAGS) bank.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) bank_ordered.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) control.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) literals.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) numericops.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) bitops.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) relationalops.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) concat.as >> result 2>&1	
	$(ASC) $(ASC_FLAGS) duplicatefield.as >> result 2>&1 || true
	$(ASC) $(ASC_FLAGS) nonlinearpat.as >> result 2>&1 || true
	$(ASC) $(ASC_FLAGS) typedef.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) type_equiv.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) quicksort.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) hoare.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) bankex.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) counter.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) testwhile.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) objects.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) overflow.as >> result 2>&1 || true
	$(ASC) $(ASC_FLAGS) menhirbug.as >> result 2>&1 || true

	diff result result.ok

# refresh
current:
	rm -f result
	make all 

clean:
	rm -f result
