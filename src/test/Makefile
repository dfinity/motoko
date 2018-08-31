# Makefile for actorscript  cases

# This works with bash

ASC=../main.native
ASC_FLAGS=-t -d
# with the compiler
all:
	rm -f result
	$(ASC) $(ASC_FLAGS) bank.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) bank-ordered.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) control.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) literals.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) numeric-ops.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) bit-ops.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) relational-ops.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) concat.as >> result 2>&1	
	$(ASC) $(ASC_FLAGS) duplicate-field.as >> result 2>&1 || true
	$(ASC) $(ASC_FLAGS) nonlinear-pat.as >> result 2>&1 || true
	$(ASC) $(ASC_FLAGS) type-definition.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) type-equivalence.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) type-inference.as >> result 2>&1 || true
	$(ASC) $(ASC_FLAGS) quicksort.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) hoare.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) bank-example.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) counter.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) while.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) objects.as >> result 2>&1
	$(ASC) $(ASC_FLAGS) overflow.as >> result 2>&1 || true
	$(ASC) $(ASC_FLAGS) menhir-bug.as >> result 2>&1 || true

	diff result result.ok

# refresh
current:
	rm -f result
	make all 

clean:
	rm -f result
