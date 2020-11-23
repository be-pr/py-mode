EMACS ::= $(shell which emacs) -Q --batch -L .
LIB ::= $(shell basename $(shell pwd))
TESTF ?= *-test.el
ELS ?= $(filter-out $(LIB)-pkg.el $(wildcard *-test.el), $(wildcard *.el))
ELCS ?= $(ELS:=c)

all : compile test

%.elc : %.el
	$(EMACS) -f batch-byte-compile $<

compile : $(ELCS)

test : compile $(TESTF)
	$(EMACS) -l $(filter-out $<, $^) -f ert-run-tests-batch-and-exit

clean :
	rm $(ELCS)

.PHONY : all compile test clean
