EMACS_BIN ?= emacs

all: test

test:
	$(EMACS_BIN) -q -batch -L `pwd` -l ert -l pyvenv-test.el \
		     -f ert-run-tests-batch-and-exit
