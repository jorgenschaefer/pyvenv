.PHONY: all test test-all cask

EMACS ?= emacs
VERSION=$(shell sed -ne 's/^;; Version: \(.*\)/\1/p' pyvenv.el)

all: test

test:
	cask exec ert-runner --quiet

test-all: clean cask
	cask exec ert-runner --quiet
	EMACS=emacs-24.1 cask exec ert-runner --quiet
	EMACS=emacs-24.2 cask exec ert-runner --quiet
	EMACS=emacs-24.3 cask exec ert-runner --quiet
	EMACS=emacs-24.4 cask exec ert-runner --quiet
	EMACS=emacs-24.5 cask exec ert-runner --quiet

cask:
	cask install
	EMACS=emacs-24.1 cask install
	EMACS=emacs-24.2 cask install
	EMACS=emacs-24.3 cask install
	EMACS=emacs-24.4 cask install
	EMACS=emacs-24.5 cask install

compile:
	$(EMACS) -batch -L . -f batch-byte-compile *.el

clean:
	rm -rf .cask dist
	find -name '*.elc' -delete
