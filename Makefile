.PHONY: all test test-all cask

all: test

test:
	cask exec ert-runner --quiet

test-all: test
	EMACS=emacs-24.1 cask exec ert-runner --quiet
	EMACS=emacs-24.2 cask exec ert-runner --quiet
	EMACS=emacs-24.3 cask exec ert-runner --quiet

cask:
	cask
	EMACS=emacs-24.1 cask
	EMACS=emacs-24.2 cask
	EMACS=emacs-24.3 cask
