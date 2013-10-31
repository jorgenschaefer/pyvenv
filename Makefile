.PHONY: all test test-all cask

all: test

test:
	cask exec ert-runner

test-all: test
	EMACS=emacs-24.1 cask exec ert-runner
	EMACS=emacs-24.2 cask exec ert-runner
	EMACS=emacs-24.3 cask exec ert-runner

cask:
	cask install
	EMACS=emacs-24.1 cask install
	EMACS=emacs-24.2 cask install
	EMACS=emacs-24.3 cask install
