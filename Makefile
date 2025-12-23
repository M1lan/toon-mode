EMACS ?= emacs

.PHONY: test

test:
	$(EMACS) -Q --batch -l test/toon-mode-tests.el -f ert-run-tests-batch-and-exit
