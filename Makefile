EMACS ?= emacs
ELFILES := perspective.el
ELCFILES = $(ELFILES:.el=.elc)

all: test

.PHONY: test
test:
	$(EMACS) -nw -Q -batch -L . -l ert $(addprefix -l ,$(wildcard test/*.el)) \
		--eval "(ert-run-tests-batch-and-exit)"

.PHONY: compile
compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<
