EMACS ?= emacs
ELFILES := perspective.el
ELCFILES = $(ELFILES:.el=.elc)
TESTFILES := $(wildcard test/test-*.el)

all: test

.PHONY: test
test:
	$(EMACS) -nw -Q -batch -L . -l ert $(addprefix -l ,$(TESTFILES)) \
		--eval "(ert-run-tests-batch-and-exit)"

.PHONY: perf
perf:
	$(EMACS) -nw -Q -batch -L . -l test/perf-perspective.el \
		--eval "(persp-perf-run)"

.PHONY: compile
compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<
