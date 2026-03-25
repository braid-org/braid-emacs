EMACS ?= emacs
BRAID_FUZZ ?= braid-fuzz

EL = braid-http.el braid-text.el braid-mode.el braid-cursors.el myers-diff.el
ELC = $(EL:.el=.elc)

all: $(ELC)

%.elc: %.el
	$(EMACS) --batch -L . -f batch-byte-compile $<

test: test-unit test-braid-fuzz

test-unit:
	$(EMACS) --batch -Q -L . -l test/hook-patch-test.el -f run-hook-patch-tests

test-braid-fuzz:
	$(BRAID_FUZZ) ./test/braid-fuzz-launcher.sh

clean:
	rm -f $(ELC)

.PHONY: all test test-unit test-braid-fuzz clean
