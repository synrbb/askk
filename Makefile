SHELL = /bin/sh
EMACS = emacs
POSFRAME_VER = 1.4.4

EXCLUDES := $(wildcard *-pkg.el *-autoloads.el)
TESTS := $(wildcard *-tests.el)
SRCS := $(filter-out $(EXCLUDES) $(TESTS) posframe.el,$(wildcard *.el))
OBJS := $(SRCS:.el=.elc)

.SUFFIXES:
.PHONY: all clean check

all: posframe.elc $(OBJS)

clean:
	-rm -f $(OBJS)

distclean: clean
	-rm -f posframe.el posframe.elc

check: all
	$(EMACS) -batch -Q -L . -l ert $(addprefix -l ,$(TESTS)) \
		-f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<

posframe.el:
	curl -sSL https://elpa.gnu.org/packages/posframe-$(POSFRAME_VER).tar \
	| tar -xf- --strip-components 1 --include posframe-$(POSFRAME_VER)/$@
