EMACS ?= emacs
CASK ?= cask

all: test

deps:
	${CASK} install

test: clean-elc
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

compile: deps
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile compile-queue.el

clean-elc:
	rm -f compile-queue.elc

.PHONY:	all test docs unit
