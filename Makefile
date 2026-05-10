ENV?=development

## NOTE: run sbcl loading the setting the current path
## on the `ql:*local-project-directories*`.
LISP?=sbcl --sysinit ./.sbclrc

LISPFLAGS=--non-interactive

.PHONY: tests
tests:
	ENV=$(ENV) \
	$(LISP) \
	$(LISPFLAGS) --quit --load tests-runner.lisp
