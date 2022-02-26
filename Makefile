help:
	@cat Makefile

.PHONY: test

hlint:
	hlint ./src ./test

test:
	stack test --fast --file-watch

watch:
	stack build --fast --file-watch


hl: hlint
t: test
w: watch
