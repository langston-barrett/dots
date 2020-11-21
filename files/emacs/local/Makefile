CASK = cask
SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)
PATTERN = .*

# TODO: Individualize
$(OBJS): %.elc: %.el
	cask build

.PHONY: build
build: $(OBJS)

.PHONY: test
test: build
	$(CASK) exec buttercup -L . --pattern '$(PATTERN)' test

.PHONY: entr
entr: build
	find . -name "*.el" -or -name "Makefile" | entr -c -s "make test"

.PHONY: clean
clean:
	rm $(OBJS)
