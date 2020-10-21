CASK = cask
SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)
PATTERN = .*

$(OBJS): %.elc: %.el
	cask build

.PHONY: build
build: $(OBJS)

.PHONY: test
test: build
	$(CASK) exec buttercup -L . --pattern '$(PATTERN)' test

.PHONY: clean
clean:
	rm $(OBJS)
