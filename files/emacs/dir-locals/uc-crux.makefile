# symlink me into uc-crux

OUT_DIR := .out
OUT := $(OUT_DIR)/.out
THIS := Makefile

CABAL_FLAGS := -j

# Executables
CABAL := cabal
HLINT := hlint

# Groups of files
LINTER_CONFIGS := .hlint.yaml
CABALS := $(shell find exe -name "*.cabal")
EXE_HS := $(shell find exe -name "*.hs")
SRC_HS := $(shell find src -name "*.hs")
TEST_HS := $(shell find test -name "*.hs")
CABAL_PROJECT := $(shell find test -name "cabal.project*")
ALL_HS := $(EXE_HS) $(SRC_HS) $(TEST_HS)
HS := $(notdir $(ALL_HS))
TYPECHECK_HS = $(filter-out %Main.hs,$(SRC_HS))

# Paths of targets
DEPS := $(OUT_DIR)/deps
BUILD := $(OUT_DIR)/build
INSTALL := $(OUT_DIR)/install
TEST := $(OUT_DIR)/tests
TYPES := $(OUT_DIR)/types

$(OUT):
	@mkdir -p $(OUT_DIR)/
	@touch $(OUT)

$(DEPS): $(OUT) $(THIS) $(CABALS) $(CABAL_PROJECT)
	$(CABAL) $(CABAL_FLAGS) install --only-dependencies
	@touch "$@"

$(BUILD): $(EXE_HS) $(SRC_HS)
	$(CABAL) $(CABAL_FLAGS) build
	@touch "$@"

$(INSTALL): $(BUILD) $(SRC_HS)
	$(CABAL) $(CABAL_FLAGS) install
	@touch "$@"

$(TEST): $(BUILD) $(SRC_HS) $(TEST_HS)
	$(CABAL) $(CABAL_FLAGS) test
	@touch "$@"

define linter
$(OUT_DIR)/$(1).$(3): $(LINTER_CONFIGS) $(1)
	@mkdir -p $$(@D)
	@echo $(2) $(1)
	$(2) $(subst {},$(1),$(4))
	@touch "$$@"
endef

$(foreach hs,$(ALL_HS),$(eval $(call linter,$(hs),hlint,syntax,--only="Parse error" -- {})))
$(foreach hs,$(ALL_HS),$(eval $(call linter,$(hs),hlint,hlint,-- {})))
# $(foreach hs,$(TYPECHECK_HS),$(eval $(call linter,$(hs),bash,types,-c "cd src; cabal exec -- ghc -O0 -fno-code -fno-break-on-exception -fno-break-on-error -ferror-spans {}")))

syntax: $(addprefix $(OUT_DIR)/, $(ALL_HS:=.syntax))
hlint: $(addprefix $(OUT_DIR)/, $(ALL_HS:=.hlint))
# types: $(addprefix $(OUT_DIR)/, $(TYPECHECK_HS:=.types))

$(TYPES): $(TYPECHECK_HS)
	cd src; cabal exec -- ghc -O0 -fno-code -fno-break-on-exception -fno-break-on-error -ferror-spans $(subst src/,,$(TYPECHECK_HS))
	touch "$@"

.PHONY: types
types: $(TYPES)

.PHONY: lint
lint: syntax hlint types

.PHONY: build
build: $(BUILD)

.PHONY: test
test: $(TEST)

.PHONY: entr-lint
entr-lint:
	for f in $(THIS) $(ALL_HS); do printf "%s\n" "$${f}"; done | \
	  entr -c -s "$(MAKE) lint && $(MAKE) types"

.PHONY: entr-build
entr-build:
	for f in $(THIS) $(ALL_HS); do printf "%s\n" "$${f}"; done | \
	  entr -c -s "$(MAKE) lint && $(MAKE) types && $(MAKE) build"

.PHONY: entr
entr:
	for f in $(THIS) $(ALL_HS); do printf "%s\n" "$${f}"; done | \
	  entr -c -s "$(MAKE) lint && $(MAKE) types && $(MAKE) build && $(MAKE) test"

.PHONY: install
install: $(INSTALL)

.PHONY: clean
clean:
	rm -rf dist*

all: lint build test
