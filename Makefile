.SUFFIXES:
Makefile:;

BAZEL_BINDIR := bazel-bin
BAZEL_CONFIG :=
BAZEL_PACKAGE :=
BAZEL_TARGET :=
BUILDDIR := .build
HIE_BIOS_OUTPUT :=
VERSION_BAZELISK := 1.7.4

# Based on https://stackoverflow.com/a/12099167/1549047.
ifeq ($(OS),Windows_NT)
BAZEL_BINARY := $(BUILDDIR)/bazelisk.exe
BAZEL_URI := https://github.com/bazelbuild/bazelisk/releases/download/v$(VERSION_BAZELISK)/bazelisk-windows-amd64.exe
else ifeq ($(shell uname -s),Linux)
BAZEL_BINARY := $(BUILDDIR)/bazelisk
BAZEL_URI := https://github.com/bazelbuild/bazelisk/releases/download/v$(VERSION_BAZELISK)/bazelisk-linux-amd64
else ifeq ($(shell uname -s),Darwin)
BAZEL_BINARY := $(BUILDDIR)/bazelisk
BAZEL_URI := https://github.com/bazelbuild/bazelisk/releases/download/v$(VERSION_BAZELISK)/bazelisk-darwin-amd64
else
$(error Platform not supported. Only Linux, macOS, and Windows are supported)
endif

BUILDIFIER := $(BAZEL_BINDIR)/tools/buildifier/buildifier
HLINT := $(BAZEL_BINDIR)/tools/hlint/hlint
IBAZEL_BINARY := $(BAZEL_BINDIR)/tools/ibazel/ibazel
ORMOLU := $(BAZEL_BINDIR)/tools/ormolu/ormolu

BAZEL := BAZEL_USE_CPP_ONLY_TOOLCHAIN=1 $(BAZEL_BINARY)
IBAZEL := BAZEL_USE_CPP_ONLY_TOOLCHAIN=1 $(IBAZEL_BINARY) -bazel_path $(BAZEL_BINARY)

.DEFAULT_GOAL := build

$(BAZEL_BINARY): | $(BUILDDIR)
	$(info Downloading bazelisk binary)
	curl --location --output $@ $(BAZEL_URI)
	@chmod 0755 $@
	@touch $@
	$(BAZEL) version

$(BUILDDIR):
	@mkdir -p $@

.PHONY: build
build: $(BAZEL_BINARY)
	$(BAZEL) build $(BAZEL_CONFIG) //...

.PHONY: clean
clean:
	$(info Cleaning bazel artifacts)
	-$(BAZEL) clean
	$(info Removing $(BUILDDIR))
	@rm -fr $(BUILDDIR)

.PHONY: format
format: format-haskell format-starlark

.PHONY: format-haskell
format-haskell: $(BAZEL_BINARY)
	$(BAZEL) build //tools/ormolu
	# Ormolu doesn't seem to support recursing in a directory,
	# so we have to find all of the files ourselves.
	$(ORMOLU) --mode=inplace $(shell git ls-files '*.hs')

.PHONY: format-starlark
format-starlark: $(BAZEL_BINARY)
	$(BAZEL) build //tools/buildifier
	$(BUILDIFIER) -r .

PHONY: hie-bios
hie-bios: $(BAZEL_BINARY)
	$(BAZEL) build //$(BAZEL_PACKAGE):$(BAZEL_TARGET) --output_groups=hie_bios
	cat $(BAZEL_BINDIR)/$(BAZEL_PACKAGE)/$(BAZEL_TARGET)@hie-bios > $(HIE_BIOS_OUTPUT)
	# Make warnings non-fatal
	echo '-Wwarn' >> $(HIE_BIOS_OUTPUT)

.PHONY: lint
lint: lint-haskell lint-starlark

.PHONY: lint-haskell
lint-haskell: $(BAZEL_BINARY)
	$(BAZEL) build //tools/hlint
	$(HLINT) --git

.PHONY: lint-starlark
lint-starlark: $(BAZEL_BINARY)
	$(BAZEL) build //tools/buildifier
	$(BUILDIFIER) -r -lint=warn -mode=check .

.PHONY: watch
watch: $(BAZEL_BINARY)
	$(BAZEL) build //tools/ibazel
	$(IBAZEL) build $(BAZEL_CONFIG) //...
