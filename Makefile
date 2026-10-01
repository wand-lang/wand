.PHONY: build test repl run fmt clean install fuzz fuzz-eval fuzz-sweep \
        release release-archive

build:
	dune build

test:
	dune test

repl: build
	dune exec wand -- i

run: build
	dune exec wand

fmt:
	dune fmt

clean:
	dune clean

install:
	dune build @install

# ── Fuzzing ──────────────────────────────────────────────────────────────────
#
# `ITERATIONS=N` on any of these, or `SEED=N` on the first two. A run exits 0
# when it finds nothing whose signature is not already in
# `test/fuzz/known.txt`, so these are usable as a gate.

ITERATIONS ?= 20000
SEED ?= 0

# The lexer, the parser, the typechecker and the formatter. About fifteen
# seconds at the default count, and worth a run after a change to any of them.
fuzz: build
	_build/default/test/fuzz/fuzz.exe --seed $(SEED) --iterations $(ITERATIONS)

# The same, and it runs the program on both sides of a format as well: a
# formatting that keeps the type and changes the answer is a bug nothing else
# here asks about. Only programs the typechecker says reach nothing outside
# themselves are run, each in a process of its own with a budget of its own.
#
# It also runs every program that typechecks up to its first effect, which is
# stopped and not carried out, and reports one its type does not name: the
# property the effect gate rests on.
#
# Slower than `fuzz` by a little over half, so it covers less ground in the
# same time. Which of the two is worth more depends on what changed.
fuzz-eval: build
	_build/default/test/fuzz/fuzz.exe --seed $(SEED) --iterations $(ITERATIONS) --eval

# Several seeds, each with its own findings directory, before a release.
# SEEDS and SECONDS_PER_SEED are read by the script.
fuzz-sweep: build
	WAND=$(PWD)/_build/default/bin/wand.exe \
	  $(PWD)/_build/default/bin/wand.exe tools/fuzz_sweep.wand

# ── Releasing ────────────────────────────────────────────────────────────────
#
# CI builds three of the four archives when a v* tag lands: both Linux
# targets and macOS arm64. The fourth, macOS x86_64, is built here, because
# GitHub's Intel macOS runner never leaves the queue and OCaml cannot
# cross-compile to reach it.
#
#     make release VERSION=0.1.0
#
# tags, pushes, builds and attaches that archive. It does not wait for CI:
# both sides create the release if it is missing and upload to it if it is
# not, so they can finish in either order.
#
# The release is a draft until someone says otherwise. Check that all four
# archives are attached, then:
#
#     gh release edit v0.1.0 --draft=false

TARGET := macos-x86_64
NAME    = wand-$(VERSION)-$(TARGET)

release:
	@test -n "$(VERSION)" || { echo "usage: make release VERSION=0.1.0"; exit 1; }
	@test "$$(uname -s)-$$(uname -m)" = "Darwin-x86_64" || \
	  { echo "the $(TARGET) archive has to be built on an Intel Mac; this is $$(uname -s)-$$(uname -m)"; exit 1; }
	@test -z "$$(git status --porcelain)" || { echo "working tree is dirty; commit before releasing"; exit 1; }
	@test "$$(cat VERSION)" = "$(VERSION)" || \
	  { echo "VERSION says $$(cat VERSION) but you asked to release $(VERSION);"; \
	    echo "update VERSION and commit it, so \`wand version\` matches the tag"; exit 1; }
	dune build
	dune test
	_build/default/bin/wand.exe test test/wand
	git tag v$(VERSION)
	git push origin main v$(VERSION)
	$(MAKE) release-archive VERSION=$(VERSION)

# The packaging half on its own, for when the tag is already pushed and only
# this archive needs rebuilding or reattaching.
release-archive:
	@test -n "$(VERSION)" || { echo "usage: make release-archive VERSION=0.1.0"; exit 1; }
# This is the one archive a person builds, because GitHub's Intel macOS
# runner never leaves the queue. So it is the one archive that can be built
# from the wrong tree, and it was: a `release-archive VERSION=0.53.2` run
# from `main` put a 0.54.0 binary on the 0.53.2 tag, over a `--clobber`
# upload, 47 minutes after that release went out. Nothing said so. Everyone
# installing 0.53.2 on this platform got a different release with a
# different command line.
#
# Three questions, asked before anything is built. What is checked out has
# to be the tag being released, the tree has to be clean, and the binary
# that comes out has to say the number on the box.
	@test -z "$$(git status --porcelain)" || \
	  { echo "working tree is dirty; commit before building an archive"; exit 1; }
	@at=$$(git describe --tags --exact-match 2>/dev/null || echo none); \
	  test "$$at" = "v$(VERSION)" || \
	  { echo "HEAD is at $$at, not v$(VERSION);"; \
	    echo "check out the tag you are building: git checkout v$(VERSION)"; exit 1; }
	dune build --profile release bin/wand.exe
	@got=$$(_build/default/bin/wand.exe version 2>/dev/null \
	          || _build/default/bin/wand.exe V 2>/dev/null); \
	  test "$$got" = "wand $(VERSION)" || \
	  { echo "the built binary reports '$$got', not 'wand $(VERSION)'"; exit 1; }
	rm -rf dist/$(NAME) dist/$(NAME).tar.gz*
	mkdir -p dist/$(NAME)
	cp _build/default/bin/wand.exe dist/$(NAME)/wand
	chmod 755 dist/$(NAME)/wand
	cp LICENSE README.md dist/$(NAME)/
	tar -czf dist/$(NAME).tar.gz -C dist $(NAME)
# Written by wand, over the binary this target just built. `shasum` was
# two problems: the manifest said a binary ran rather than what was
# verified, and the binary is `shasum` on macOS and `sha256sum` on most
# Linux images. The format is unchanged, so `shasum -c` still reads it.
	cd dist && $(CURDIR)/_build/default/bin/wand.exe $(CURDIR)/tools/checksum.wand \
	  $(NAME).tar.gz > $(NAME).tar.gz.sha256
	gh release view v$(VERSION) >/dev/null 2>&1 || \
	  gh release create v$(VERSION) --draft --title v$(VERSION) --notes-file .github/release-notes.md
	gh release upload v$(VERSION) dist/$(NAME).tar.gz dist/$(NAME).tar.gz.sha256 --clobber
	@echo
	@echo "attached $(NAME).tar.gz to the v$(VERSION) draft"
# The archives CI builds carry a build attestation, signed by the workflow
# that built them. This one is built here, so it cannot have one: an
# attestation says which workflow produced the bytes, and no workflow
# produced these. Said out loud rather than left to be noticed, because it
# is the one archive whose provenance rests on the .sha256 alone -- and the
# one that was clobbered before.
	@echo "not attested: built here, so only the .sha256 stands behind it"
	@echo "publish with: gh release edit v$(VERSION) --draft=false"
