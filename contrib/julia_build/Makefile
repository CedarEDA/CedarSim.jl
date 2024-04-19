all: build

include julia_version.inc
JULIA_SHORT_GITSHA := $(shell echo $(JULIA_GITSHA) | cut -c1-10)

OS ?= $(shell if [ "$$(uname -s)" = Darwin ]; then echo "macos"; else echo "linux"; fi)
ARCH ?= $(shell uname -m)
ifeq "$(ARCH)" "arm64"
ARCH=aarch64
endif

julia/.git/HEAD: julia_version.inc
	if [ ! -d julia ]; then \
		git clone https://github.com/julialang/julia.git julia ; \
	fi
	git -C julia fetch
	git -C julia checkout $(JULIA_GITSHA)
	git -C julia reset --hard
	git -C julia clean -ffdx
	touch $@

julia/Make.user: julia/.git/HEAD
	echo "# This file automatically generated" > "$@"
	# We used to need to patch LLVM, no longer since https://github.com/JuliaLang/llvm-project/commit/5c82f5309b10fab0adf6a94969e0dddffdb3dbce
	#echo "DEPS_GIT := llvm" >> "$@"
	#echo "USE_BINARYBUILDER_LLVM := 0" >> "$@"
	if [ "${ARCH}" = "x86_64" ]; then \
		echo "JULIA_CPU_TARGET := generic;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1)" >> "$@"; \
	else \
		echo "JULIA_CPU_TARGET := generic;cortex-a57;thunderx2t99;carmel" >> "$@"; \
	fi
	echo "JL_STACK_SIZE := 67108864" >> "$@"
	echo "WITH_TRACY := 1" >> "$@"

JULIA_TARBALL := julia-$(JULIA_SHORT_GITSHA)-$(OS)-$(ARCH).tar.gz
JULIA_TARBALL_PATH := julia/$(JULIA_TARBALL)
$(JULIA_TARBALL_PATH): julia/Make.user
	$(MAKE) -C julia binary-dist
	mv julia/julia-${JULIA_SHORT_GITSHA}-*.tar.gz ${JULIA_TARBALL_PATH}

build: $(JULIA_TARBALL_PATH)

upload-$(JULIA_SHORT_GITSHA): $(JULIA_TARBALL_PATH)
	aws s3 cp $< s3://jc-cedarsim-juliabuilds/vanilla/$(JULIA_TARBALL)

upload-$(JULIA_SHORT_GITSHA)-latest: $(JULIA_TARBALL_PATH)
	aws s3 cp $< s3://jc-cedarsim-juliabuilds/vanilla/julia-latest-$(OS)-$(ARCH).tar.gz

upload: upload-$(JULIA_SHORT_GITSHA)
upload-latest: upload-$(JULIA_SHORT_GITSHA) upload-$(JULIA_SHORT_GITSHA)-latest

clean:
	git -C julia clean -ffdx

print-%:
	@echo "$*=$($*)"
