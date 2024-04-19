BENCHMARKS_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
REPO_ROOT := $(abspath $(BENCHMARKS_DIR)/../..)

JULIA ?= julia
BENCHMARK_GITSHA ?= $(if $(BUILDKITE_COMMIT),-$(BUILDKITE_COMMIT),)

define add_dep
$1: $2
endef

all: run-benchmarks

$(BENCHMARKS_DIR)/output:
	mkdir -p $@

$(BENCHMARKS_DIR)/output/%$(BENCHMARK_GITSHA).log: $(BENCHMARKS_DIR)/%.jl | $(BENCHMARKS_DIR)/output
	@bash -c "set -euo pipefail; $(JULIA) --color=yes --project=$(BENCHMARKS_DIR) $< 2>&1 | tee >(sed 's/\x1b\[[0-9;]*m//g' > $@)"
	@rm -f $(BENCHMARKS_DIR)/output_stats/_combined.*
	@cat $(BENCHMARKS_DIR)/output_stats/*.tsv > $(BENCHMARKS_DIR)/output_stats/_combined.tsv
	@cat $(BENCHMARKS_DIR)/output_stats/*.txt > $(BENCHMARKS_DIR)/output_stats/_combined.txt

BENCHMARK_SCRIPTS := $(basename $(wildcard *_bench.jl))
$(foreach benchmark,$(BENCHMARK_SCRIPTS),$(eval $(call add_dep,run-benchmarks,$(BENCHMARKS_DIR)/output/$(benchmark)$(BENCHMARK_GITSHA).log)))

print-%:
	@echo $*=$($*)

