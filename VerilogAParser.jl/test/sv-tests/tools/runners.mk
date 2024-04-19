# Copyright (C) 2020 The SymbiFlow Authors.
#
# Use of this source code is governed by a ISC-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/ISC
#
# SPDX-License-Identifier: ISC

INSTALL_DIR := $(abspath $(OUT_DIR)/runners/)

RDIR := $(abspath third_party/tools)
TDIR := $(abspath tools)

.PHONY: runners

runners:

# odin
odin: $(INSTALL_DIR)/bin/odin_II

$(INSTALL_DIR)/bin/odin_II:
	$(MAKE) -C $(RDIR)/odin_ii/ODIN_II/ build
	install -D $(RDIR)/odin_ii/ODIN_II/odin_II $@

# yosys
yosys: $(INSTALL_DIR)/bin/yosys

$(INSTALL_DIR)/bin/yosys:
	$(MAKE) -C $(RDIR)/yosys ENABLE_TCL=0 ENABLE_ABC=0 ENABLE_GLOB=0 ENABLE_PLUGINS=0 ENABLE_READLINE=0 ENABLE_COVER=0
	install -D $(RDIR)/yosys/yosys $@

# antmicro-yosys
antmicro-yosys: $(INSTALL_DIR)/bin/antmicro-yosys

$(INSTALL_DIR)/bin/antmicro-yosys:
	$(MAKE) -C $(RDIR)/../cores/ibex-yosys-build/yosys ENABLE_TCL=0 ENABLE_ABC=0 ENABLE_GLOB=0 ENABLE_PLUGINS=0 ENABLE_READLINE=0 ENABLE_COVER=0
	install -D $(RDIR)/../cores/ibex-yosys-build/yosys/yosys $@

# icarus
icarus: $(INSTALL_DIR)/bin/iverilog

$(INSTALL_DIR)/bin/iverilog:
	cd $(RDIR)/icarus && autoconf
	cd $(RDIR)/icarus && ./configure --prefix=$(abspath $(INSTALL_DIR))/
	$(MAKE) -C $(RDIR)/icarus
	$(MAKE) -C $(RDIR)/icarus installdirs
	$(MAKE) -C $(RDIR)/icarus install

# verilator
verilator: $(INSTALL_DIR)/bin/verilator

$(INSTALL_DIR)/bin/verilator:
	cd $(RDIR)/verilator && autoconf
	cd $(RDIR)/verilator && ./configure --prefix=$(abspath $(INSTALL_DIR))/
	$(MAKE) -C $(RDIR)/verilator
	$(MAKE) -C $(RDIR)/verilator install

# slang
slang: $(INSTALL_DIR)/bin/slang-driver

$(INSTALL_DIR)/bin/slang-driver:
	mkdir -p $(RDIR)/slang/build
	cd $(RDIR)/slang/build && cmake .. -DSLANG_INCLUDE_TESTS=OFF -DCMAKE_BUILD_TYPE=Release
	$(MAKE) -C $(RDIR)/slang/build
	install -D $(RDIR)/slang/build/bin/slang $@

# Surelog
surelog: $(INSTALL_DIR)/bin/surelog

$(INSTALL_DIR)/bin/surelog:
	cd $(RDIR)/Surelog ; mkdir -p build/tests dist
	cd $(RDIR)/Surelog/build && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$(INSTALL_DIR) ../
	$(MAKE) -C $(RDIR)/Surelog/build install

# zachjs-sv2v
zachjs-sv2v: $(INSTALL_DIR)/bin/zachjs-sv2v

$(INSTALL_DIR)/bin/zachjs-sv2v:
	$(MAKE) -C $(RDIR)/zachjs-sv2v
	install -D $(RDIR)/zachjs-sv2v/bin/sv2v $@

# tree-sitter-verilog
tree-sitter-verilog: $(INSTALL_DIR)/lib/tree-sitter-verilog.so

$(INSTALL_DIR)/lib/tree-sitter-verilog.so:
	mkdir -p $(INSTALL_DIR)/lib
	cd $(RDIR)/tree-sitter-verilog && npm install
	/usr/bin/env python3 -c "from tree_sitter import Language; Language.build_library(\"$@\", [\"$(abspath $(RDIR)/tree-sitter-verilog)\"])"

uhdm-common:
	mkdir -p $(INSTALL_DIR)/bin/
	cd $(RDIR)/uhdm-integration && $(MAKE) image/bin/surelog
	cp $(RDIR)/uhdm-integration/Surelog/build/bin/surelog $(INSTALL_DIR)/bin/surelog-uhdm

# surelog-uhdm-verilator
verilator-uhdm: $(INSTALL_DIR)/bin/verilator-uhdm

# cannot use 'make -C uhdm-integration <target> as uhdm relies on $PWD
$(INSTALL_DIR)/bin/verilator-uhdm: uhdm-common
	cd $(RDIR)/uhdm-integration && $(MAKE) image/bin/verilator
	cp $(RDIR)/uhdm-integration/image/bin/verilator $(INSTALL_DIR)/bin/verilator-uhdm
	sed -i 's/"verilator_bin"/"verilator_bin-uhdm"/g' $(INSTALL_DIR)/bin/verilator-uhdm
	cp $(RDIR)/uhdm-integration/image/bin/verilator_bin $(INSTALL_DIR)/bin/verilator_bin-uhdm

# surelog-uhdm-yosys
yosys-uhdm: $(INSTALL_DIR)/bin/yosys-uhdm

# cannot use 'make -C uhdm-integration <target> as uhdm relies on $PWD
$(INSTALL_DIR)/bin/yosys-uhdm: uhdm-common
	cd $(RDIR)/uhdm-integration && $(MAKE) image/bin/yosys
	cp $(RDIR)/uhdm-integration/image/bin/yosys $(INSTALL_DIR)/bin/yosys-uhdm

# sv-parser
sv-parser: $(INSTALL_DIR)/bin/parse_sv

$(INSTALL_DIR)/bin/parse_sv:
	cd $(RDIR)/sv-parser && cargo build --release --example parse_sv
	install -D $(RDIR)/sv-parser/target/release/examples/parse_sv $@

# moore
moore: $(INSTALL_DIR)/bin/moore

$(INSTALL_DIR)/bin/moore:
	cargo install --git "https://github.com/fabianschuiki/moore" --root $(INSTALL_DIR) --bin moore

# verible
verible:
	cd $(RDIR)/verible/ && bazel run :install --noshow_progress -c opt -- $(INSTALL_DIR)/bin

$(INSTALL_DIR)/bin/verible-verilog-kythe-extractor: verible

$(INSTALL_DIR)/bin/verilog_syntax: verible

# setup the dependencies
RUNNERS_TARGETS := odin yosys icarus verilator slang zachjs-sv2v tree-sitter-verilog sv-parser moore verible surelog antmicro-yosys yosys-uhdm verilator-uhdm
.PHONY: $(RUNNERS_TARGETS)
runners: $(RUNNERS_TARGETS)
