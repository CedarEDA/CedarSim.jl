// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: 22.4--include_via_define
:description: Though not well documented, real world code does `defines that cause includes
:tags: 22.4
:type: preprocessing parsing
*/
`define DO_INCLUDE(FN) `include FN

// Check that multiple define references don't throw a multiple `include-on-line error
`DO_INCLUDE("dummy_include.sv") `DO_INCLUDE("dummy_include.sv")

// Check that ifdefs
`ifdef NEVER
 `DO_INCLUDE("SHOULD_NOT_BE_INCLUDED")
`endif

module top ();
endmodule
