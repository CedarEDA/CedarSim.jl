// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: 22.4--check_included_definitions
:description: Test
:tags: 22.4
:type: preprocessing parsing
*/
`include "include_directory/defs.sv"
module top ();
initial begin
        $display(":assert:(`TWO_PLUS_TWO == 5)");
	$display(":assert:('%s' == '%s')", `define_var, "define_var");
end

endmodule
