// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: 22.6--ifdef-nested
:description: Test
:tags: 22.6
:type: preprocessing
*/
module test(out);
output out;
`define wow
`define nest_one
`define second_nest
`define nest_two
`ifdef wow
	initial $display("wow is defined");
	`ifdef nest_one
		initial $display("nest_one is defined");
		`ifdef nest_two
			initial $display("nest_two is defined");
		`else
			initial $display("nest_two is not defined");
		`endif
	`else
		initial $display("nest_one is not defined");
	`endif
`else
	initial $display("wow is not defined");
	`ifdef second_nest
		initial $display("second_nest is defined");
	`else
		initial $display("second_nest is not defined");
	`endif
`endif
endmodule
