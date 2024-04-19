// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: implicit_port
:description: implicit port signal tests
:tags: 6.10
*/
module top(input [3:0] a, input [3:0] b);
	wire [3:0] c;
	assign c = a | b;
endmodule
