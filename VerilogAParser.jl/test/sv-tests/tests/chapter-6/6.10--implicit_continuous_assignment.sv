// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: implicit_continuous_assignment
:description: implicit declaration in continuous assignment tests
:tags: 6.10
*/
module top();
	wire [3:0] a = 8;
	wire [3:0] b = 5;

	assign c = | (a | b);
endmodule
