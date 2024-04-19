// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: tagged_union_member_access_inv
:description: invalid tagged union member access test
:should_fail_because: accessing wrong member should result in run-time error
:tags: 11.9
:type: simulation
*/
module top();

typedef union tagged {
	void Invalid;
	int Valid;
} u_int;

u_int a, b;

int c;

initial begin
	a = tagged Invalid;
	b = tagged Valid(42);
	c = a.Valid;
end

endmodule
