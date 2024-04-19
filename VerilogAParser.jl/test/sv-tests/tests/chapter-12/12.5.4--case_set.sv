// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: case_set
:description: A module testing case set membership
:tags: 12.5.4
*/
module case_tb ();
	reg [3:0] a = 0;
	reg [3:0] b = 0;
	always @* begin
		case(a) inside
			1, 3: b = 1;
			4'b01??, [5:6]: b = 2;
			default b = 3;
		endcase
	end
endmodule
