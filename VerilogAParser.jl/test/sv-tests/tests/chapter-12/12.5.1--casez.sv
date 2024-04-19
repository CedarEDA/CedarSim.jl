// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: casez
:description: A module testing casez statement
:tags: 12.5.1
*/
module case_tb ();
	wire [3:0] a = 4'b1z11;
	reg [3:0] b = 0;
	always @* begin
		casez(a)
			4'b1zzz: b = 1;
			4'b01z?: b = 2;
			4'b001z: b = 3;
			4'b0001: b = 4;
			default b = 0;
		endcase
	end
endmodule
