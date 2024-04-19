// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: idx_select
:description: indexed select bit test
:tags: 11.5.1
*/
module top();
logic [15:0] a;
logic b;

initial begin
	b = a[11];
end

endmodule
