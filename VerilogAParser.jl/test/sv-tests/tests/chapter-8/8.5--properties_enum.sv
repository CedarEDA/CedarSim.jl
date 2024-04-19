// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: properties_enum
:description: enum defined inside class
:tags: 8.5
*/
module class_tb ();
	class test_cls;
		typedef enum {A = 10, B = 20, C = 30, D = 1} e_type;
	endclass

	test_cls test_obj;

	initial begin
		test_obj = new;
		$display(test_obj.C);
	end
endmodule
