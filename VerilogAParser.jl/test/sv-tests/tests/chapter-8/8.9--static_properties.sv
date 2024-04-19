// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: static_properties
:description: static class properties test
:tags: 8.9
*/
module class_tb ();
	class test_cls;
		static int s = 24;
	endclass

	test_cls test_obj0;
	test_cls test_obj1;

	initial begin
		test_obj0 = new;
		test_obj1 = new;

		test_obj0.s = 12;
		$display(test_obj0.s);
		test_obj0.s = 13;
		$display(test_obj1.s);
	end
endmodule
