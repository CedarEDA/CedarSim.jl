// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: out_of_block_methods
:description: out-of-body method declaration
:tags: 8.24
*/
module class_tb ();
	class test_cls;
		int a;
		extern function void test_method(int val);
	endclass

	function void test_cls::test_method(int val);
		$display("test_method");
		a += val;
	endfunction

	test_cls test_obj;

	initial begin
		test_obj = new;

		test_obj.a = 12;

		$display(test_obj.a);

		test_obj.test_method(9);

		$display(test_obj.a);
	end
endmodule
