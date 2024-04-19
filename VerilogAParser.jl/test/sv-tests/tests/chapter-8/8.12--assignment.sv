// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: assignment
:description: object assignment
:tags: 8.12
*/
module class_tb ();
	class test_cls;
		int a;
		task test_method(int val);
			$display("test_method");
			a += val;
		endtask
	endclass

	test_cls test_obj0;
	test_cls test_obj1;

	initial begin
		test_obj0 = new;
		test_obj1 = test_obj0;

		test_obj0.a = 12;

		$display(test_obj0.a);

		test_obj0.test_method(9);

		$display(test_obj1.a);
	end
endmodule
