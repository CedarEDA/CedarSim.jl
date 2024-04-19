// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: static_methods
:description: static class methods test
:tags: 8.10
*/
module class_tb ();
	class test_cls;
		static int id = 0;
		static function int next_id();
			++id;
			next_id = id;
		endfunction
	endclass

	test_cls test_obj0;
	test_cls test_obj1;

	initial begin
		test_obj0 = new;
		test_obj1 = new;

		$display(test_obj0.next_id());
		$display(test_obj1.next_id());
	end
endmodule
