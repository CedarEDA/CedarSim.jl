// Copyright (C) 2019-2021  The SymbiFlow Authors.
//
// Use of this source code is governed by a ISC-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/ISC
//
// SPDX-License-Identifier: ISC


/*
:name: structure-replication
:description: Structure replication assignment tests
:tags: 5.10
*/
module top();
  struct {int X,Y,Z;} XYZ = '{3{1}};
  typedef struct {int a,b[4];} ab_t;
  int a,b,c;

  initial begin
    ab_t v1[1:0] [2:0];
    v1 = '{2{'{3{'{a,'{2{b,c}}}}}}};
  end

endmodule
