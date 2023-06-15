--  Test driver for checking the coverage information in case of dispatching
--  in the situation of multiple inheritance. The dispatching takes place in
--  a class-wide operation that is defined for an interface type, and this
--  interface is used in more then one inheritance hierarchies. The check is
--  made that only the actually called implementations of the operations
--  defined for interfaces are reported as covered.

with A;         use A;
with B;         use B;
with C;         use C;

with X;         use X;
with X.A_Child; use X.A_Child;
with X.B_Child; use X.B_Child;
with X.C_Child; use X.C_Child;

with Y;         use Y;
with Y.A_Child; use Y.A_Child;
with Y.B_Child; use Y.B_Child;
with Y.C_Child; use Y.C_Child;

with Support;          use Support;

procedure Test_7 is
   Var : T_Y_A := (I1 => 1, I2 => 1);
begin
   Class_Wide_IA (Var);
   Assert (Var.I1 = -9 and then Var.I2 = -19);
end Test_7;

--# a.adb
--  /class_wide_ia/  l+ ## 0
--  /if_cw_ia/       l- ## s-
--  /else_cw_ia/     l+ ## 0

--# b.adb
--  /class_wide_ib/  l- ## s-
--  /if_cw_ib/       l- ## s-
--  /else_cw_ib/     l- ## s-

--# c.adb
--  /class_wide_ic/  l- ## s-
--  /if_cw_ic/       l- ## s-
--  /else_cw_ic/     l- ## s-

--# x-a_child.adb
--  /xa_p_ia_1/      l- ## s-
--  /xa_p_ia_2/      l- ## s-
--  /xa_test_ia/     l- ## s-

--# x-b_child.adb
--  /xb_p_ib_1/      l- ## s-
--  /xb_p_ib_2/      l- ## s-
--  /xb_test_ib/     l- ## s-

--# x-c_child.adb
--  /xc_test_ia/     l- ## s-
--  /xc_test_ib/     l- ## s-
--  /xc_test_ic/     l- ## s-

--# y-a_child.adb
--  /ya_p_ia_1/      l- ## s-
--  /ya_p_ia_2/      l+ ## 0
--  /ya_test_ia/     l+ ## 0

--# y-b_child.adb
--  /yb_p_ib_1/      l- ## s-
--  /yb_p_ib_2/      l- ## s-
--  /yb_test_ib/     l- ## s-

--# y-c_child.adb
--  /yc_test_ia/     l- ## s-
--  /yc_test_ib/     l- ## s-
--  /yc_test_ic/     l- ## s-
