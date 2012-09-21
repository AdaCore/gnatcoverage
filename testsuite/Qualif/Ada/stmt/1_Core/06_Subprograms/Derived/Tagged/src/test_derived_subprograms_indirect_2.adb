--  Test driver for derived subprograms. It "with's" the functional code and
--  executes only a part of it. It contains a subprogram call resulted from
--  evaluating default initialization expression and a dispatching call.

with Derived_1;       use Derived_1;
with Derived_2;       use Derived_2;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Derived_Subprograms_Indirect_2 is
   Var_T           : T           := (I => 1);
   Var_Derived_T_1 : Derived_T_1 := (I => 2);
   Var_Derived_T_2 : Derived_T_2 := (I => 3);

   Rec             : Rec2;
   --  Call to overriding Derived_2.Fun2 from evaluation of default expression
   --  for record component, overridden function in Subprogram_Pack shall be
   --  reported as uncovered.
begin
   Assert (Rec.Comp = -1);

   Class_Wide_Proc (T'Class (Var_Derived_T_1));
   --  Results in dispatching to Derived_1.Proc2, all the other Proc2 shall be
   --  reported as uncovered
   Assert (Var_Derived_T_1.I = 4);

end Test_Derived_Subprograms_Indirect_2;

--# subprogram_pack.adb
-- /fun1/       l- ## s-
-- /fun2/       l- ## s-
-- /fun3/       l- ## s-
-- /proc1/      l- ## s-
-- /proc2/      l- ## s-
-- /class_wide/ l+ ## 0

--# derived_1.adb
-- /proc2/      l+ ## 0

--# derived_2.adb
-- /fun2/       l+ ## 0
-- /proc2/      l- ## s-
