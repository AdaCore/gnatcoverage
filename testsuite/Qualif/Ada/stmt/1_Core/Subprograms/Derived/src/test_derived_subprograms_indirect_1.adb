--  Test driver for derived subprograms. It "with's" the functional code and
--  executes only a part of it. It contains a subprogram call resulted from
--  evaluating default initialization expression, an indirect call through
--  access-to-subprogram value and a dispatching call.

with Derived_1;       use Derived_1;
with Derived_2;       use Derived_2;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Derived_Subprograms_Indirect_1 is
   Var_T           : T           := (I => 1);
   Var_Derived_T_1 : Derived_T_1 := (I => 2);
   Var_Derived_T_2 : Derived_T_2 := (I => 3);
   Int             : Integer;

   Rec             : Rec1;
   --  Call to Fun1 from evaluation of default expression for record
   --  component

   Indirect_Call : Access_DT1_Proc;
begin
   Assert (Rec.Comp = 0);

   Int := Fun3 (Var_Derived_T_2);
   --  Call to Subprogram_Pack.Fun2 from evaluation of default expression for
   --  a parameter. Note that Derived_2.Fun2 shall be reported as uncovered!
   Assert (Int = 5);

   Class_Wide_Proc (T'Class (Var_Derived_T_2));
   --  Results in dispatching to Derived_2.Proc2, all the other Proc2 shall be
   --  reported as uncovered
   Assert (Var_Derived_T_2.I = 13);

   Indirect_Call := Proc1'Access;
   Indirect_Call (Var_Derived_T_1);
   Assert (Var_Derived_T_1.I = 3);
end Test_Derived_Subprograms_Indirect_1;

--# subprogram_pack.adb
-- /fun1/       l+ 0
-- /fun2/       l+ 0
-- /fun3/       l+ 0
-- /proc1/      l+ 0
-- /proc2/      l- s-
-- /class_wide/ l+ 0

--# derived_1.adb
-- /proc2/      l- s-

--# derived_2.adb
-- /fun2/       l- s-
-- /proc2/      l+ 0
