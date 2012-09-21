--  Test driver for derived subprograms. It executes all of the functional code
--  using different ways to call subprograms (direct, indirect, dispatching
--  calls). All the functional code is expected to be reported as being
--  covered.

with Derived_1;       use Derived_1;
with Derived_2;       use Derived_2;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Derived_Subprograms_Full is
   Var_T           : T           := (I => 1);
   Var_Derived_T_1 : Derived_T_1 := (I => 2);
   Var_Derived_T_2 : Derived_T_2 := (I => 3);
   Int             : Integer;

   Var_Rec1        : Rec1;
   --  Call to Fun1 from evaluation of default expression for record
   --  component

   Var_Rec2        : Rec2;
   --  Call to overriding Derived_2.Fun2 from evaluation of default expression
   --  for record component.

   Indirect_Call : Access_DT2_Proc;
begin
   Assert (Var_Rec1.Comp = 0);
   Assert (Var_Rec2.Comp = -1);

   Int := Fun3 (Var_Derived_T_1);
   --  Call to Subprogram_Pack.Fun2 from evaluation of default expression for
   --  a parameter.
   Assert (Int = 4);

   Class_Wide_Proc (T'Class (Var_T));
   --  Results in dispatching to Subprogram_Pack.Proc2.
   Assert (Var_T.I = -1);

   Indirect_Call := Proc2'Access;
   Indirect_Call (Var_Derived_T_2);
   Assert (Var_Derived_T_2.I = 13);

   Proc1 (Var_Derived_T_2);
   Assert (Var_Derived_T_2.I = 14);

   Proc2 (Var_Derived_T_1);
   Assert (Var_Derived_T_1.I = 4);
end Test_Derived_Subprograms_Full;

--# subprogram_pack.adb
-- /fun1/       l+ ## 0
-- /fun2/       l+ ## 0
-- /fun3/       l+ ## 0
-- /proc1/      l+ ## 0
-- /proc2/      l+ ## 0
-- /class_wide/ l+ ## 0

--# derived_1.adb
-- /proc2/      l+ ## 0

--# derived_2.adb
-- /fun2/       l+ ## 0
-- /proc2/      l+ ## 0
