--  Test driver for derived subprograms. It "with's" the functional code and
--  executes only a part of it. There is no subprogram calls resulted from
--  evaluating default initialization expressions in this test, no indirect
--  calls through access-to-subprogram values and no dispatching calls.

with Derived_1;       use Derived_1;
with Derived_2;       use Derived_2;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Derived_Subprograms_Part is
   Var_T           : T           := (I => 1);
   Var_Derived_T_1 : Derived_T_1 := (I => 2);
   Var_Derived_T_2 : Derived_T_2 := (I => 3);
   Int             : Integer;
begin
   Int := Fun3 (Var_Derived_T_2, 1);
   Assert (Int = 4);

end Test_Derived_Subprograms_Part;

--# subprogram_pack.adb
-- /fun1/       l- s-
-- /fun2/       l- s-
-- /fun3/       l+ 0
-- /proc1/      l- s-
-- /proc2/      l- s-
-- /class_wide/ l- s-

--# derived_1.adb
-- /proc2/      l- s-

--# derived_2.adb
-- /fun2/       l- s-
-- /proc2/      l- s-
