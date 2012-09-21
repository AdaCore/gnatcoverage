--  Test driver for derived subprograms. It only "with's" the functional code,
--  but does not execute anything from it, so everything is expected to be
--  reported as uncovered.

with Derived_1;       use Derived_1;
with Derived_2;       use Derived_2;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Derived_Subprograms_No is
begin
   Assert (True);
end Test_Derived_Subprograms_No;

--# subprogram_pack.adb
-- /fun1/       l- ## s-
-- /fun2/       l- ## s-
-- /fun3/       l- ## s-
-- /proc1/      l- ## s-
-- /proc2/      l- ## s-
-- /class_wide/ l- ## s-

--# derived_1.adb
-- /proc2/      l- ## s-

--# derived_2.adb
-- /fun2/       l- ## s-
-- /proc2/      l- ## s-
