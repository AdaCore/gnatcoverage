--  Test driver for packages. It only "with's" the part of the functional code,
--  but does not execute anything from it. This does not result in elaboration
--  of any package code. So nothing is expected to be reported as covered.

with Library_Level_Proc;
with Pack_1;          use Pack_1;
with Support;         use Support;
procedure Test_Packages_No is
begin
   Assert (True);
end Test_Packages_No;

--# library_level_proc.adb
-- /proc/      l- ## s-

--# pack_1.adb
-- /fun/       l- ## s-
-- /proc_1/    l- ## s-
