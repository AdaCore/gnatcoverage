--  Test driver for subprogram declarations and subprogram body declarations.
--  It only "with's" a part of the functional code, but does not execute
--  anything from it, so everything is expected to be reported as uncovered.

with Library_Level_Fun;
with Library_Level_Proc;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Subprogram_Decls_No is
begin
   Assert (True);
end Test_Subprogram_Decls_No;

--# library_level_fun.adb
-- /fun/        l- ## s-

--# library_level_proc.adb
-- /proc/       l- ## s-

--# subprogram_pack.adb
-- /fun1/       l- ## s-
-- /fun2/       l- ## s-
-- /proc1/      l- ## s-
-- /proc2/      l- ## s-
-- /proc3/      l- ## s-
-- /proc4/      l- ## s-
-- /local_proc/ l- ## s-
-- /local_fun/  l- ## s-
