--  Test driver for subprogram declarations and subprogram body declarations.
--  It executes a part of the functional code. It calls subprograms from the
--  functional code using only normal explicit subprogram calls. So for some
--  subprograms the code in their bodies is expected to be reported as covered,
--  and for other subprograms - as uncovered.

with Library_Level_Fun;
with Library_Level_Proc;
with Subprogram_Pack; use Subprogram_Pack;
with Support;         use Support;
procedure Test_Subprogram_Decls_Ordinary_Calls is
   I : Integer;
begin
   I := 0;
   Library_Level_Proc (I);
   Assert (I = 1);

   I := Fun1 (10);
   Assert (I = 9);

   Proc1 (I);
   Assert (I = 6);

   Proc2 (I, 0);
   Assert (I = 4);

end Test_Subprogram_Decls_Ordinary_Calls;

--# library_level_fun.adb
-- /fun/        l- s-

--# library_level_proc.adb
-- /proc/       l+ 0

--# subprogram_pack.adb
-- /fun1/       l+ 0
-- /fun2/       l- s-
-- /proc1/      l+ 0
-- /proc2/      l+ 0
-- /proc3/      l- s-
-- /proc4/      l- s-
-- /local_proc/ l+ 0
-- /local_fun/  l+ 0
