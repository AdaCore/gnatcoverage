--  Test driver for subprogram renamings. It only "with's" the functional code
--  but does not execute anything from it. So nothing is expected to be
--  reported as covered.

with Pack;          use Pack;
with Renaming_Pack; use Renaming_Pack;
with Lib_Level_Proc_Renaming;
with Lib_Level_Fun_Renaming;

with Support;         use Support;
procedure Test_Subprogram_Renamings_No is
begin
   Assert (True);
end Test_Subprogram_Renamings_No;

--# lib_level_fun.adb
-- /stmt/     l- s-

--# lib_level_proc.adb
-- /stmt/     l- s-

--# pack.adb
-- /local/    l- s-
-- /p1/       l- s-
-- /p2/       l- s-
-- /p3/       l- s-
-- /f1/       l- s-
-- /f2/       l- s-
-- /f3/       l- s-
