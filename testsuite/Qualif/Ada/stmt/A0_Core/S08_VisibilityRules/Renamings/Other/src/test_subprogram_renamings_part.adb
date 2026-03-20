--  Test driver for subprogram renamings. It calls some subprograms from the
--  functional code using names defined by subprogram renamings. Only the code
--  of these subprograms shall be reported as covered, all the other code shall
--  be reported as uncovered.

with Pack;          use Pack;
with Renaming_Pack; use Renaming_Pack;
with Lib_Level_Proc_Renaming;
with Lib_Level_Fun_Renaming;

with Support;         use Support;
procedure Test_Subprogram_Renamings_Part is
   Int : Integer;
begin
   Int := Lib_Level_Fun_Renaming (1);
   Assert (Int = 0);

   Int := F1 (0);
   Assert (Int = 0);

   P1 (Int);
   Assert (Int = 3);
end Test_Subprogram_Renamings_Part;

--# lib_level_fun.adb
-- /stmt/     l+ ## 0

--# lib_level_proc.adb
-- /stmt/     l- ## s-

--# pack.adb
-- /local/    l+ ## 0
-- /p1/       l+ ## 0
-- /p2/       l+ ## 0
-- /p3/       l- ## s-
-- /f1/       l+ ## 0
-- /f2/       l- ## s-
-- /f3/       l- ## s-
