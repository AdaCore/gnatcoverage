--  Test driver for subprogram renamings. It calls all the subprograms from the
--  functional code using names defined by subprogram renamings. All the
--  functional code is expected to be reported as covered.

with Pack;          use Pack;
with Renaming_Pack; use Renaming_Pack;
with Lib_Level_Proc_Renaming;
with Lib_Level_Fun_Renaming;

with Support;         use Support;
procedure Test_Subprogram_Renamings_Full is
   Int : Integer;
begin
   Int := Lib_Level_Fun_Renaming (1);
   Assert (Int = 0);

   Int := F1 (0);
   Assert (Int = 0);

   P1 (Int);
   Assert (Int = 3);

   Lib_Level_Proc_Renaming (Int);
   Assert (Int = 4);

   Q3 (Int);
   Assert (Int = 7);

   Int := R2;
   Assert (Int = 3);

   Int := R3 (0);
   Assert (Int = 3);
end Test_Subprogram_Renamings_Full;

--# lib_level_fun.adb
-- /stmt/     l+ ## 0

--# lib_level_proc.adb
-- /stmt/     l+ ## 0

--# pack.adb
-- /local/    l+ ## 0
-- /p1/       l+ ## 0
-- /p2/       l+ ## 0
-- /p3/       l+ ## 0
-- /f1/       l+ ## 0
-- /f2/       l+ ## 0
-- /f3/       l+ ## 0
