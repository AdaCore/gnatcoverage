--  Test driver for packages. It executes all the functional code, so nothing
--  shall be reported as uncovered.

with Library_Level_Proc;
with Pack_1;          use Pack_1;
with Pack_2;          use Pack_2;
with Support;         use Support;
procedure Test_Packages_Full is
   Int : Integer := 0;
begin
   Library_Level_Proc (Int);
   Assert (Int = 5);

   Proc_2 (Int);
   Assert (Int = 8);
end Test_Packages_Full;

--# library_level_proc.adb
-- /proc/        l+ ## 0

--# pack_1.adb
-- /fun/         l+ ## 0
-- /proc_1/      l+ ## 0

--# pack_2.ads
-- /elab/        ~l+ ## ~0

--# pack_2.adb
-- /proc_2/      l+ ## 0
-- /elab/        l+ ## 0
