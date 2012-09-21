--  Test driver for packages. It "with's" all the functional code and executes
--  a part from it. This results in elaboration of the code of
--  library package (and all the code it calls) and some of the local packages,
--  all the rest shall be reported as uncovered.

with Library_Level_Proc;
with Pack_1;          use Pack_1;
with Pack_2;          use Pack_2;
with Support;         use Support;
procedure Test_Packages_Part_2 is
   Int : Integer := 0;
begin
   Library_Level_Proc (Int);
   Assert (Int = 5);
end Test_Packages_Part_2;

--# library_level_proc.adb
-- /proc/        l+ ## 0

--# pack_1.adb
-- /fun/         l+ ## 0
-- /proc_1/      l+ ## 0

--# pack_2.ads
-- /elab/        ~l+ ## ~0

--# pack_2.adb
-- /proc_2/      l- ## s-
-- /elab/        l+ ## 0
