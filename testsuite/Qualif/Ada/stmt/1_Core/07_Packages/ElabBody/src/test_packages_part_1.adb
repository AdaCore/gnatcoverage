--  Test driver for packages. It "with's" all the functional code, but does not
--  execute anything from it. This results in elaboration of the code of
--  library package (and all the code it calls) only, all the rest shall be
--  reported as uncovered.

with Library_Level_Proc;
with Pack_1;          use Pack_1;
with Pack_2;          use Pack_2;
with Support;         use Support;
procedure Test_Packages_Part_1 is
begin
   Assert (True);
end Test_Packages_Part_1;

--# library_level_proc.adb
-- /proc/        l- ## s-

--# pack_1.adb
-- /fun/         l+ ## 0
-- /proc_1/      l- ## s-

--# pack_2.ads
-- /elab/        ~l+ ## ~0

--# pack_2.adb
-- /proc_2/      l- ## s-
-- /elab/        l+ ## 0
