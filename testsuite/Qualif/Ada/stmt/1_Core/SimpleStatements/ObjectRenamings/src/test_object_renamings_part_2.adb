--  Test driver for object renamings. It "with's" all the functional code, and
--  explicitly call a subprogram from it. But some of the functional code
--  still shall be reported as uncovered.

with Libray_Level_Renamings; use Libray_Level_Renamings;
with Local_Renamings;        use Local_Renamings;
with Renamed_Objects;        use Renamed_Objects;
with Support;                use Support;
procedure Test_Object_Renamings_Part_2 is
begin
   Arr_Var         := (10, 20, 30, 40, 50, 60, 70, 80, 90, 100);
   Another_Arr_Var := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   Assert (Renamed_Comp_1 = 10);

   Assert (Get_Another_Comp (3) = 3);
end Test_Object_Renamings_Part_2;

--# libray_level_renamings.ads
-- /renaming/         l+ 0

--# renamed_objects.adb
-- /get_index/        l+ 0
-- /if_get_index/     l+ 0
-- /else_get_index/   l- s-

--# local_renamings.adb
-- /get_comp/         l- s-
-- /get_another_comp/ l+ 0
