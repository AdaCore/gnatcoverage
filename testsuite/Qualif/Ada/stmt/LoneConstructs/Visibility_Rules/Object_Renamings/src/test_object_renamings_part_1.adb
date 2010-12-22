--  Test driver for object renamings. It "with's" all the functional code, but
--  it does not execute anything from it. However, the elaboration of a withed
--  Libray_Level_Renamings package causes some code to be reported as covered.

with Libray_Level_Renamings; use Libray_Level_Renamings;
with Local_Renamings;        use Local_Renamings;
with Renamed_Objects;        use Renamed_Objects;
with Support;                use Support;
procedure Test_Object_Renamings_Part_1 is
begin
   Arr_Var := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   Assert (Renamed_Comp_1 = 1);
end Test_Object_Renamings_Part_1;

--# libray_level_renamings.ads
-- /renaming/         l+ 0

--# renamed_objects.adb
-- /get_index/        l+ 0
-- /if_get_index/     l+ 0
-- /else_get_index/   l- s-

--# local_renamings.adb
-- /get_comp/         l- s-
-- /get_another_comp/ l- s-
