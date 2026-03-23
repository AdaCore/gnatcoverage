--  Test driver for object renamings. It only "with's" the part of the
--  functional code that does not contain library-level renamings, but does not
--  execute anything from it. So everything is expected to be reported as
--  uncovered.

with Local_Renamings; use Local_Renamings;
with Support;         use Support;
procedure Test_Object_Renamings_No is
begin
   Assert (True);
end Test_Object_Renamings_No;

--#  renamed_objects.adb
-- /get_index/        l- ## s-
-- /if_get_index/     l- ## s-
-- /else_get_index/   l- ## s-

--# local_renamings.adb
-- /get_comp/         l- ## s-
-- /get_another_comp/ l- ## s-
