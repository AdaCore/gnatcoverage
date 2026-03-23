pragma Ada_2005;

with Show_Whether_Local;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Exc is
begin
   Show_Whether_Local ("x");
   raise Program_Error with "expected C_E";
exception
   when Constraint_Error =>
      Put_Line ("got expected exception");
end Test_Exc;

--# show_whether_local.adb
--  /eval/  l! ## d-
--  /true/  l- ## s-
--  /false/ l- ## s-
--  /ret/   l- ## s-
--  /exc/   l+ ## 0
