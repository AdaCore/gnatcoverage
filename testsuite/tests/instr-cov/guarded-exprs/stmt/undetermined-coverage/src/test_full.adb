with Ada.Text_IO; use Ada.Text_IO;
with Animal; use Animal;

procedure Test_Full is
begin
   Put_Line (Scream (Dog));
   Put_Line (Scream (Cat));
   Put_Line (Scream (Cow));
end Test_Full;

--# animal.adb
--  /case_root/  l+ ## 0
--  /alt_dog/    l? ## g?
--  /alt_cat/    l? ## g?
--  /alt_cow/    l? ## g?
