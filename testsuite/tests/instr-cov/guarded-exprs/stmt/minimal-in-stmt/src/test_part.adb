with Ada.Text_IO; use Ada.Text_IO;
with Animal; use Animal;

procedure Test_Part is
begin
   Put_Line (Scream (Cat));
end Test_Part;

--# animal.adb
--  /case_root/  l+ ## 0
--  /alt_dog/    l! ## g-
--  /alt_cat/    l+ ## 0
--  /alt_cow/    l! ## g-
