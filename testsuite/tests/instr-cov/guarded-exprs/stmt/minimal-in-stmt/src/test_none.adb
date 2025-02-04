with Ada.Text_IO; use Ada.Text_IO;
with Animal; use Animal;

procedure Test_None is
begin
   null;
end Test_None;

--# animal.adb
--  /return/     l- ## s-
--  /case_root/  l- ## 0
--  /alt_dog/    l- ## g-
--  /alt_cat/    l- ## g-
--  /alt_cow/    l- ## g-
