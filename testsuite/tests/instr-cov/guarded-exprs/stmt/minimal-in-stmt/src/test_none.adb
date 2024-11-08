with Ada.Text_IO; use Ada.Text_IO;
with Animal; use Animal;

procedure Test_Case_Expr_None is
begin
   null;
end Test_Case_Expr_None;

--# animal.adb
--  /return/     l- ## s-
--  /case_root/  l- ## 0
--  /alt_dog/    l- ## g-
--  /alt_cat/    l- ## g-
--  /alt_cow/    l- ## g-
