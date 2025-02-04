with Ada.Text_IO; use Ada.Text_IO;
with Life; use Life;

procedure Test_None is
begin
   null;
end Test_None;

--# life.adb
--  /return/         l- ## s-
--  /case_animal/    l- ## 0
--  /when_mammal/    l- ## g-
--  /case_mammal/    l- ## 0
--  /when_dog/       l- ## g-
--  /when_cat/       l- ## g-
--  /when_cow/       l- ## g-
--  /when_reptile/   l- ## g-
--  /case_reptile/   l- ## 0
--  /when_snake/     l- ## g-
--  /when_turtle/    l- ## g-
