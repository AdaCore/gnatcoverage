with Ada.Text_IO; use Ada.Text_IO;
with Life; use Life;

procedure Test_Partial_Root is
   A_Dog    : constant Animal := (T => Mammal, M_Kind => Dog);
   A_Snake  : constant Animal := (T => Reptile, R_Kind => Snake);
begin
   Put_Line (Scream (A_Dog));
   Put_Line (Scream (A_Snake));
end Test_Partial_Root;

--# life.adb
--  /return/         l+ ## 0
--  /when_mammal/    l+ ## 0
--  /when_dog/       l+ ## 0
--  /when_cat/       l! ## g-
--  /when_cow/       l! ## g-
--  /when_reptile/   l+ ## 0
--  /when_snake/     l+ ## 0
--  /when_turtle/    l! ## g-
