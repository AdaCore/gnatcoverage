with Ada.Text_IO; use Ada.Text_IO;
with Life; use Life;

procedure Test_Partial_Leaf is
   A_Dog    : constant Animal := (T => Mammal, M_Kind => Dog);
   A_Cat    : constant Animal := (T => Mammal, M_Kind => Cat);
   A_Cow    : constant Animal := (T => Mammal, M_Kind => Cow);
begin
   Put_Line (Scream (A_Dog));
   Put_Line (Scream (A_Cat));
   Put_Line (Scream (A_Cow));
end Test_Partial_Leaf;

--# life.adb
--  /return/         l+ ## 0
--  /when_mammal/    l+ ## 0
--  /when_dog/       l+ ## 0
--  /when_cat/       l+ ## 0
--  /when_cow/       l+ ## 0
--  /when_reptile/   l! ## g-
--  /case_reptile/   l! ## 0
--  /when_snake/     l! ## g-
--  /when_turtle/    l! ## g-
