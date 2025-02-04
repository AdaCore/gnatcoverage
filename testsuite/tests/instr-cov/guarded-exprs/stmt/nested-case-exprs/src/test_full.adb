with Ada.Text_IO; use Ada.Text_IO;
with Life; use Life;

procedure Test_Full is
   A_Dog    : constant Animal := (T => Mammal, M_Kind => Dog);
   A_Cat    : constant Animal := (T => Mammal, M_Kind => Cat);
   A_Cow    : constant Animal := (T => Mammal, M_Kind => Cow);
   A_Snake  : constant Animal := (T => Reptile, R_Kind => Snake);
   A_Turtle : constant Animal := (T => Reptile, R_Kind => Turtle);
begin
   Put_Line (Scream (A_Dog));
   Put_Line (Scream (A_Cat));
   Put_Line (Scream (A_Cow));
   Put_Line (Scream (A_Snake));
   Put_Line (Scream (A_Turtle));
end Test_Full;

--# life.adb
--  /return/         l+ ## 0
--  /when_mammal/    l+ ## 0
--  /when_dog/       l+ ## 0
--  /when_cat/       l+ ## 0
--  /when_cow/       l+ ## 0
--  /when_reptile/   l+ ## 0
--  /when_snake/     l+ ## 0
--  /when_turtle/    l+ ## 0
