pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Life;        use Life;

procedure Test_Part is
begin
   Put_Line(Animal_Is_Meowing (Cat)'Image);
end Test_Part;

--# life.adb
-- /dog/    l! ## g-
-- /cow/    l! ## g-
