pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Life;        use Life;

procedure Test_Full is
begin
   Put_Line(Animal_Is_Meowing (Dog)'Image);
   Put_Line(Animal_Is_Meowing (Cat)'Image);
   Put_Line(Animal_Is_Meowing (Cow)'Image);
end Test_Full;

--# life.adb
-- /return/    l+ ## 0
-- /decl/      l+ ## 0
-- /sound/     l+ ## 0
-- /case/      l+ ## 0
-- /dog/       l+ ## 0
-- /cat/       l+ ## 0
-- /cow/       l+ ## 0
-- /begin/     l+ ## 0
-- /cmp/       l+ ## 0
