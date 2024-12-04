with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_Full is
begin
   Put_Line (Foo (Dog, True, True));
   Put_Line (Foo (Cat, True, True));
   Put_Line (Foo (Cow, True, True));
   Put_Line (Foo (Cow, True, False));
   Put_Line (Foo (Cow, False, False));
end Test_Full;

--# pkg.adb
-- /return/ l+ ## 0
-- /case/   l+ ## 0
-- /dog/    l+ ## 0
-- /cat/    l+ ## 0
-- /cow/    l+ ## 0
-- /cond/   l+ ## 0
-- /true/   l+ ## 0
-- /false/  l+ ## 0
