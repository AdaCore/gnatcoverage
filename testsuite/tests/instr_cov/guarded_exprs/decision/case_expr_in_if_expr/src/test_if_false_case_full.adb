with Ada.Text_IO; use Ada.Text_IO;
with Life;        use Life;

procedure Test_If_False_Case_Full is
begin
   Put_Line (Scream (Dog, False));
   Put_Line (Scream (Cat, False));
   Put_Line (Scream (Cow, False));
end Test_If_False_Case_Full;

--# life.adb
-- /if_cond/ l! ## dF-
-- /dog/     l+ ## 0
-- /cat/     l+ ## 0
-- /cow/     l+ ## 0
-- /else/    l! ## g-
