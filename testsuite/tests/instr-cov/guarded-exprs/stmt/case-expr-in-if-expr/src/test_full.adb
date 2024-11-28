with Ada.Text_IO; use Ada.Text_IO;
with Life;        use Life;

procedure Test_Full is
begin
   Put_Line (Scream (Dog, True));
   Put_Line (Scream (Cat, True));
   Put_Line (Scream (Cow, True));
   Put_Line (Scream (Dog, False));
   Put_Line (Scream (Cat, False));
   Put_Line (Scream (Cow, False));
end Test_Full;

--# life.adb
-- /dog/     l+ ## 0
-- /cat/     l+ ## 0
-- /cow/     l+ ## 0
