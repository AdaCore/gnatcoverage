with Ada.Text_IO; use Ada.Text_IO;
with Life;        use Life;

procedure Test_All_Enum_True_False is
begin
   Put_Line (Scream (Dog, True));
   Put_Line (Scream (Cat, False));
   Put_Line (Scream (Cow, False));
end Test_All_Enum_True_False;

--# life.adb
-- /dog/     l! ## g-
-- /cat/     l+ ## 0
-- /cow/     l+ ## 0
