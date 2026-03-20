with Ada.Text_IO; use Ada.Text_IO;
with Life;        use Life;

procedure Test_If_True is
begin
   Put_Line (Scream (Cow, True));
end Test_If_True;

--# life.adb
-- /if_cond/ l! ## dT-
-- /case/    l! ## g-
-- /dog/     l! ## g-
-- /cat/     l! ## g-
-- /cow/     l! ## g-
