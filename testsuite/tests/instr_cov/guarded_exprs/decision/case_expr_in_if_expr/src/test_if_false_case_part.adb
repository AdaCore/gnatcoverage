with Ada.Text_IO; use Ada.Text_IO;
with Life;        use Life;

procedure Test_If_False_Case_Part is
begin
   Put_Line (Scream (Cow, False));
end Test_If_False_Case_Part ;

--# life.adb
-- /if_cond/ l! ## dF-
-- /dog/     l! ## g-
-- /cat/     l! ## g-
-- /else/    l! ## g-
