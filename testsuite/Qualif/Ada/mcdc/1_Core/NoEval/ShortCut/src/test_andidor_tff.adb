with Support, Andidor; use Support;

--  One evaluation with X True.
--  Inner decision False. Outer decision False.

procedure Test_Andidor_TFF is
begin
   Assert (Andidor (X => True, A => False, B => False) = False);
end;

--# andidor.adb
--  /eval/ l! dT-:"X", mu=>dT-:"A"
--  /true/  l- s-
--  /false/ l+ 0
