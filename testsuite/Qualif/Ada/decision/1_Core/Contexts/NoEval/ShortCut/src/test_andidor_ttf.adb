with Support, Andidor; use Support;

--  One evaluation with X True.
--  Inner decision True. Outer decision True.

procedure Test_Andidor_TTF is
begin
   Assert (Andidor (X => True, A => True, B => False) = True);
end;

--# andidor.adb
--  /eval/ l! dF-:"X", dF-:"A"
