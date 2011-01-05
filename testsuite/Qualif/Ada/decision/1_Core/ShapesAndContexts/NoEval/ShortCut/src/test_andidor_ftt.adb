with Support, Andidor; use Support;

--  One evaluation with X False.
--  Outer decision False, Inner decision never evaluated.

procedure Test_Andidor_FTT is
begin
   Assert (Andidor (X => False, A => True, B => True) = False);
end;

--# andidor.adb
--  /eval/ l! dT-:"X", d-:"A"
