with Support, Andidor; use Support;

--  One evaluation with X False.
--  Outer decision False, Inner decision never evaluated.

procedure Test_Andidor_FFF is
begin
   Assert (Andidor (X => False, A => False, B => False) = False);
end;

--# andidor.adb
--  /eval/ l! dT-:"X", e-:"A"
--  /true/  l- s-
--  /false/ l+ 0
