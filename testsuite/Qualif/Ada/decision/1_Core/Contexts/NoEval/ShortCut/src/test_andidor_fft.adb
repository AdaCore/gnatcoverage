with Support, Andidor; use Support;

--  One evaluation with X False.
--  Outer decision False, Inner decision never evaluated.

procedure Test_Andidor_FFT is
begin
   Assert (Andidor (X => False, A => False, B => True) = False);
end;

--# andidor.adb
--  /eval/ l! dT-:"X", mu=>d-:"A"
--  /true/  l- s-
--  /false/ l+ 0
