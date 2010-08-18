with Support, Minx2; use Support;

-- Exercize with X = 0 and verify that everything past that case in the loop
-- body is reported uncovered.

procedure Test_Minx2_0 is
begin
   Assert (MinX2 (0) = 0);
end;

--# minx2.adb
--  /(x1|x2|xnot2)/ l- s-
