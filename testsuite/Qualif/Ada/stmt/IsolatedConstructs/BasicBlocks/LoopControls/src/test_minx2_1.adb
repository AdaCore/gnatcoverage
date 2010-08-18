with Support, Minx2; use Support;

-- Exercize with X = 1 and verify that everything past that case in the loop
-- body is reported uncovered.

procedure Test_Minx2_1 is
begin
   Assert (MinX2 (1) = 1);
end;

--# minx2.adb
--  /(x2|xnot2)/ l- s-
