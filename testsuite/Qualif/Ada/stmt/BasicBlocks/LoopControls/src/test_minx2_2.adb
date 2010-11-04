with Support, Minx2; use Support;

-- Exercize with X = 2 and verify that only the latest loop exit
-- is reported uncovered.

procedure Test_Minx2_2 is
begin
   Assert (MinX2 (2) = 2);
end;

--# minx2.adb
--  /xnot2/ l- s-
