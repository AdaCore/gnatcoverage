with Support, Minx2; use Support;

-- Exercize with X = 3 and verify that nothing is reported uncovered.

procedure Test_Minx2_3 is
begin
   Assert (MinX2 (3) = 2);
end;

--# minx2.adb
--  /./ l+ 0
