with Support, Minx2; use Support;

-- Verify that only the "min is 2" part is reported uncovered when
-- calling once with X < 2.

procedure Test_Minx2_3 is
begin
   Assert (Minx2 (3) = 2);
end;

--# minx2.adb
--  /(Call|MinIs2)/  l+ 0
--  /MinIsX/         l- s-
