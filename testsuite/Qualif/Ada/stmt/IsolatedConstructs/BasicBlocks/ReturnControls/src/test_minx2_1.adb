with Support, Minx2; use Support;

-- Verify that only the "min is 2" part is reported uncovered when
-- calling once with X < 2.

procedure Test_Minx2_1 is
begin
   Assert (Minx2 (1) = 1);
end;

--# minx2.adb
--  /(Call|MinIsX)/ l+ 0
--  /MinIs2/        l- s-
