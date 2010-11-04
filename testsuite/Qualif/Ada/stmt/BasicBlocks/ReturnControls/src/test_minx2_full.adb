with Support, Minx2; use Support;

-- Verify that nothing is reported uncovered when exercizing
-- the two possible functional variations.

procedure Test_Minx2_Full is
begin
   Assert (Minx2 (1) = 1);
   Assert (Minx2 (3) = 2);
end;

--# minx2.adb
--  /(Call|MinIsX|MinIs2)/  l+ 0

