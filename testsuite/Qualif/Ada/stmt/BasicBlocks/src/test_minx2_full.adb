with Support, Minx2; use Support;

-- Verify that nothing is reported uncovered when exercizing
-- all the possible functional variations.

procedure Test_Minx2_Full is
begin
   Assert (Minx2 (1) = 1);
   Assert (Minx2 (2) = 2);
   Assert (Minx2 (3) = 2);
end;

--# minx2.adb
--  /common/ l+ 0
--  /xgt0/   l+ 0
--  /xgt2/   l+ 0
--  /xle2/   l+ 0


