with Support, Minx2; use Support;

-- Verify that everything is reported uncovered when nothing is called.

procedure Test_Minx2_0 is
begin
   Assert (MinX2 (0) = 0);
end;

--# minx2.adb
--  /common/ l+ 0
--  /xle2/   l+ 0
--  /xgt0/   l- s-
--  /xgt2/   l- s-
