with Support, Minx2; use Support;

-- Verify that only the xle2 part is reported uncovered when
-- calling once with X > 2.

procedure Test_Minx2_3 is
begin
   Assert (Minx2 (3) = 2);
end;

--# minx2.adb
--  /common/  l+ ## 0
--  /xle2/    l- ## s-
--  /xgt0/    l+ ## 0
--  /xgt2/    l+ ## 0
