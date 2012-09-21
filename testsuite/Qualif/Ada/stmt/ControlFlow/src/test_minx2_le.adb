with Support, Minx2; use Support;

-- Verify that only the xgt2 part is reported uncovered when calling
-- twice, with X < 2 and X = 2.

procedure Test_Minx2_LE is
begin
   Assert (Minx2 (1) = 1);
   Assert (Minx2 (2) = 2);
end;

--# minx2.adb
--  /common/ l+ ## 0
--  /xgt0/   l+ ## 0
--  /xgt2/   l- ## s-
--  /xle2/   l+ ## 0


