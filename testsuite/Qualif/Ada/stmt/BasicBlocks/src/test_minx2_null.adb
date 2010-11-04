with Support, Minx2; use Support;

-- Verify that everything is reported uncovered when nothing is called.

procedure Test_Minx2_Null is
begin
   Assert (True);
end;

--# minx2.adb
--  /common/ l- s-
--  /xle2/   l- s-
--  /xgt0/   l- s-
--  /xgt2/   l- s-
