with Support, Minx2; use Support;

--  Verify that everything is reported uncovered when nothing is called.

--  We might have code associated with the exception declaration in
--  some runtime profiles, which would trigger expected stmt violations.

procedure Test_Minx2_Null is
begin
   Assert (True);
end;

--# minx2.adb
--  /decl/   ~l- ~s-
--  /common/ l- s-
--  /xle2/   l- s-
--  /xgt0/   l- s-
--  /xgt2/   l- s-
