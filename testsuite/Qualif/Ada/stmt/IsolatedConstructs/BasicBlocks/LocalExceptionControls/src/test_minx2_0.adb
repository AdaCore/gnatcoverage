with Support, Minx2; use Support;

-- Verify that everything is reported uncovered when nothing is called.

procedure Test_Minx2_0 is
begin
   Assert (True);
end;

--# minx2.adb
--  /Call/   l- s-
--  /MinIsX/ l- s-
--  /MinIs2/ l- s-
