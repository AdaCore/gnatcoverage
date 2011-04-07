with Support, Points; use Support, Points;

--  Call Set and Same_X - not Same_X neither Same_XY.

--  Verify stmt and partial coverage on preconditions for the first 2, and
--  stmts uncovered for everything on the last 2.

procedure Test_Points_1 is
   P1, P2 : Point;
begin
   Set (P1, X => 0, Y => 1);
   Set (P2, X => 0, Y => 2);

   Assert (Same_X (P1, P2));
end;

--# points.adb
--  /bodySX/   l+ 0
--  /bodySY/   l- s-
--  /bodySet/  l+ 0
--  /preSXY/   l- s-
--  /retSXY/   l- s-
--  /exempt/   l* x+
--  /preSX$/   l! dF-
--  /preSY$/   l- s-
