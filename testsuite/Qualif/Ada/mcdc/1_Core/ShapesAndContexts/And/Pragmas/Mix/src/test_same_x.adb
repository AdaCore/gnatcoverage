with Support, Points; use Support, Points;

--  Call Set and Same_X - not Same_Y neither Same_XY.

procedure Test_Same_X is
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

--# points.ads
--  /preSX/    l! dF-
--  /preSY/    l- s-
