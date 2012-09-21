with Support, Points; use Support, Points;

--  Call Set, Same_Y and same_XY - not Same_X.

--  Verify full coverage on everything for the first 3, and stmts
--  uncovered for everything on the last one.

procedure Test_Same_Y_XY is
   P1, P2 : Point;
begin
   Set (P1, X => 0, Y => 1);
   Set (P2, X => 0, Y => 2);

   Assert (not Same_Y (P1, P2));
   Assert (not Same_XY (P1, P2));
end;

--# points.adb
--  /bodySX/   l- ## s-
--  /bodySY/   l+ ## 0
--  /bodySet/  l+ ## 0
--  /preSXY/   l+ ## 0
--  /retSXY/   l+ ## 0

--# points.ads
--  /preSX/    l- ## s-
--  /preSY/    l+ ## 0
