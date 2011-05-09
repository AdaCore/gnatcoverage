with Support, Points; use Support, Points;

--  Call Set, Same_Y and same_XY - not Same_X.

--  Verify partial mcdc coverage on preconditions for the first 3, and stmts
--  uncovered for everything on the last one. Account for the decision in
--  Same_XY as well.

procedure Test_Points_2 is
   P1, P2 : Point;
begin
   Set (P1, X => 0, Y => 1);
   Set (P2, X => 0, Y => 2);

   Assert (not Same_Y (P1, P2));
   Assert (not Same_XY (P1, P2));
end;

--# points.adb
--  /bodySX/   l- s-
--  /bodySY/   l+ 0
--  /bodySet/  l+ 0
--  /preSXY/   l+;mu=>l! mu => dF-
--  /retSXY/   l+;mu=>l! mu => dT-

--# points.ads
--  /preSX/    l- s-
--  /preSY/    l+;mu=>l! mu => dF-
