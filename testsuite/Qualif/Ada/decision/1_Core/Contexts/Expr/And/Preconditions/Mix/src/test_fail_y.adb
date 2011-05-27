with Support, Points, Silent_Last_Chance; use Support, Points;

--  Call Same_X and Same_XY with regular arguments. Call Same_Y with an
--  unset second point, causing a precondition failure and only that for
--  the associated expression.

--  Verify full stmt coverage nevertheless, except for the Same_Y body.

procedure Test_Fail_Y is
   P1, P2 : Point;
begin
   Set (P1, X => 0, Y => 1);

   Assert (Same_X (P1, P1));
   Assert (Same_XY (P1, P1));

   P2.Y := 1; -- improper way to set
   Assert (Same_Y (P1, P2));
end;

--# points.adb
--  /bodySX/   l+ 0
--  /bodySY/   l- s-
--  /bodySet/  l+ 0
--  /preSXY/   l+ 0
--  /retSXY/   l+ 0

--# points.ads
--  /preSX/    l+ 0
--  /preSY/    l+ 0
