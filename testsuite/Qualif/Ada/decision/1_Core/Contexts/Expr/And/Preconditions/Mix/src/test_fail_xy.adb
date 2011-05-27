with Support, Points, Silent_Last_Chance; use Support, Points;

--  Call Same_X and Same_Y with regular arguments. Call Same_XY with an
--  unset second point, causing a evalcondition failure and only that for
--  the associated expression.

--  Verify full stmt coverage nevertheless, except for the Same_XY body.

procedure Test_Fail_XY is
   P1, P2 : Point;
begin
   Set (P1, X => 0, Y => 1);

   Assert (Same_X (P1, P1));
   Assert (Same_Y (P1, P1));

   P2.X := 0; -- improper way to set
   P2.Y := 1; -- improper way to set
   Assert (Same_XY (P1, P2));
end;

--# points.adb
--  /bodySX/   l+ 0
--  /bodySY/   l+ 0
--  /bodySet/  l+ 0
--  /preSXY/   l+ 0
--  /retSXY/   l- s-

--# points.ads
--  /preSX/    l+ 0
--  /preSY/    l+ 0
