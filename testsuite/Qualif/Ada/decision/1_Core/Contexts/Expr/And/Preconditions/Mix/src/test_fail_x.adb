with Support, Points, Silent_Last_Chance; use Support, Points;

--  Call Same_Y and Same_XY with regular arguments. Call Same_X with an
--  unset second point, causing a evalcondition failure and only that for
--  the associated expression.

--  Verify full stmt coverage nevertheless, except for the Same_X body.

procedure Test_Fail_X is
   P1, P2 : Point;
begin
   Set (P1, X => 0, Y => 1);

   Assert (Same_Y (P1, P1));
   Assert (Same_XY (P1, P1));

   P2.X := 0; -- improper way to set
   Assert (Same_X (P1, P2));
end;

--# points.adb
--  /bodySX/   l- s-
--  /bodySY/   l+ 0
--  /bodySet/  l+ 0
--  /preSXY/   l+ 0
--  /retSXY/   l+ 0

--# points.ads
--  /preSX/    l+ 0
--  /preSY/    l+ 0
