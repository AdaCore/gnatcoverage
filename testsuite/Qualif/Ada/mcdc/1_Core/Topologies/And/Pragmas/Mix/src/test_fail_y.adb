with Support, Points, Silent_Last_Chance; use Support, Points;

--  Call Same_X and Same_XY with regular arguments. Call Same_Y with an
--  unset second point, causing a precondition failure and only that for
--  the associated expression.

procedure Test_Fail_Y is
   P1, P2 : Point;
begin
   Set (P1, X => 0, Y => 1);

   Assert (Same_X (P1, P1));
   Assert (Same_XY (P1, P1));

   P2.Y := 1; -- improper way to set
   Assert (Same_Y (P1, P2));
exception
    when others => null;
end;

--# points.adb
--  /bodySX/   l+ ## 0
--  /bodySY/   l- ## s-
--  /bodySet/  l+ ## 0
--  /preSXY/   l! ## eF-
--  /retSXY/   l! ## eF-

--# points.ads
--  /preSX/    l! ## eF-
--  /preSY/    l! ## eT-
