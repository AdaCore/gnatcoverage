with Support, Points; use Support, Points;

procedure Test_Points_1 is
   P1 : Point;
begin
   Set (P1, X => 0, Y => 1);
   Assert (X (P1) = 0);
end;

--# points.adb
--  /bodyX/    l+ 0
--  /bodyY/    l- s-
--  /bodySet/  l+ 0
--  /preSet/   l+;mu=>l! mu=>d!

--# points.ads
--  /preX/    l+;mu=>l! mu=>d!
--  /preY/    l- s-
