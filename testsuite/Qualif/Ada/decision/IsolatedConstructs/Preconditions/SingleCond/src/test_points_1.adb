with Support, Points; use Support, Points;

procedure Test_Points_1 is
   P : Point;
begin
   Set (P, X => 0, Y => 1);
   Assert (X (P) = 0);
end;

--# points.adb
--  /bodyX/   l+ 0
--  /bodyY/   l- s-
--  /bodySet/ l+ 0
--  /preSet/  l! d!

--# points.ads
--  /preX/    l! d!
--  /preY/    l- s-
