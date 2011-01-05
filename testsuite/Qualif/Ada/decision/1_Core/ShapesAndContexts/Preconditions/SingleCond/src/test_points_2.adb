with Support, Points; use Support, Points;

procedure Test_Points_2 is
   P1, P2 : Point;
begin
   Set (P1, X => 0, Y => 1);
   Assert (X (P1) = 0);

   Set (P2, X => 1, Y => 0);
   Assert (Y (P2) = 0);
end;

--# points.adb
--  /bodyX/    l+ 0
--  /bodyY/    l+ 0
--  /bodySet/  l+ 0
--  /preSet/   l! d!

--# points.ads
--  /preX/    l! d!
--  /preY/    l! d!
