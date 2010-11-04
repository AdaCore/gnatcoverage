with Support, Ranges; use Support, Ranges;

procedure Test_Ranges_Inrange is
   In_Range : Boolean;
begin
   Check_Range (X => 3, Min => 2, Max => 4, In_Range => In_Range);
   Assert (In_Range);
end;

--# ranges.adb
--  /checkOmin/ l+ 0
--  /outMin/    l- s-
--  /checkOmax/ l+ 0
--  /outMax/    l- s-
--  /inRange/   l+ 0
