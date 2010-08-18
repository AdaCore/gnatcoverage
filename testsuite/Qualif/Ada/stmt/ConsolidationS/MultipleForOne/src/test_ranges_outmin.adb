with Support, Ranges; use Support, Ranges;

procedure Test_Ranges_Outmin is
   In_Range : Boolean;
begin
   Check_Range (X => 1, Min => 2, Max => 4, In_Range => In_Range);
   Assert (not In_Range);
end;

--# ranges.adb
--  /checkOmin/ l+ 0
--  /outMin/    l+ 0
--  /checkOmax/ l- s-
--  /outMax/    l- s-
--  /inRange/   l- s-
