with Support, Ranges; use Support, Ranges;

procedure Test_Ranges_Outmax is
   In_Range : Boolean;
begin
   Check_Range (X => 5, Min => 2, Max => 4, In_Range => In_Range);
   Assert (not In_Range);
end;

--# ranges.adb
--  /checkOmin/ l+ 0
--  /outMin/    l- s-
--  /checkOmax/ l+ 0
--  /outMax/    l+ 0
--  /inRange/   l- s-
