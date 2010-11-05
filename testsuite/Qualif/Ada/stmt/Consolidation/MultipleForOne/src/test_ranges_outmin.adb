with Support, Ranges; use Support, Ranges;

--  Exercize the X < Min case. Verify that statements specific to X determined
--  in range and to X determined > Max are reported uncovered, as well as the
--  > Max test because of an early exit .

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
