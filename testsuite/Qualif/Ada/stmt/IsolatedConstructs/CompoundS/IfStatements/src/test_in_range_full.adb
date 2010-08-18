with In_Range, Support; use Support;

--  Exercize X < min, > max and in-range. Verify that nothing is reported
--  uncovered.

procedure Test_In_Range_Full is
begin
   Assert (In_Range (2, 1, 3));
   Assert (not In_Range (0, 1, 3));
   Assert (not In_Range (5, 1, 3));
end;

--# in_range.adb
--  /XcmpMin/  l+ 0
--  /XoutMin/  l+ 0
--  /XcmpMax/  l+ 0
--  /XoutMax/  l+ 0
--  /Xin/      l+ 0
