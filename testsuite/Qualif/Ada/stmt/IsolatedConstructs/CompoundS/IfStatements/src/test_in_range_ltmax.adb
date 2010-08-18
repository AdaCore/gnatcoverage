with In_Range, Support; use Support;

--  Exercize X < min and in-range. Verify that the > max exit is reported
--  uncovered.

procedure Test_In_Range_LTmax is
begin
   Assert (not In_Range (1, 2, 4));
   Assert (In_Range (3, 2, 4));
end;

--# in_range.adb
--  /XcmpMin/  l+ 0
--  /XcmpMax/  l+ 0
--  /Xin/      l+ 0
--  /XoutMin/  l+ 0
--  /XoutMax/  l- s-
