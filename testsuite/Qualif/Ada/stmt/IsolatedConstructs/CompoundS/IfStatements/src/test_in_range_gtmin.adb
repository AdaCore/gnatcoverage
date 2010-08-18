with In_Range, Support; use Support;

--  Exercize X in-range and > max. Verify that the < min exit is reported
--  uncovered.

procedure Test_In_Range_GTmin is
begin
   Assert (In_Range (3, 2, 4));
   Assert (not In_Range (5, 2, 4));
end;

--# in_range.adb
--  /XcmpMin/  l+ 0
--  /XcmpMax/  l+ 0
--  /Xin/      l+ 0
--  /XoutMax/  l+ 0
--  /XoutMin/  l- s-
