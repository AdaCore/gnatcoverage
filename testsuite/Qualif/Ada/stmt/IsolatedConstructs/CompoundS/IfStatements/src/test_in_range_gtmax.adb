with In_Range, Support; use Support;

--  Exercize X > max only. Verify that the < min exit and the in-range case
--  are reported uncovered.

procedure Test_In_Range_GTmax is
begin
   Assert (not In_Range (4, 2, 3));
end;

--# in_range.adb
--  /XcmpMin/  l+ 0
--  /XcmpMax/  l+ 0
--  /XoutMax/  l+ 0
--  /Xin/      l- s-
--  /XoutMin/  l- s-
