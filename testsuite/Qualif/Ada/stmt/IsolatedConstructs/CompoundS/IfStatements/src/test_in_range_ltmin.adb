with In_Range, Support; use Support;

--  Exercize X < min only. Verify that code for the > max and in-range cases
--  is reported uncovered.

procedure Test_In_Range_LTmin is
begin
   Assert (not In_Range (1, 2, 3));
end;

--# in_range.adb
--  /XcmpMin/  l+ 0
--  /XoutMin/  l+ 0
--  /XcmpMax/  l- s-
--  /XoutMax/  l- s-
--  /Xin/      l- s-
