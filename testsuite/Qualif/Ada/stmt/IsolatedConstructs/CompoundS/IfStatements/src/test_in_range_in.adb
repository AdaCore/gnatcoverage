with In_Range, Support; use Support;

--  Exercize X in-range. Verify that the < min and > max exits are reported
--  uncovered.

procedure Test_In_Range_In is
begin
   Assert (In_Range (2, 1, 3));
end;

--# in_range.adb
--  /XcmpMin/  l+ 0
--  /XcmpMax/  l+ 0
--  /Xin/      l+ 0
--  /XoutMin/  l- s-
--  /XoutMax/  l- s-
