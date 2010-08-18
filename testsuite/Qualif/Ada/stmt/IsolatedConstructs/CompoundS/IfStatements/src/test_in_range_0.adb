with In_Range, Support; use Support;

--  Exercize nothing. Verify that everything is reported uncovered.

procedure Test_In_Range_0 is
begin
   Assert (True);
end;

--# in_range.adb
--  /XcmpMin/  l- s-
--  /XoutMin/  l- s-
--  /XcmpMax/  l- s-
--  /XoutMax/  l- s-
--  /Xin/      l- s-
