with Support, C3; use Support;

procedure Test_C3_V1 is
begin
   Assert (C3 (True, True, False) = True);
   Assert (C3 (True, True, True) = True);
end;

--# c3.adb
--  /eval/ l! dF-


