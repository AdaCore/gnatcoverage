with Support, C3; use Support;

procedure Test_C3_V4 is
begin
   Assert (C3 (False, True, True) = True);
   Assert (C3 (False, False, True) = True);
end;

--# c3.adb
--  /eval/ l! dF-


