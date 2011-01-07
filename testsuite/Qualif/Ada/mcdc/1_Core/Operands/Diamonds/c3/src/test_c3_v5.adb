with Support, C3; use Support;

procedure Test_C3_V5 is
begin
   Assert (C3 (False, True, False) = False);
   Assert (C3 (False, False, False) = False);
end;

--# c3.adb
--  /eval/ l! dT-


