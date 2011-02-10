with Support, Checks; use Support, Checks;

procedure Test_0 is
begin
   Assert (N_Checks_Ok = 0 and then N_Checks = 0);
end;

--# checks.adb
-- /eval/   l- s-
-- /incOK/  l- s-
-- /incAll/ l- s-
