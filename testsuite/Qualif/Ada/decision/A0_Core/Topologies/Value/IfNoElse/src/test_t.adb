with Support, Checks; use Support, Checks;

procedure Test_T is
begin
   Check (Cond => True);
   Assert (N_Checks_Ok = 1 and then N_Checks = 1);
end;

--# checks.adb
-- /eval/   l! ## oF-
-- /incOK/  l+ ## 0
-- /incAll/ l+ ## 0
