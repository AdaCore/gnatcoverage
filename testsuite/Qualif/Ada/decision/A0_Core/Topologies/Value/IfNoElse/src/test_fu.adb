with Support, Checks; use Support, Checks;

procedure Test_FU is
begin
   for Cond in False .. True loop
      Check (Cond => Cond);
   end loop;

   Assert (N_Checks_Ok = 1 and then N_Checks = 2);
end;

--# checks.adb
-- /eval/   l+ ## 0
-- /incOK/  l+ ## 0
-- /incAll/ l+ ## 0
