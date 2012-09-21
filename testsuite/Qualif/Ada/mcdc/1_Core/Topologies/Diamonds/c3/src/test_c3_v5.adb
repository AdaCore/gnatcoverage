with Support, C3; use Support;

procedure Test_C3_V5 is
begin
   for B in False .. True loop
      Assert (C3 (False, B, False) = False);
   end loop;
end;

--# c3.adb
--  /eval/ l! ## eT-


