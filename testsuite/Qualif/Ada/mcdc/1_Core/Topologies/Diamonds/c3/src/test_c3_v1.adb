with Support, C3; use Support;

procedure Test_C3_V1 is
begin
   for C in False .. True loop
      Assert (C3 (True, True, C) = True);
   end loop;
end;

--# c3.adb
--  /eval/ l! ## eF-


