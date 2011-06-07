with Support, C3; use Support;

procedure Test_C3_V4 is
begin
   for B in False .. True loop
      Assert (C3 (False, B, True) = True);
   end loop;
end;

--# c3.adb
--  /eval/ l! dF-


