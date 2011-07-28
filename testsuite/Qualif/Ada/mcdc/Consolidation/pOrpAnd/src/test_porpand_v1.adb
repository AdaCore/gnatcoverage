with Support, Porpand; use Support;

procedure Test_Porpand_V1 is
begin
   for C in False .. True loop
      Assert (Porpand (False, False, C) = False);
   end loop;
end;

--# porpand.adb
-- /eval/ l! eT-


