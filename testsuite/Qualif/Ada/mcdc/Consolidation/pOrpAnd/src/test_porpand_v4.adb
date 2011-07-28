with Support, Porpand; use Support;

procedure Test_Porpand_V4 is
begin
   for B in False .. True loop
      Assert (Porpand (True, B, False) = False);
   end loop;
end;

--# porpand.adb
-- /eval/ l! eT-

