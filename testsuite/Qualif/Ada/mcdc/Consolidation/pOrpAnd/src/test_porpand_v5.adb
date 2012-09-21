with Support, Porpand; use Support;

procedure Test_Porpand_V5 is
begin
   for B in False .. True loop
      Assert (Porpand (True, B, True) = True);
   end loop;
end;

--# porpand.adb
-- /eval/ l! ## eF-


