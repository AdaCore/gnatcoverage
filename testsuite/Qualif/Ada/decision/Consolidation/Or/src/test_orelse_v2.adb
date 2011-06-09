with Support, Orelse; use Support;

procedure Test_Orelse_V2 is
begin
   for X in False .. True loop
      Assert (Orelse (True, X) = True);
   end loop;
end;

--# orelse.adb
--  /eval/ l! dF-
--  /true/  l+ 0
--  /false/ l- s-


