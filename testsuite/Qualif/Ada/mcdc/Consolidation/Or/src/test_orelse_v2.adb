with Support, Orelse; use Support;

procedure Test_Orelse_V2 is
begin
   Assert (Orelse (True, False) = True);
end;

--# orelse.adb
--  /eval/ l! eF-


