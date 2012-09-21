with Support, Orelse; use Support;

procedure Test_Orelse_V1 is
begin
   Assert (Orelse (False, True) = True);
end;

--# orelse.adb
--  /eval/ l! ## dF-
--  /true/  l+ ## 0
--  /false/ l- ## s-

