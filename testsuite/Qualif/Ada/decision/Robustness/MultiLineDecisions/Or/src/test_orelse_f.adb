with Support, Orelse; use Support;

procedure Test_Orelse_F is
begin
   Assert (Orelse (False, False)  = False);
end;

--# orelse.adb
-- /eval0/  l! dT-
-- /eval1/  l! 0
-- /true/   l- s-
-- /false/  l+ 0
