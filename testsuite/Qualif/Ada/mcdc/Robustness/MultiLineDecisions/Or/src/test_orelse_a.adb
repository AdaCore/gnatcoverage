with Support, Orelse; use Support;

procedure Test_Orelse_A is
begin
   Assert (Orelse (False, False) = False);
   Assert (Orelse (True, False) = True);
end;

--# orelse.adb
-- /eval0/  l! 0
-- /eval1/  l! c!:"B"
-- /true/   l+ 0
-- /false/  l+ 0
