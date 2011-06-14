with Support, Orelse; use Support;

procedure Test_Orelse_AB is
begin
   Assert (Orelse (False, False) = False);
   Assert (Orelse (True, False) = True);
   Assert (Orelse (False, True) = True);
end;

--# orelse.adb
-- /eval0/  l+ 0
-- /eval1/  l+ 0
-- /true/   l+ 0
-- /false/  l+ 0
