with Support, Orelse; use Support;

procedure Test_Orelse_TF is
begin
   Assert (Orelse (True, True) = True);
   Assert (Orelse (False, False) = False);
end;

--# orelse.adb
-- /eval0/  l+ 0
-- /eval1/  l+ 0c
-- /true/   l+ 0
-- /false/  l+ 0
