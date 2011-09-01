with Support, Orelse; use Support;

procedure Test_Orelse_T is
begin
   Assert (Orelse (True, True) = True);
   Assert (Orelse (False, True) = True);
   Assert (Orelse (True, False) = True);
end;

--# orelse.adb
-- /eval0/  l! dF-
-- /eval1/  l! 0c
-- /true/   l+ 0
-- /false/  l- s-
