with Support, Orelse; use Support;

procedure Test_Orelse_T is
begin
   Assert (Orelse (False, True) = True);
   Assert (Orelse (True, False) = True);
   Assert (Orelse (True, True) = True);
end;

--# orelse.adb
-- /eval0/  l! ## oF-
-- /eval1/  l! ## 0
-- /true/   l+ ## 0
-- /false/  l- ## s-

