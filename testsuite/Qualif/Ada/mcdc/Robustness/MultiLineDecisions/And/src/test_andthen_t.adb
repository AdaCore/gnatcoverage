with Support, Andthen; use Support;

procedure Test_Andthen_T is
begin
   Assert (Andthen (True, True) = True);
end;

--# andthen.adb
-- /eval0/  l! dF-
-- /eval1/  l! 0
-- /true/   l+ 0
-- /false/  l- s-

