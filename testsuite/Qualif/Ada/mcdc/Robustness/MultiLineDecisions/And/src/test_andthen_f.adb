with Support, Andthen; use Support;

procedure Test_Andthen_F is
begin
   Assert (Andthen (False, True)  = False);
   Assert (Andthen (True , False) = False);
   Assert (Andthen (False, False) = False);
end;

--# andthen.adb
-- /eval0/  l! oT-
-- /eval1/  l! 0
-- /true/   l- s-
-- /false/  l+ 0
