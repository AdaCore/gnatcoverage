with Support, Andthen; use Support;

procedure Test_Andthen_F is
begin
   Assert (Andthen (False, True)  = False);
   Assert (Andthen (True , False) = False);
   Assert (Andthen (False, False) = False);
end;

--# andthen.adb
-- /eval0/  l! ## dT-
-- /eval1/  l! ## 0c
-- /true/   l- ## s-
-- /false/  l+ ## 0
