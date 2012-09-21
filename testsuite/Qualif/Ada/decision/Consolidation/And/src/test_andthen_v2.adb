with Support, Andthen; use Support;

procedure Test_Andthen_V2 is
begin
   Assert (Andthen (True, True) = True);
end;

--# andthen.adb
--  /eval/ l! ## dF-
--  /true/  l+ ## 0
--  /false/ l- ## s-


