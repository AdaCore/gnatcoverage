with Support, Andthen; use Support;

procedure Test_Andthen_T is
begin
   Assert (Andthen (True, True) = True);
end;

--# andthen.adb
--  /eval/ l! dF-
--  /true/  l+ 0
--  /false/ l- s-


