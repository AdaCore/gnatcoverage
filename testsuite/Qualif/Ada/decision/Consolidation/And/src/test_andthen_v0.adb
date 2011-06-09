with Support, Andthen; use Support;

procedure Test_Andthen_V0 is
begin
   for X in False .. True loop
      Assert (Andthen (False, X) = False);
   end loop;
end;

--# andthen.adb
--  /eval/ l! dT-
--  /true/  l- s-
--  /false/ l+ 0


