with Support, Andthen; use Support;

procedure Test_Andthen_V1 is
begin
   Assert (Andthen (False, True) = False);
end;

--# andthen.adb
--  /eval/ l! ## eT-


