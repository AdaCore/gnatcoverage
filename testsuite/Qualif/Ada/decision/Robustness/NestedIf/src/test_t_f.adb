with Andthen, Support; use Support;

procedure Test_T_F is
begin
   Assert (Andthen (True, False) = False);
end;

--# andthen.adb
--  /evalA/ l! dF-
--  /evalB/ l! dT-
--  /true/  l- s-
--  /false/ l+ 0
