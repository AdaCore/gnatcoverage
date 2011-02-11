with Andthen, Support; use Support;

procedure Test_T_F is
begin
   Assert (Andthen (True, False) = False);
end;

--# andthen.adb
--  /evalA/ l! d!
--  /evalB/ l! d!
--  /true/  l- s-
--  /false/ l+ 0
