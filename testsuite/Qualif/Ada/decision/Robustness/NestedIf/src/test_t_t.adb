with Andthen, Support; use Support;

procedure Test_T_T is
begin
   Assert (Andthen (True, True) = True);
end;

--# andthen.adb
--  /evalA/ l! d!
--  /evalB/ l! d!
--  /true/  l+ 0
--  /false/ l- s-
