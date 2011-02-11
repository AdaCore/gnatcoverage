with Andthen, Support; use Support;

procedure Test_F_T is
begin
   Assert (Andthen (False, True) = False);
end;

--# andthen.adb
--  /evalA/ l! d!
--  /evalB/ l- s-
--  /true/  l- s-
--  /false/ l+ 0
