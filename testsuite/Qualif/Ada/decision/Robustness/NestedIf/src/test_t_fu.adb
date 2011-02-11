with Andthen, Support; use Support;

procedure Test_T_FU is
begin
   Assert (Andthen (True, False) = False);
   Assert (Andthen (True, True)  = True);
end;

--# andthen.adb
--  /evalA/ l! d!
--  /evalB/ l+ 0
--  /true/  l+ 0
--  /false/ l+ 0
