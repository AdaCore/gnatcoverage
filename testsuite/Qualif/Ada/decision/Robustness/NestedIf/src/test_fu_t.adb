with Andthen, Support; use Support;

procedure Test_FU_T is
begin
   Assert (Andthen (False, True) = False);
   Assert (Andthen (True, True) = True);
end;

--# andthen.adb
--  /evalA/ l+ 0
--  /evalB/ l! dF-
--  /true/  l+ 0
--  /false/ l+ 0
