with Andthen, Support; use Support;

procedure Test_FU_FU is
begin
   Assert (Andthen (False, False) = False);
   Assert (Andthen (False, True)  = False);
   Assert (Andthen (True , False) = False);
   Assert (Andthen (True , True)  = True);
end;

--# andthen.adb
--  /evalA/ l+ ## 0
--  /evalB/ l+ ## 0
--  /true/  l+ ## 0
--  /false/ l+ ## 0
