with Andthen, Support; use Support;

procedure Test_FU_F is
begin
   Assert (Andthen (False, False) = False);
   Assert (Andthen (True, False) = False);
end;

--# andthen.adb
--  /evalA/ l+ ## 0
--  /evalB/ l! ## dT-
--  /true/  l- ## s-
--  /false/ l+ ## 0
