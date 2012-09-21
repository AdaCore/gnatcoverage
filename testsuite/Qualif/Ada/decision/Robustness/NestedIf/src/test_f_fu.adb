with Andthen, Support; use Support;

procedure Test_F_FU is
begin
   Assert (Andthen (False, False) = False);
   Assert (Andthen (False, True)  = False);
end;

--# andthen.adb
--  /evalA/ l! ## dT-
--  /evalB/ l- ## s-
--  /true/  l- ## s-
--  /false/ l+ ## 0
