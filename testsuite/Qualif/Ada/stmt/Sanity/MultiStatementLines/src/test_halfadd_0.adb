with Support, Halfadd; use Support;

--  Call nothing. Verify that all the lines with statements are reported
--  uncovered.

procedure Test_Halfadd_0 is
begin
   Assert (True);
end;

--# halfadd.adb
--  /sum/   l- s-
--  /carry/ l- s-:"if A", s-:"Carry .= True", s-:"Carry .= False"
