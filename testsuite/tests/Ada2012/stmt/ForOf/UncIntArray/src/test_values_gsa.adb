with Values.Global, Support; use Values, Support;

procedure Test_Values_GSA is
begin
   Global.Do_Loop;
   Assert (Global.GSA (3) = 2);
   Assert (Global.GSA (6) = 4);
end;

--# values-global.adb
--  /stmt/    l+ ## 0
--  /loop_op/ l+ ## 0

