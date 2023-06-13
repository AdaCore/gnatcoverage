with Values.Global, Support; use Values, Support;

procedure Test_Values_GSA is
begin
   Global.GSA := (1 .. 2 => 1, 3 .. 4 => 2);
   Global.Do_Loop;
   Assert (Global.GSA (2) = 2);
   Assert (Global.GSA (4) = 4);
end;

--# values-global.adb
--  /stmt/    l+ ## 0
--  /loop_op/ l+ ## 0

