with Values.Global, Support; use Values, Support;

procedure Test_Values_GSA is
begin
   Global.GSA := (1, 2, 3, 4, 5, 6, 7, 8);

   Global.Do_Loop;
   Assert (Global.GSA (3) = 6);
   Assert (Global.GSA (6) = 12);
end;

--# values-global.adb
--  /stmt/    l+ ## 0
--  /loop_op/ l+ ## 0
