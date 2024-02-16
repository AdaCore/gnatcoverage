with Values.Global, Support; use Values, Support;

procedure Test_Values_GSA is
begin
   Global.GSA := (others => (X => 4));

   Global.Do_Loop;
   Assert (Global.GSA (1).X = 5);
   Assert (Global.GSA (3).X = 5);
end;

--# values-global.adb
--  /stmt/    l+ ## 0
--  /loop_op/ l+ ## 0
