with Values.Global, Support; use Values, Support;

procedure Test_Values_GSA is
begin
   Global.Do_Loop;
   Check_Value (VA (2), 10);
   ChecK_Value (VA (4), 10);
end;

--# values-global.adb
--  /stmt/    l+ ## 0
--  /loop_op/ l+ ## 0

