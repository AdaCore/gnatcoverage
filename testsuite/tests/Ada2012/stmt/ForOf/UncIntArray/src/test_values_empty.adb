with Values.Arg, Support; use Values, Support;

procedure Test_Values_EMPTY is
   LA : Array_Type (1 .. 0);
begin
   Arg.Do_Loop_Over (LA);
end;

--# values-arg.adb
--  /stmt/    l+ ## 0
--  /loop_op/ l- ## s-
