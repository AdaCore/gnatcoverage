pragma Ada_2012;

with Values.ARG, Support; use Values, Support;

-- test with limit to 1 iteration

procedure Test_Values_ARG_IT1 is   
begin
   ARG.Do_Loop_Over
     (VA, Early_Return => False, Max_Iterations => 1);
   Check_Value (VA (VA'First), 10);
   for I in VA'First + 1 .. VA'Last loop
      Check_Value (VA (I), 5);
   end loop;
end;

--# values-arg.adb
--  /decl/    l+ ## 0
--  /test-return/ l+ ## 0
--  /return/      l- ## s-
--  /for-stmt/    l+ ## 0
--  /test-exit/   l+ ## 0
--  /exit/        l+ ## 0
--  /loop_op/   l+ ## 0
--  /post-loop/ l+ ## 0

