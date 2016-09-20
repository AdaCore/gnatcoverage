pragma Ada_2012;

with Values.ARG, Support; use Values, Support;

-- test with limit to 0 iteration

procedure Test_Values_ARG_IT0 is   
begin
   ARG.Do_Loop_Over
     (VA, Early_Return => False, Max_Iterations => 0);
   for E of VA loop
      Check_Value (E, 5);
   end loop;
end;

--# values-arg.adb
--  /decl/    l+ ## 0
--  /test-return/ l+ ## 0
--  /return/      l- ## s-
--  /for-stmt/    l+ ## 0
--  /test-exit/   l+ ## 0
--  /exit/        l+ ## 0
--  /loop_op/   l- ## s-
--  /post-loop/ l+ ## 0

