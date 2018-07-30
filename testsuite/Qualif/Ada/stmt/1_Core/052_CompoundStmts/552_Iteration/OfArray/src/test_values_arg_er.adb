pragma Ada_2012;

with Values.ARG, Support; use Values, Support;

-- test with Early Return

procedure Test_Values_ARG_ER is   
begin
   ARG.Do_Loop_Over
     (VA, Early_Return => True, Max_Iterations => VA'Length);
   for E of VA loop
      Check_Value (E, 5);
   end loop;
end;

--# values-arg.adb
--  /decl/    l+ ## 0
--  /test-return/ l+ ## 0
--  /return/      l+ ## 0
--  /for-stmt/    l- ## s-
--  /test-exit/   l- ## s-
--  /exit/        l- ## s-
--  /loop_op/   l- ## s-
--  /post-loop/ l- ## s-

