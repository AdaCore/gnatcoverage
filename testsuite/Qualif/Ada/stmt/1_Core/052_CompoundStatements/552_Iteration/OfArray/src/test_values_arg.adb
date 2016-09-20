pragma Ada_2012;

with Values.ARG, Support; use Values, Support;

procedure Test_Values_ARG is   
begin
   ARG.Do_Loop_Over
     (VA, Early_Return => False, Max_Iterations => VA'Length);
   for E of VA loop
      Check_Value (E, 10);
   end loop;
end;

--# values-arg.adb
--  /decl/    l+ ## 0
--  /test-return/ l+ ## 0
--  /return/      l- ## s-
--  /for-stmt/    l+ ## 0
--  /test-exit/   l+ ## 0
--  /exit/        l- ## s-
--  /loop_op/   l+ ## 0
--  /post-loop/ l+ ## 0

