with Values.Arg, Support; use Values, Support;

procedure Test_Values_ARG_EMPTY is
begin
   Arg.Do_Loop_Over (EA, Early_Return => False, Max_Iterations => EA'Length);
end;
 
--# values-arg.adb
--  /decl/    l+ ## 0
--  /test-return/ l+ ## 0
--  /return/      l- ## s-
--  /for-stmt/    l+ ## 0
--  /test-exit/   l- ## s-
--  /exit/        l- ## s-
--  /loop_op/   l- ## s-
--  /post-loop/ l+ ## 0


