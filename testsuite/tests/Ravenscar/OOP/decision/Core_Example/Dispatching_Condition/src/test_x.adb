--  Test driver for checking DC in case when a decision contains a (dynamically
--  resolved) dispatching call.
--
--  The simplest structure of the decision is used - it contains the only
--  condition that is a dispatching call and no logical operation.
--
--  The coverage of the code of called dispatching operation is NOT checked.
--
--  The driver calls a dispatching operation in such a way that evaluation if
--  the decision raises an exception.

with Check_Dispatching_Condition;

with Support; use Support;
procedure Test_X is
   Result : Integer;
begin
   Check_Dispatching_Condition
     (Check_Var => 3,
      Result    => Result);

   Assert (Result = 0);
end Test_X;

--# check_dispatching_condition.adb
-- /dcl/         l+ ## 0
-- /case/        l+ ## 0
-- /var_1/       l- ## s-
-- /var_2/       l- ## s-
-- /var_3/       l+ ## 0
-- /var_others/  l- ## s-
-- /eval/        l! ## d-
-- /true/        l- ## s-
-- /false/       l- ## s-
-- /exc/         l+ ## 0
