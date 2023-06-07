--  Test driver for checking DC in case when a decision contains a (dynamically
--  resolved) dispatching call.
--
--  The simplest structure of the decision is used - it contains the only
--  condition that is a dispatching call and no logical operation.
--
--  The coverage of the code of called dispatching operation is NOT checked.
--
--  The driver calls a dispatching operation in such a way that a decision to
--  check is evaluated to False.

with Check_Dispatching_Condition;

with Support; use Support;
procedure Test_F is
   Result : Integer;
begin
   Check_Dispatching_Condition
     (Check_Var => 2,
      Result    => Result);

   Assert (Result = 20);
end Test_F;

--# check_dispatching_condition.adb
-- /dcl/         l+ ## 0
-- /case/        l+ ## 0
-- /var_1/       l- ## s-
-- /var_2/       l+ ## 0
-- /var_3/       l- ## s-
-- /var_others/  l- ## s-
-- /eval/        l! ## dT-
-- /true/        l- ## s-
-- /false/       l+ ## 0
-- /exc/         l- ## s-
