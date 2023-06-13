--  Test driver for checking DC in case when a decision contains call to a
--  class-wide-operation.
--
--  The simplest structure of the decision is used - it contains the only
--  condition that is the call to the class-wide operation and no logical
--  operation.
--
--  The coverage of the code of called dispatching operation is NOT checked.
--
--  The driver calls a class-wide operation in such a way that evaluation if
--  the decision raises an exception.

with Check_Class_Wide_Condition;

with Support; use Support;
procedure Test_X is
   Result : Integer;
begin
   Check_Class_Wide_Condition
     (Check_Var => 3,
      Result    => Result);

   Assert (Result = 0);
end Test_X;

--# check_class_wide_condition.adb
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
