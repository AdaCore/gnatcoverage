--  Test driver for checking DC in case when a decision contains call to a
--  class-wide-operation.
--
--  The simplest structure of the decision is used - it contains the only
--  condition that is the call to the class-wide operation and no logical
--  operation.
--
--  The coverage of the code of called dispatching operation is NOT checked.
--
--  The driver calls a class-wide operation three times to get all possible
--  results - False, True and raising an exception.

with Check_Class_Wide_Condition;

with Support; use Support;
procedure Test_FTX is
   Result : Integer;
begin
   Check_Class_Wide_Condition
     (Check_Var => 2,
      Result    => Result);
   Assert (Result = 20);

   Check_Class_Wide_Condition
     (Check_Var => 1,
      Result    => Result);
   Assert (Result = 10);

   Check_Class_Wide_Condition
     (Check_Var => 3,
      Result    => Result);

   Assert (Result = 0);
end Test_FTX;

--# check_class_wide_condition.adb
-- /dcl/         l+ ## 0
-- /case/        l+ ## 0
-- /var_1/       l+ ## 0
-- /var_2/       l+ ## 0
-- /var_3/       l+ ## 0
-- /var_others/  l- ## s-
-- /eval/        l+ ## 0
-- /true/        l+ ## 0
-- /false/       l+ ## 0
-- /exc/         l+ ## 0
