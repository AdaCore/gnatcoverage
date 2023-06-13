--  Test driver for checking DC in case when a decision contains call to a
--  class-wide-operation.
--
--  The simplest structure of the decision is used - it contains the only
--  condition that is the call to the class-wide operation and no logical
--  operation.
--
--  The coverage of the code of called dispatching operation is NOT checked.
--
--  The driver does not call anything - so nothing from the functional code is
--  expected to be reported as covered.

with Check_Class_Wide_Condition;

with Support; use Support;
procedure Test_No is
begin
   Assert (True);
end Test_No;

--# check_class_wide_condition.adb
-- /dcl/         l- ## s-
-- /case/        l- ## s-
-- /var_1/       l- ## s-
-- /var_2/       l- ## s-
-- /var_3/       l- ## s-
-- /var_others/  l- ## s-
-- /eval/        l- ## s-
-- /true/        l- ## s-
-- /false/       l- ## s-
-- /exc/         l- ## s-
