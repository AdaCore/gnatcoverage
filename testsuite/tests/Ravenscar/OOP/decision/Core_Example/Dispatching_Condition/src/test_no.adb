--  Test driver for checking DC in case when a decision contains a (dynamically
--  resolved) dispatching call.
--
--  The simplest structure of the decision is used - it contains the only
--  condition that is a dispatching call and no logical operation.
--
--  The coverage of the code of called dispatching operation is NOT checked.
--
--  The driver does not call anything - so nothing from the functional code is
--  expected to be reported as covered.

with Check_Dispatching_Condition;

with Support; use Support;
procedure Test_No is
begin
   Assert (True);
end Test_No;

--# check_dispatching_condition.adb
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
