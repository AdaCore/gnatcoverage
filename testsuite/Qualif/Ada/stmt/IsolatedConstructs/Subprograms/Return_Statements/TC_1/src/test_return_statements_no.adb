--  Test driver for return statements. It only "with's" the functional code,
--  but does not execute anything from it, so everything is expected to be
--  reported as uncovered.

with Return_Statements; use Return_Statements;
with Support;           use Support;
procedure Test_Return_Statements_No is
begin
   Assert (True);
end Test_Return_Statements_No;

--# return_statements.adb
-- /proc1_1/            l- s-
-- /proc1_return/       l- s-
-- /proc1_after_return/ l- s-
-- /proc2_start/        l- s-
-- /proc2_1/            l- s-
-- /proc2_2/            l- s-
-- /proc2_3/            l- s-
-- /proc2_others/       l- s-
-- /proc2_fin/          l- s-
-- /fun1_start/         l- s-
-- /fun1_first_return/  l- s-
-- /fun_1_fin/          l- s-
-- /fun2_start/         l- s-
-- /fun2_1_return/      l- s-
-- /fun2_1_elsif/       l- s-
-- /fun2_2_return/      l- s-
-- /fun2_2_elsif/       l- s-
-- /fun2_case/          l- s-
-- /fun2_3_return/      l- s-
-- /fun2_4_return/      l- s-
-- /fun2_others/        l- s-
-- /fun_2_fin/          l- s-
