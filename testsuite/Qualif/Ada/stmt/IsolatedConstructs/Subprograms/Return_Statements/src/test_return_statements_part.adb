--  Test driver for return statements. It executes a part of the functional
--  code, so some return statements are expected to be reported as covered and
--  some are not.

with Return_Statements; use Return_Statements;
with Support;           use Support;
procedure Test_Return_Statements_Part is
   Int : Integer := 0;
begin
   Proc1 (1, Int);
   Assert (Int = 2);

   Proc2 (1, Int);
   Assert (Int = 2);

   Int := Fun1 (1);
   Assert (Int = 1);

   Int := Fun2 (0);
   Assert (Int = 0);

end Test_Return_Statements_Part;

--# return_statements.adb
-- /proc1_1/            l+ 0
-- /proc1_return/       l- s-
-- /proc1_after_return/ l+ 0

-- /proc2_start/        l+ 0
-- /proc2_1/            l+ 0
-- /proc2_2/            l- s-
-- /proc2_3/            l- s-
-- /proc2_others/       l- s-
-- /proc2_fin/          l- s-

-- /fun1_start/         l+ 0
-- /fun1_first_return/  l+ 0
-- /fun_1_fin/          l- s-

-- /fun2_start/         l+ 0
-- /fun2_1_return/      l- s-
-- /fun2_1_elsif/       l+ 0
-- /fun2_2_return/      l- s-
-- /fun2_2_elsif/       l+ 0
-- /fun2_case/          l- s-
-- /fun2_3_return/      l- s-
-- /fun2_4_return/      l- s-
-- /fun2_others/        l- s-
-- /fun_2_fin/          l+ 0
