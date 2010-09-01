--  Test driver for return statements. It executes all the functional code, so
--  everything in the functional code is expected to be reported as covered.

with Return_Statements; use Return_Statements;
with Support;           use Support;
procedure Test_Return_Statements_Full is
   Int : Integer := 0;
begin
   Proc1 (1, Int);
   Assert (Int = 2);

   Proc1 (0, Int);
   Assert (Int = 0);

   Proc2 (1, Int);
   Assert (Int = 2);

   Proc2 (2, Int);
   Assert (Int = 4);

   Proc2 (3, Int);
   Assert (Int =7 );

   Proc2 (4, Int);
   Assert (Int = 0);

   Int := Fun1 (1);
   Assert (Int = 1);

   Int := Fun1 (0);
   Assert (Int = 10);

   Int := Fun2 (0);
   Assert (Int = 0);

   Int := Fun2 (101);
   Assert (Int = 103);

   Int := Fun2 (11);
   Assert (Int = 9);

   Int := Fun2 (-1);
   Assert (Int = 1);

   Int := Fun2 (-2);
   Assert (Int = 3);

   Int := Fun2 (-3);
   Assert (Int = -6);

end Test_Return_Statements_Full;

--# return_statements.adb
-- /proc1_1/            l+ 0
-- /proc1_return/       l+ 0
-- /proc1_after_return/ l+ 0

-- /proc2_start/        l+ 0
-- /proc2_1/            l+ 0
-- /proc2_2/            l+ 0
-- /proc2_3/            l+ 0
-- /proc2_others/       l+ 0
-- /proc2_fin/          l+ 0

-- /fun1_start/         l+ 0
-- /fun1_first_return/  l+ 0
-- /fun_1_fin/          l+ 0

-- /fun2_start/         l+ 0
-- /fun2_1_return/      l+ 0
-- /fun2_1_elsif/       l+ 0
-- /fun2_2_return/      l+ 0
-- /fun2_2_elsif/       l+ 0
-- /fun2_case/          l+ 0
-- /fun2_3_return/      l+ 0
-- /fun2_4_return/      l+ 0
-- /fun2_others/        l+ 0
-- /fun_2_fin/          l+ 0
