--  Test driver for exception handlers. It calls all the subprograms from the
--  functional code, but does it in such a way that no exception is raised, so
--  no code from exception handlers is expected to be reported as uncovered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Handlers_No_Exception_Raise is
   Int : My_Int := 0;
begin
   Proc1 (Int);
   Assert (Int = 10);

   Assert (Fun1 (0) = 10);

   Int := 1;
   Proc2 (Int);
   Assert (Int = 1);

   Assert (Fun2 (0) = 0);

   Proc3 (Int);
   Assert (Int = 11);

   Assert (Fun3 (0) = 11);

   Int := 1;
   Proc4 (Int);
   Assert (Int = 11);

   Assert (Fun4 (0) = 11);

end Test_Handlers_No_Exception_Raise;

--#  pack.adb
-- /proc1/                          l+ ## 0
-- /raise_user_defined_proc1/       l- ## s-
-- /raise_proc1/                    l- ## s-
-- /no_raise_proc1/                 l+ ## 0

-- /fun1/                           l+ ## 0
-- /raise_user_defined_fun1/        l- ## s-
-- /no_raise_user_defined_fun1/     l+ ## 0
-- /raise_fun1/                     l- ## s-
-- /no_raise_fun1/                  l+ ## 0

-- /proc2/                          l+ ## 0
-- /raise_user_defined_proc2/       l- ## s-
-- /no_user_defined_raise_proc2/    l+ ## 0
-- /in_if_proc2/                    l- ## s-
-- /no_predefined_raise_proc2/      l+ ## 0
-- /raise_predefined_proc2/         l- ## s-

-- /fun2/                           l+ ## 0
-- /raise_user_defined_fun2/        l- ## s-
-- /no_user_defined_raise_fun2/     l+ ## 0
-- /no_predefined_raise_fun2/       l+ ## 0
-- /raise_predefined_fun2/          l- ## s-
-- /no_raise_fun2/                  l+ ## 0

-- /proc3/                          l+ ## 0
-- /no_predefined_raise_proc3/      l+ ## 0
-- /raise_user_defined_proc3/       l- ## s-
-- /no_raise_proc3/                 l+ ## 0
-- /raise_predefined_proc3/         l- ## s-

-- /fun3/                           l+ ## 0
-- /no_predefined_raise_fun3/       l+ ## 0
-- /raise_user_defined_fun3/        l- ## s-
-- /no_raise_fun3/                  l+ ## 0
-- /raise_predefined_fun3/          l- ## s-

-- /proc4/                          l+ ## 0
-- /no_predefined_raise_proc4/      l+ ## 0
-- /no_raise_proc4/                 l+ ## 0
-- /raise_user_defined_proc4/       l- ## s-
-- /raise_predefined_proc4/         l- ## s-

-- /fun4/                           l+ ## 0
-- /no_predefined_raise_fun4/       l+ ## 0
-- /raise_user_defined_fun4/        l- ## s-
-- /no_raise_fun4/                  l+ ## 0
-- /raise_user_defined_fun4/        l- ## s-
-- /raise_predefined_fun4/          l- ## s-
