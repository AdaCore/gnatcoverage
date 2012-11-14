--  Test driver for exception handlers. It calls all the subprograms from the
--  functional code in such a way that a user-defined exception is raised in
--  each called subprogram. So only the code from handlers for this exception
--  or the code from OTHERS handler in case if there is no nadler for this
--  exception is expected to be reported as covered, and the code from all the
--  other handlers is expected to be reported as uncovered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Handlers_User_Exception_Raise is
   Int : My_Int := 100;
begin
   Proc1 (Int);
   Assert (Int = 0);

   Assert (Fun1 (100) = 0);

   Int := 100;
   Proc2 (Int);
   Assert (Int = 1);

   Assert (Fun2 (-100) = 1);

   Int := -1;
   Proc3 (Int);
   Assert (Int = 1);

   Assert (Fun3 (-1) = 1);

   Int := -1;
   Proc4 (Int);
   Assert (Int = -1);

   Assert (Fun4 (-1) = -1);

end Test_Handlers_User_Exception_Raise;

--#  pack.adb
-- /proc1/                          l+ ## 0
-- /raise_user_defined_proc1/       l+ ## 0
-- /raise_proc1/                    l+ ## 0
-- /no_raise_proc1/                 l- ## s-

-- /fun1/                           l+ ## 0
-- /raise_user_defined_fun1/        l+ ## 0
-- /no_raise_user_defined_fun1/     l- ## s-
-- /raise_fun1/                     l+ ## 0
-- /no_raise_fun1/                  l- ## s-

-- /proc2/                          l+ ## 0
-- /raise_user_defined_proc2/       l+ ## 0
-- /no_user_defined_raise_proc2/    l- ## s-
-- /in_if_proc2/                    l- ## s-
-- /no_predefined_raise_proc2/      l- ## s-
-- /raise_predefined_proc2/         l- ## s-

-- /fun2/                           l+ ## 0
-- /raise_user_defined_fun2/        l+ ## 0
-- /no_user_defined_raise_fun2/     l- ## s-
-- /no_predefined_raise_fun2/       l- ## s-
-- /raise_predefined_fun2/          l- ## s-
-- /no_raise_fun2/                  l- ## s-

-- /proc3/                          l+ ## 0
-- /no_predefined_raise_proc3/      l+ ## 0
-- /raise_user_defined_proc3/       l+ ## 0
-- /no_raise_proc3/                 l- ## s-
-- /raise_predefined_proc3/         l- ## s-

-- /fun3/                           l+ ## 0
-- /no_predefined_raise_fun3/       l+ ## 0
-- /raise_user_defined_fun3/        l+ ## 0
-- /no_raise_fun3/                  l- ## s-
-- /raise_predefined_fun3/          l- ## s-

-- /proc4/                          l+ ## 0
-- /no_predefined_raise_proc4/      l+ ## 0
-- /no_raise_proc4/                 l- ## s-
-- /raise_user_defined_proc4/       l+ ## 0
-- /raise_predefined_proc4/         l- ## s-

-- /fun4/                           l+ ## 0
-- /no_predefined_raise_fun4/       l+ ## 0
-- /raise_user_defined_fun4/        l+ ## 0
-- /no_raise_fun4/                  l- ## s-
-- /raise_predefined_fun4/          l- ## s-
