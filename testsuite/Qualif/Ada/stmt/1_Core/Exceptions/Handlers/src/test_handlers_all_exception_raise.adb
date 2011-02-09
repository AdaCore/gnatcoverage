--  Test driver for exception handlers. It calls each the subprogram from the
--  functional code twice: one call raises a predefined exception, and another
--  raises a  user-defined exception. So no code from the exception handlers
--  is expected to be reported as covered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Handlers_All_Exception_Raise is
   Int : My_Int := 99;
begin
   Proc1 (Int);
   Assert (Int = 0);

   Int := 100;
   Proc1 (Int);
   Assert (Int = 0);

   Assert (Fun1 (99) = 0);
   Assert (Fun1 (100) = 0);

   Int := -99;
   Proc2 (Int);
   Assert (Int = -1);

   Int := 100;
   Proc2 (Int);
   Assert (Int = 1);

   Assert (Fun2 (50) = -1);
   Assert (Fun2 (-100) = 1);

   Int := 100;
   Proc3 (Int);
   Assert (Int = -1);

   Int := -1;
   Proc3 (Int);
   Assert (Int = 1);

   Assert (Fun3 (100) = -1);
   Assert (Fun3 (-1) = 1);

   Int := 100;
   Proc4 (Int);
   Assert (Int = 1);

   Int := -1;
   Proc4 (Int);
   Assert (Int = -1);

   Assert (Fun4 (100) = 1);
   Assert (Fun4 (-1) = -1);

end Test_Handlers_All_Exception_Raise;

--#  pack.adb
-- /proc1/                          l+ 0
-- /raise_user_defined_proc1/       l+ 0
-- /raise_proc1/                    l+ 0
-- /no_raise_proc1/                 l+ 0

-- /fun1/                           l+ 0
-- /raise_user_defined_fun1/        l+ 0
-- /no_raise_user_defined_fun1/     l+ 0
-- /raise_fun1/                     l+ 0
-- /no_raise_fun1/                  l- s-

-- /proc2/                          l+ 0
-- /raise_user_defined_proc2/       l+ 0
-- /no_user_defined_raise_proc2/    l+ 0
-- /in_if_proc2/                    l+ 0
-- /no_predefined_raise_proc2/      l- s-
-- /raise_predefined_proc2/         l+ 0

-- /fun2/                           l+ 0
-- /raise_user_defined_fun2/        l+ 0
-- /no_user_defined_raise_fun2/     l+ 0
-- /no_predefined_raise_fun2/       l- s-
-- /raise_predefined_fun2/          l+ 0
-- /no_raise_fun2/                  l- s-

-- /proc3/                          l+ 0
-- /no_predefined_raise_proc3/      l+ 0
-- /raise_user_defined_proc3/       l+ 0
-- /no_raise_proc3/                 l- s-
-- /raise_predefined_proc3/         l+ 0

-- /fun3/                           l+ 0
-- /no_predefined_raise_fun3/       l+ 0
-- /raise_user_defined_fun3/        l+ 0
-- /no_raise_fun3/                  l- s-
-- /raise_predefined_fun3/          l+ 0

-- /proc4/                          l+ 0
-- /no_predefined_raise_proc4/      l+ 0
-- /no_raise_proc4/                 l- s-
-- /raise_user_defined_proc4/       l+ 0
-- /raise_predefined_proc4/         l+ 0

-- /fun4/                           l+ 0
-- /no_predefined_raise_fun4/       l+ 0
-- /raise_user_defined_fun4/        l+ 0
-- /no_raise_fun4/                  l- s-
-- /raise_user_defined_fun4/        l+ 0
-- /raise_predefined_fun4/          l+ 0
