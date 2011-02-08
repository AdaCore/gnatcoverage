--  Test driver for exceptions. It calls all the subprograms from the
--  functional code, but does it in such a way that each subprogram raises
--  an exception. So the code that follows the raise statement in effect shall
--  be reported as uncovered..

with Pack;    use Pack;
with Support; use Support;
procedure Test_Exceptions_Full is
   Int : My_Int := 100;
begin
   Proc1 (Int);
   Assert (Int = 0);

   Int := 0;
   Proc1 (Int);
   Assert (Int = 1);

   Assert (Fun1 (100) = 0);
   Assert (Fun1 (0) = 1);

   Int := 100;
   Proc2 (Int);
   Assert (Int = 0);

   Int := 0;
   Proc2 (Int);
   Assert (Int = 1);

   Assert (Fun2 (100) = 0);
   Assert (Fun2 (1) = 2);

   Int := 100;
   Proc3 (Int);
   Assert (Int = 1);

   Int := -100;
   Proc3 (Int);
   Assert (Int = 2);

   Int := 100;
   Proc3 (Int);
   Assert (Int = 1);

   Int := 3;
   Proc3 (Int);
   Assert (Int = 6);

   Assert (Fun3 (-100) = 1);
   Assert (Fun3 (100) = 2);
   Assert (Fun3 (3) = 6);

end Test_Exceptions_Full;

--#  pack.adb
-- /proc1/                          l+ 0
-- /raise_proc1/                    l+ 0
-- /no_raise_proc1/                 l+ 0

-- /fun1/                           l+ 0
-- /raise_fun1/                     l+ 0
-- /no_raise_fun1/                  l+ 0

-- /proc2/                          l+ 0
-- /raise_proc2/                    l+ 0
-- /no_raise_proc2/                 l+ 0

-- /fun2/                           l+ 0
-- /raise_fun2/                     l+ 0
-- /no_raise_fun2/                  l+ 0

-- /proc3/                          l+ 0
-- /raise_my_exception_proc3/       l+ 0
-- /after_raise_proc3/              l+ 0
-- /raise_constraint_error_proc3/   l+ 0
-- /elsif_proc3/                    l+ 0
-- /in_elsif_proc3/                 l+ 0
-- /after_if_proc3/                 l+ 0
-- /constraint_error_handler_proc3/ l+ 0
-- /others_handler_proc3/           l+ 0

-- /fun3/                           l+ 0
-- /raise_constraint_error_fun3/    l+ 0
-- /after_raise_fun3/               l+ 0
-- /raise_my_exception_fun3/        l+ 0
-- /elsif_fun3/                     l+ 0
-- /in_elsif_fun3/                  l+ 0
-- /after_if_fun3/                  l+ 0
-- /my_exception_handler_fun3/      l+ 0
-- /others_handler_fun3/            l+ 0
