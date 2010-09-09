--  Test driver for exceptions. It calls all the subprograms from the
--  functional code, but does it in such a way that no exception is raised, so
--  no raise statement and no code in exception handlers is expected to be
--  reported as covered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Exceptions_No_Exception_Raise is
   Int : My_Int := 0;
begin
   Proc1 (Int);
   Assert (Int = 1);

   Assert (Fun1 (0) = 1);

   Proc2 (Int);
   Assert (Int = 2);

   Assert (Fun2 (0) = 1);

   Proc3 (Int);
   Assert (Int = 5);

   Assert (Fun3 (0) = 0);

end Test_Exceptions_No_Exception_Raise;

--#  pack.adb
-- /proc1/                          l+ 0
-- /raise_proc1/                    l- s-
-- /no_raise_proc1/                 l+ 0

-- /fun1/                           l+ 0
-- /raise_fun1/                     l- s-
-- /no_raise_fun1/                  l+ 0

-- /proc2/                          l+ 0
-- /raise_proc2/                    l- s-
-- /no_raise_proc2/                 l+ 0

-- /fun2/                           l+ 0
-- /raise_fun2/                     l- s-
-- /no_raise_fun2/                  l+ 0

-- /proc3/                          l+ 0
-- /raise_my_exception_proc3/       l- s-
-- /after_raise_proc3/              l+ 0
-- /raise_constraint_error_proc3/   l- s-
-- /elsif_proc3/                    l+ 0
-- /in_elsif_proc3/                 l+ 0
-- /after_if_proc3/                 l+ 0
-- /constraint_error_handler_proc3/ l- s-
-- /others_handler_proc3/           l- s-

-- /fun3/                           l+ 0
-- /raise_constraint_error_fun3/    l- s-
-- /after_raise_fun3/               l+ 0
-- /raise_my_exception_fun3/        l- s-
-- /elsif_fun3/                     l+ 0
-- /in_elsif_fun3/                  l- s-
-- /after_if_fun3/                  l+ 0
-- /my_exception_handler_fun3/      l- s-
-- /others_handler_fun3/            l- s-
