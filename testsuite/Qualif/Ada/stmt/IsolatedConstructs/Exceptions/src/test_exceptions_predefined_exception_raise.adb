--  Test driver for exceptions. It calls all the subprograms from the
--  functional code, but does it in such a way that if a subprogram can raise
--  a predefined exception Constraint_Error, it raises this exception. So no
--  raise statement for user-defined exception My_Exception and no code in
--  exception handlers that may handle the My_Exception exceptions is expected
--  to be reported as covered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Exceptions_Predefined_Exception_Raise is
   Int : My_Int := 0;
begin
   Proc1 (Int);
   Assert (Int = 1);

   Assert (Fun1 (0) = 1);

   Int := 100;
   Proc2 (Int);
   Assert (Int = 0);

   Assert (Fun2 (100) = 0);

   Int := 100;
   Proc3 (Int);
   Assert (Int = 1);

   Assert (Fun3 (-100) = 1);

end Test_Exceptions_Predefined_Exception_Raise;

--#  pack.adb
-- /proc1/                          l+ 0
-- /raise_proc1/                    l- s-
-- /no_raise_proc1/                 l+ 0

-- /fun1/                           l+ 0
-- /raise_fun1/                     l- s-
-- /no_raise_fun1/                  l+ 0

-- /proc2/                          l+ 0
-- /raise_proc2/                    l+ 0
-- /no_raise_proc2/                 l- s-

-- /fun2/                           l+ 0
-- /raise_fun2/                     l+ 0
-- /no_raise_fun2/                  l- s-

-- /proc3/                          l+ 0
-- /raise_my_exception_proc3/       l- s-
-- /after_raise_proc3/              l+ 0
-- /raise_constraint_error_proc3/   l+ 0
-- /elsif_proc3/                    l- s-
-- /in_elsif_proc3/                 l- s-
-- /after_if_proc3/                 l- s-
-- /constraint_error_handler_proc3/ l+ 0
-- /others_handler_proc3/           l- s-

-- /fun3/                           l+ 0
-- /raise_constraint_error_fun3/    l+ 0
-- /after_raise_fun3/               l- s-
-- /raise_my_exception_fun3/        l- s-
-- /elsif_fun3/                     l- s-
-- /in_elsif_fun3/                  l- s-
-- /after_if_fun3/                  l- s-
-- /my_exception_handler_fun3/      l- s-
-- /others_handler_fun3/            l+ 0
