--  Test driver for exception raise. It calls all the subprograms from the
--  functional code, but does it in such a way that if a subprogram can raise
--  a user-defined exception My_Exception, it raises this exception. So all the
--  raise statement for predefined exception and all the code in exception
--  handlers that may handle predefined exceptions are expected to be reported
--  as uncovered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Exceptions_User_Exception_Raise is
   Int : My_Int := 100;
begin
   Proc1 (Int);
   Assert (Int = 0);

   Assert (Fun1 (100) = 0);

   Int := 1;
   Proc2 (Int);
   Assert (Int = 2);

   Assert (Fun2 (0) = 1);

   Proc3 (Int);
   Assert (Int = 1);

   Assert (Fun3 (0) = 1);

   Int := -100;
   Proc4 (Int);
   Assert (Int = 2);

   Assert (Fun4 (0) = 0);

end Test_Exceptions_User_Exception_Raise;

--#  pack.adb
-- /proc1/                          l+ 0
-- /raise_proc1/                    l+ 0
-- /no_raise_proc1/                 l- s-

-- /fun1/                           l+ 0
-- /raise_fun1/                     l+ 0
-- /no_raise_fun1/                  l- s-

-- /proc2/                          l+ 0
-- /raise_proc2/                    l- s-
-- /no_raise_proc2/                 l+ 0

-- /fun2/                           l+ 0
-- /raise_fun2/                     l- s-
-- /no_raise_fun2/                  l+ 0

-- /proc3/                          l+ 0
-- /no_raise_proc3/                 l+ 0
-- /raise_proc3/                    l- s-

-- /fun3/                           l+ 0
-- /raise_fun3/                     l- s-
-- /no_raise_fun3/                  l+ 0

-- /proc4/                          l+ 0
-- /raise_my_exception_proc4/       l+ 0
-- /after_raise_proc4/              l- s-
-- /raise_constraint_error_proc4/   l- s-
-- /elsif_proc4/                    l- s-
-- /in_elsif_proc4/                 l- s-
-- /after_if_proc4/                 l- s-
-- /constraint_error_handler_proc4/ l- s-
-- /others_handler_proc4/           l+ 0

-- /fun4/                           l+ 0
-- /raise_constraint_error_fun4/    l- s-
-- /after_raise_fun4/               l+ 0
-- /raise_my_exception_fun4/        l- s-
-- /elsif_fun4/                     l+ 0
-- /in_elsif_fun4/                  l- s-
-- /after_if_fun4/                  l+ 0
-- /my_exception_handler_fun4/      l- s-
-- /others_handler_fun4/            l- s-
