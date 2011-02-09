--  Test driver for exception raise. It only "with's" the functional code, but
--  does not call anything from it, so everything is expected to be reported as
--  uncovered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Exceptions_No is
begin
   Assert (True);
end Test_Exceptions_No;

--#  pack.adb
-- /proc1/                          l- s-
-- /raise_proc1/                    l- s-
-- /no_raise_proc1/                 l- s-

-- /fun1/                           l- s-
-- /raise_fun1/                     l- s-
-- /no_raise_fun1/                  l- s-

-- /proc2/                          l- s-
-- /raise_proc2/                    l- s-
-- /no_raise_proc2/                 l- s-

-- /fun2/                           l- s-
-- /raise_fun2/                     l- s-
-- /no_raise_fun2/                  l- s-

-- /proc3/                          l- s-
-- /no_raise_proc3/                 l- s-
-- /raise_proc3/                    l- s-

-- /fun3/                           l- s-
-- /raise_fun3/                     l- s-
-- /no_raise_fun3/                  l- s-

-- /proc4/                          l- s-
-- /raise_my_exception_proc4/       l- s-
-- /after_raise_proc4/              l- s-
-- /raise_constraint_error_proc4/   l- s-
-- /elsif_proc4/                    l- s-
-- /in_elsif_proc4/                 l- s-
-- /after_if_proc4/                 l- s-
-- /constraint_error_handler_proc4/ l- s-
-- /others_handler_proc4/           l- s-

-- /fun4/                           l- s-
-- /raise_constraint_error_fun4/    l- s-
-- /after_raise_fun4/               l- s-
-- /raise_my_exception_fun4/        l- s-
-- /elsif_fun4/                     l- s-
-- /in_elsif_fun4/                  l- s-
-- /after_if_fun4/                  l- s-
-- /my_exception_handler_fun4/      l- s-
-- /others_handler_fun4/            l- s-
