--  Test driver for exception handlers. It only "with's" the functional code,
--  but does not call anything from it, so everything is expected to be
--  reported as uncovered.

with Pack;    use Pack;
with Support; use Support;
procedure Test_Handlers_No is
begin
   Assert (True);
end Test_Handlers_No;

--#  pack.adb
-- /proc1/                          l- ## s-
-- /raise_user_defined_proc1/       l- ## s-
-- /raise_proc1/                    l- ## s-
-- /no_raise_proc1/                 l- ## s-

-- /fun1/                           l- ## s-
-- /raise_user_defined_fun1/        l- ## s-
-- /no_raise_user_defined_fun1/     l- ## s-
-- /raise_fun1/                     l- ## s-
-- /no_raise_fun1/                  l- ## s-

-- /proc2/                          l- ## s-
-- /raise_user_defined_proc2/       l- ## s-
-- /no_user_defined_raise_proc2/    l- ## s-
-- /in_if_proc2/                    l- ## s-
-- /no_predefined_raise_proc2/      l- ## s-
-- /raise_predefined_proc2/         l- ## s-

-- /fun2/                           l- ## s-
-- /raise_user_defined_fun2/        l- ## s-
-- /no_user_defined_raise_fun2/     l- ## s-
-- /no_predefined_raise_fun2/       l- ## s-
-- /raise_predefined_fun2/          l- ## s-
-- /no_raise_fun2/                  l- ## s-

-- /proc3/                          l- ## s-
-- /no_predefined_raise_proc3/      l- ## s-
-- /raise_user_defined_proc3/       l- ## s-
-- /no_raise_proc3/                 l- ## s-
-- /raise_predefined_proc3/         l- ## s-

-- /fun3/                           l- ## s-
-- /no_predefined_raise_fun3/       l- ## s-
-- /raise_user_defined_fun3/        l- ## s-
-- /no_raise_fun3/                  l- ## s-
-- /raise_predefined_fun3/          l- ## s-

-- /proc4/                          l- ## s-
-- /no_predefined_raise_proc4/      l- ## s-
-- /no_raise_proc4/                 l- ## s-
-- /raise_user_defined_proc4/       l- ## s-
-- /raise_predefined_proc4/         l- ## s-

-- /fun4/                           l- ## s-
-- /no_predefined_raise_fun4/       l- ## s-
-- /raise_user_defined_fun4/        l- ## s-
-- /no_raise_fun4/                  l- ## s-
-- /raise_predefined_fun4/          l- ## s-
