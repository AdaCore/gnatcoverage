with Support;
with Pkg;

procedure Test_Pkg_0 is
begin
   null;
end Test_Pkg_0;

--# pkg.adb
--
-- /before/     l- ## s-
-- /ret_stmt/   l- ## s-
-- /if_stmt/    l- ## s-
-- /declare/    l- ## 0
-- /at/         l- ## s-
-- /oe/         l- ## s-
-- /stop_stmt/  l- ## s-
-- /raise_expr/ l- ## 0
-- /rename/     l- ## s-
-- /begin/      l- ## 0
-- /eval/       l- ## 0
-- /after/      l- ## s-
