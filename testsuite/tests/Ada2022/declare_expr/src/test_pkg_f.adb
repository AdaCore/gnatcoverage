with Support;
with Pkg;

procedure Test_Pkg_F is
   Res : Boolean;
begin
   Res := Pkg.Compute
     (A    => True,
      B    => False,
      C    => False,
      D    => False,
      Stop => False);
   Res := Pkg.Compute
     (A    => True,
      B    => True,
      C    => True,
      D    => False,
      Stop => False);
   Res := Pkg.Compute
     (A    => False,
      B    => True,
      C    => False,
      D    => True,
      Stop => False);
   Res := Pkg.Compute
     (A    => False,
      B    => False,
      C    => False,
      D    => False,
      Stop => True);
   Support.Assert (False);
exception
   when Pkg.Dummy_Error =>
      null;
end Test_Pkg_F;

--# pkg.adb
--
-- /before/     l+ ## 0
-- /ret_stmt/   l+ ## 0
-- /if_stmt/    l+ ## 0
-- /declare/    l+ ## 0
-- /at/         l+ ## 0
-- /oe/         l+ ## 0
-- /stop_stmt/  l+ ## 0
-- /raise_expr/ l+ ## 0
-- /rename/     l+ ## 0
-- /begin/      l+ ## 0
-- /eval/       l+ ## 0
-- /after/      l+ ## 0
