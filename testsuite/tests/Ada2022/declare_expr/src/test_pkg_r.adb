with Support;
with Pkg;

procedure Test_Pkg_R is
   Res : Boolean;
begin
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
end Test_Pkg_R;

--  These test expectations are not applicable for the "in_if_stmt" test, as in
--  this test the enclosing statement for the declare contains a decision and
--  thus require different expectations.

--# pkg.adb
--
-- /before/     l+ ## 0
-- /ret_stmt/   l+ ## 0
-- /if_stmt/    l! ## d-
-- /declare/    l+ ## 0
-- /at/         l! ## eT-
-- /oe/         l! ## eT-
-- /stop_stmt/  l+ ## 0
-- /raise_expr/ l! ## dF-
-- /rename/     l! ## s-
-- /begin/      l+ ## 0
-- /eval/       l! ## e-
-- /after/      l- ## s-
