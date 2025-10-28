with Support; use Support;

with Pkg;

procedure Test_0 is
begin
   Assert (not Pkg.Eval_Decl_Expr (False));
end Test_0;

--# pkg.adb
--
-- /st/   l+ ## 0
-- /decl/ l! ## s-
