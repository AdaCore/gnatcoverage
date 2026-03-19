with Support; use Support;

with Pkg;

procedure Test_Full is
begin
   Assert (Pkg.Eval_Decl_Expr (True));
end Test_Full;

--# pkg.adb
--
-- /st/   l+ ## 0
-- /decl/ l+ ## 0
