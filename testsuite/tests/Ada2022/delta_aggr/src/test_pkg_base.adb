with Pkg; use Pkg;

with Support; use Support;

procedure Test_Pkg_Base is
   Comp : Composite := Cached_Comp;
begin
   Assert (Eq (Copy_With_Abs_Update (Comp, First, True), 0, 0));
   Assert (Eq (Copy_With_Abs_Update (Comp, First, False), 0, 0));
end Test_Pkg_Base;

--# pkg.adb
--
-- /st/               l+ ## 0
-- /base_expr_last/   l! ## d-
-- /base_expr_first/  l+ ## 0
-- /comp_expr/        l! ## dF-
-- /value_expr_last/  l! ## d-
-- /value_expr_first/ l! ## dF-
