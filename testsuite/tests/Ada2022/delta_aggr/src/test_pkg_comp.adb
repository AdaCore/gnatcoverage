with Pkg; use Pkg;

with Support; use Support;

procedure Test_Pkg_Comp is
   Comp : Composite := Cached_Comp;
begin
   Assert (Eq (Copy_With_Abs_Update (Comp, First, False), 0, 0));
   Assert (Eq (Copy_With_Abs_Update (Comp, Last, False), 0, 0));
end Test_Pkg_Comp;

--# pkg.adb
--
-- /st/         l+ ## 0
-- /base_expr/  l! ## dT-
-- /comp_expr/  l+ ## 0
-- /value_expr/ l! ## dF-
