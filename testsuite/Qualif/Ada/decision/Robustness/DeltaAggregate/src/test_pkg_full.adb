with Pkg; use Pkg;

with Support; use Support;

procedure Test_Pkg_Full is
   Comp : Composite := Cached_Comp;
begin
   Assert (Eq (Copy_With_Abs_Update (Comp, First, True), 0, 0));
   Assert (Eq (Copy_With_Abs_Update (Comp, Last, True), 0, 0));
   Assert (Eq (Copy_With_Abs_Update (Comp, First, False), 0, 0));
   Assert (Eq (Copy_With_Abs_Update (Comp, Last, False), 0, 0));
   Comp := Make (-1, 0);
   Assert (Eq (Copy_With_Abs_Update (Comp, First, False), 1, 0));
   Comp := Make (0, -1);
   Assert (Eq (Copy_With_Abs_Update (Comp, Last, False), 0, 1));

end Test_Pkg_Full;

--# pkg.adb
--
-- /st/         l+ ## 0
-- /base_expr/  l+ ## 0
-- /comp_expr/  l+ ## 0
-- /value_expr/ l+ ## 0
