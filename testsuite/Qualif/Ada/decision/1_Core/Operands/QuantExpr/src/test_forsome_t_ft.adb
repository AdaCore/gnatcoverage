with Forsome, Expr, Support; use Expr, Support;

-- Outer "for some" True only, from predicate True on last item

procedure Test_Forsome_T_FT is
begin
   Assert (Forsome ((2, 1, -1, 8), GT_3'access));
end;

--# forsome.adb
--  /eval/ l! ## dF-:"for some"
--  /true/ l+ ## 0
--  /false/ l- ## s-
