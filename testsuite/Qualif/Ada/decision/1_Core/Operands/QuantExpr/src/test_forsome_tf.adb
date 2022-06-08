with Forsome, Expr, Support; use Expr, Support;

-- Outer "for some" True and False, from predicate True then always False

procedure Test_Forsome_TF is
begin
   Assert (Forsome ((8, 2), GT_3'access));
   Assert (not Forsome ((1, 2), GT_3'access));
end;

--# forsome.adb
--  /eval/ l+ ## 0
--  /true/ l+ ## 0
--  /false/ l+ ## 0
