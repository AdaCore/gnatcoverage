with Forall, Expr, Support; use Expr, Support;

-- Outer "for all" decision False only, Predicate False on last item

procedure Test_Forall_F_TF is
begin
   Assert (not Forall ((5, 1), GT_3'access));
end;

--# forall.adb
--  /eval/ l! ## dT-:"for all"
--  /true/ l- ## s-
--  /false/ l+ ## 0
