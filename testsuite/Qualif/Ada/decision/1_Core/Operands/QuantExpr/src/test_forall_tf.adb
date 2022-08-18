with Forall, Expr, Support; use Expr, Support;

-- Outer "for all" True and False, from predicate always True, then True/False

procedure Test_Forall_TF is
begin
   Assert (Forall ((9, 5, 6), GT_3'access));
   Assert (not Forall ((1, 5, 6), GT_3'access));
end;

--# forall.adb
--  /eval/ l+ ## 0
--  /true/ l+ ## 0
--  /false/ l+ ## 0
