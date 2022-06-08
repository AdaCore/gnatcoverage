with Forall, Expr, Support; use Expr, Support;

-- Outer "for all" decision False only, Predicate False on first item

procedure Test_Forall_F_FX is
   X : Integer := 0;
begin
   Assert (not Forall ((1, X), GT_3'access));
end;

--# forall.adb
--  /eval/ l! ## dT-:"for all", dT-:"P ("
--  /true/ l- ## s-
--  /false/ l+ ## 0

-- Decision coverage with bin traces is imprecise on simple expressions

-- %opts: --trace-mode=bin
-- =/eval/ l! ## dT-:"for all", d!:"P ("
