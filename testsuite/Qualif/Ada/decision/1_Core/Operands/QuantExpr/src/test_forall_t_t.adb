with Forall, Expr, Support; use Expr, Support;

-- Outer "for all" decision True only, Predicate True only

procedure Test_Forall_T_T is
begin
   Assert (Forall ((4, 5, 6), GT_3'access));
end;

--# forall.adb
--  /eval/ l! ## dF-:"for all", dF-:"P ("
--  /true/ l+ ## 0
--  /false/ l- ## s-

-- Decision coverage with bin traces is imprecise on simple expressions

-- %opts: --trace-mode=bin
-- =/eval/ l! ## dF-:"for all", d!:"P ("
