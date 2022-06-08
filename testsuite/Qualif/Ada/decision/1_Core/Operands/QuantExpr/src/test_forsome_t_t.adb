with Forsome, Expr, Support; use Expr, Support;

-- Outer "for some" decision True only, Predicate True only

procedure Test_Forsome_T_T is
begin
   Assert (Forsome ((4, 5, 6), GT_3'access));
end;

--# forsome.adb
--  /eval/ l! ## dF-:"for some", dF-:"P ("
--  /true/ l+ ## 0
--  /false/ l- ## s-

-- Decision coverage with bin traces is imprecise on simple expressions

-- %opts: --trace-mode=bin
-- =/eval/ l! ## dF-:"for some", d!:"P ("
